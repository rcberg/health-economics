library(tidyverse)

library(rvest)
states_id <- 
  read_html("https://www.mcc.co.mercer.pa.us/dps/state_fips_code_listing.htm") %>%
  html_node("body > div > center > table") %>%
  html_table()

eligibility_files <- list.files(path="data/raw", pattern = "medicaid_elig_")

topcode_func <- 
  function(i){
    z <- ifelse( i == "No Upper Limit" , "9999997" , i)
  }

read_elig_csv <- 
  function(elig_file){
    elig_file_tmp <- 
      read_csv( file = paste0("data/raw/",elig_file)
      ) %>%
      mutate(across(everything() , 
                    as.character) )
    
    return(elig_file_tmp)
  }

medicaid_eligibility_df <- 
  map_dfr(eligibility_files, 
          read_elig_csv )

library(lubridate)

medicaid_elig_long <- 
  medicaid_eligibility_df %>%
  pivot_longer(cols = -c("Location","age_low","age_high","nda","parent") , 
               names_to = "date" ,
               values_to = "threshhold" )%>%
  separate(
    col = date , 
    into = c("month","year") , 
    sep = "_"
  ) %>%
  mutate(
    across(
      .cols = c(age_low,age_high,nda,parent,year),
      .fns = as.numeric
    ) ,  
    year_adj = ifelse( month %in% c("october","december") , 
                       year + 1 , 
                       year ) ,
    threshhold = as.numeric(ifelse(threshhold == "No Upper Limit" , 5.01 , threshhold)) , 
    date = ymd(paste(year,month,"01",sep="-")) 
  )

fpl_data <- 
  read_csv( "data/raw/fpl_historic.csv") %>%
  separate( col = "year" , into = c("year",NA), sep=" ") %>%
  mutate( year = as.numeric(year))

fpl_data_sample <- 
  filter(fpl_data, year>2010) %>% 
  select(-c(fpl_7a, cpi_83)) %>%
  cbind( additional_person = c(3820,3960,4020,4060,4160,4160,4180,4320,4420,4480))

medicaid_elig_sample <- 
  filter(medicaid_elig_long, year>2010)  %>%
  left_join( tibble(Location = c(state.name) , 
                    state_abb = c(state.abb) ) ) %>%
  mutate( state_abb = ifelse( Location == "United States" , "US" , 
                              ifelse( Location == "District of Columbia" , "DC" , 
                                      state_abb)
                              ) ,
          threshhold = threshhold*100
          ) %>%
  left_join( tibble( state_abb = c(states_id$X1[2:length(states_id$X1)],states_id$X4[2:length(states_id$X4)]) , 
                     fips = as.numeric(
                       c(states_id$X2[2:length(states_id$X2)],states_id$X5[2:length(states_id$X5)])
                       )
                     )
  ) %>%
  rename(state = Location , 
         medicaid_age_lower = age_low , 
         medicaid_age_upper = age_high ) %>% arrange( fips, date )

medicaid_elig_sample$parent <- factor( x = medicaid_elig_sample$parent , 
                                       levels = c(0,1,2) , 
                                       labels = c("non-parent","parent","pregnant"))

saveRDS( medicaid_elig_sample , "data/export/state_medicaid_eligibility_2011_2020.rds")
saveRDS( fpl_data_sample , "data/export/federal_poverty_levels_2011_2020.rds")

medicaid_elig_infant <- 
  medicaid_elig_sample %>%
  filter( medicaid_age_upper == 1 ) %>%
  select( year_adj , 
          threshhold ,
          fips ) %>%
  rename( "year" = "year_adj",
          "thresh_infant" = "threshhold" , 
          "statefip" = "fips")

medicaid_elig_youngkids <- 
  medicaid_elig_sample %>%
  filter( medicaid_age_upper == 6 ) %>%
  select( year_adj , 
          threshhold ,
          fips ) %>%
  rename( "year" = "year_adj",
          "thresh_youngkids" = "threshhold" , 
          "statefip" = "fips")

medicaid_elig_bigkids <- 
  medicaid_elig_sample %>%
  filter( medicaid_age_lower == 5 & medicaid_age_upper == 19 ) %>%
  select( year_adj , 
          threshhold ,
          fips ) %>%
  rename( "year" = "year_adj",
          "thresh_bigkids" = "threshhold" , 
          "statefip" = "fips")

medicaid_elig_nda <- 
  medicaid_elig_sample %>%
  filter( nda == 1  ) %>%
  select( year_adj , 
          threshhold ,
          fips ) %>%
  rename( "year" = "year_adj",
          "thresh_nda" = "threshhold" , 
          "statefip" = "fips")

medicaid_elig_parents <- 
  medicaid_elig_sample %>%
  filter( parent == "parent" ) %>%
  select( year_adj , 
          threshhold ,
          fips ) %>%
  rename( "year" = "year_adj",
          "thresh_parents" = "threshhold" , 
          "statefip" = "fips")

medicaid_chip_elig <- 
  medicaid_elig_sample %>%
  filter( parent == "non-parent" & medicaid_age_lower == -1 & medicaid_age_upper == 19 ) %>%
  select( year_adj , 
          threshhold ,
          fips ) %>%
  rename( "year" = "year_adj",
          "thresh_chip_med" = "threshhold" , 
          "statefip" = "fips")

medicaid_elig_wide <- 
  medicaid_elig_nda %>%
  left_join( medicaid_elig_infant ) %>%
  left_join( medicaid_elig_youngkids ) %>%
  left_join( medicaid_elig_bigkids ) %>%
  left_join( medicaid_elig_parents ) %>%
  left_join( medicaid_chip_elig )

saveRDS( medicaid_elig_wide , "data/export/state_medicaid_eligibility_2011_2020_wide.rds")
