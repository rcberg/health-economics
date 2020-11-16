library(ipumsr)
library(data.table)
library(janitor)

ddi_15 <- 
  read_ipums_ddi("data/raw/usa_00015.xml") #ddi is a file that will let us access additional info about our data
usa_08_10 <- 
  data.table( 
    clean_names( 
      read_ipums_micro( ddi_15 )
    )
  )
usa_08_10 <- usa_08_10[,indnaics:=NULL][,(names(usa_08_10)):=lapply(.SD, as.numeric)][year<2011,]

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

medicaid_elig_sample <- 
  filter(medicaid_elig_long, year>2007&year<2011)  %>%
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
         medicaid_age_upper = age_high ) %>% 
  arrange( fips, date ) %>%
  mutate( threshhold = replace_na(threshhold,0))

medicaid_elig_sample$parent <- factor( x = medicaid_elig_sample$parent , 
                                       levels = c(0,1,2) , 
                                       labels = c("non-parent","parent","pregnant"))

saveRDS( medicaid_elig_sample , "data/export/state_medicaid_eligibility_2008_2010.rds")
saveRDS( fpl_data_sample , "data/export/federal_poverty_levels_2008_2010.rds")

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

saveRDS( medicaid_elig_wide , "data/export/state_medicaid_eligibility_2008_2010_wide.rds")

state_pop <- 
  usa_08_10[,
         .(
           population = sum(perwt)
         ),
         by=.(statefip,year)]
saveRDS(state_pop, "data/export/usa_state_pop_2008_2010.rds")

state_personalinc <- 
  usa_08_10[ inctot > 0 & inctot < 9999999,
          .(
            personal_income = weighted.mean(x=inctot,w=perwt)
          ),
          by=.(statefip,year)]
saveRDS(state_personalinc, "data/export/usa_state_income_2008_2010.rds")

state_medicaid_enrolled <- 
  usa_08_10[ hinscaid == 2,
          .(
            medicaid_enrolled = sum(perwt)
          ),
          by=.(statefip,year)]

saveRDS(state_medicaid_enrolled, "data/export/usa_state_medicaid_rolls_2008_2010.rds")

usa_08_10 <- 
  usa_08_10[ poverty > 0 & ftotinc < 9999999,] 

households_dt <- usa_08_10[, `:=`(
  n_parents = sum( ifelse(nchild > 0,
                          1, 0)
  ),
  n_infants = sum( ifelse(age < 1,
                          1 , 0)
  ) , 
  n_youngkids = sum( ifelse(age > 0 & age < 6,
                            1 , 0)
  ) , 
  n_bigkids = sum( ifelse(age > 5 & age < 19,
                          1 , 0)
  ) , 
  n_elderly = sum( ifelse(age > 64, 
                          1 , 0)
  ) ,
  n_nda = sum( ifelse( age > 18 & age < 65 & nchild == 0 ,
                       1, 0))
)
, by=.(cbserial,sample)
][
  pernum == 1, 
  .(year,
    sample,
    cbserial,
    hhwt, 
    statefip, 
    ftotinc, 
    famsize,
    n_parents, 
    n_infants,
    n_youngkids, 
    n_bigkids, 
    n_elderly,
    n_nda, 
    poverty)
]

rm(usa_08_10)

household_elig <- 
  merge.data.table(households_dt, medicaid_elig_wide)

household_elig <- 
  household_elig[
    ,
    `:=`(
      elig_nda = ifelse( poverty <= thresh_nda , 
                         1 , 
                         0 ) , 
      elig_infant = ifelse( poverty <= thresh_infant , 
                            1 , 
                            0 ) , 
      elig_youngkids = ifelse(poverty <= thresh_youngkids , 
                              1 , 
                              0 ) , 
      elig_bigkids = ifelse( poverty <= thresh_bigkids , 
                             1 , 
                             0 ) ,
      elig_parents = ifelse( poverty <= thresh_parents , 
                             1 , 
                             0 ) ,
      elig_chip_med = ifelse( poverty <= thresh_chip_med , 
                              1 , 
                              0 )
    )
  ][,
    `:=`(
      n_elig = 
        n_nda*elig_nda +
        n_infants*elig_infant +
        n_youngkids*elig_youngkids +
        n_bigkids*elig_bigkids +
        n_parents*elig_parents ,
      n_elig_chip = 
        n_nda*elig_nda +
        n_infants*elig_infant +
        n_youngkids*elig_youngkids + 
        n_bigkids*elig_bigkids +
        n_parents*elig_parents + 
        (n_infants+n_youngkids+n_bigkids)*elig_chip_med
    )
  ]

household_elig_lite <- 
  household_elig[,
                 c("year","statefip","sample","cbserial","hhwt","ftotinc","famsize","poverty","n_elig","n_elig_chip")]

saveRDS(household_elig_lite , "data/export/medicaid_eligibility_panel_2008_2010.rds")
saveRDS(household_elig , "data/export/medicaid_eligibility_panel_detail_2008_2010.rds")

state_elig_panel <- 
  household_elig_lite[,
               .(mean_family_income = weighted.mean(ftotinc, w=hhwt) , 
                 medicaid_eligible = sum(hhwt*n_elig),
                 medicaid_chip_eligible = sum(hhwt*n_elig_chip),
                 households = sum(hhwt)
               ),
               by=.(statefip,year)]

state_income_pop_data <- 
  merge.data.table(state_personalinc,
                   state_pop)

state_medicaid_rolls_data <- 
  merge.data.table(state_income_pop_data,
                   state_medicaid_enrolled)

state_panel_data <- 
  merge.data.table(state_elig_panel,state_medicaid_rolls_data, by=c("statefip","year"))

state_panel_data <- 
  state_panel_data[,`:=`(medicaid_eligibility_ratio = medicaid_eligible/population,
                         med_chip_elig_ratio = medicaid_chip_eligible/population ,
                         medical_income_assist_enrolled = medicaid_enrolled)
  ][
    , medicaid_enrolled:=NULL]

saveRDS(state_panel_data, "data/export/state_medicaid_elig_panel_2008_2010.rds" )

cdc_suicide_raw <- 
  read_csv("data/raw/cdc_wonder_suicide_raw.csv") %>%
  clean_names() %>%
  rename( "statefip" = "state_code") %>%
  select(-year_code)

cdc_suicide_total <-
  cdc_suicide_raw %>%
  filter( is.na(notes) != T &
            is.na(year) != T) %>%
  select(-c(cause_of_death,
            cause_of_death_code,
            notes))

cdc_suicide_spec <- 
  cdc_suicide_raw %>%
  filter( is.na(notes) == T ) %>%
  select(-notes)

cdc_suicide_firearms <- 
  cdc_suicide_spec %>%
  filter( cause_of_death_code %in%
            c("X72","X73","X74") 
  ) %>%
  group_by( state,
            statefip,
            year ) %>%
  summarise( 
    deaths = sum(deaths) , 
    population = mean(population) , 
    adj_ci_low = sum(age_adjusted_rate_lower_95_percent_confidence_interval),
    adj_ci_hi = sum(age_adjusted_rate_upper_95_percent_confidence_interval)
  ) %>%
  mutate( 
    crude_rate = round((deaths/population)*100000,1) , 
    age_adjusted_rate = round((1/2)*(adj_ci_low+adj_ci_hi),1) 
  )

cdc_suicide <- 
  merge(
    select( cdc_suicide_total, 
            c(state,
              statefip,
              year,
              deaths,
              population,
              crude_rate)
    ) ,
    select( cdc_suicide_firearms, 
            c(state,
              statefip,
              year,
              deaths,
              population,
              crude_rate)
    ),
    by=
      c("state","statefip","year","population")
  ) %>%
  rename( "suicide_total" = "deaths.x" ,
          "crude_rate_total" = "crude_rate.x",
          "suicide_firearms" = "deaths.y" ,
          "crude_rate_firearms" = "crude_rate.y"
  ) %>%
  mutate( across(-state, as.numeric))

state_medicaid_suicide_panel <- 
  left_join(
    state_panel_data,
    select(cdc_suicide,-population)
  ) %>%
  mutate(suicide_non_firearm = suicide_total - suicide_firearms , 
         crude_rate_non_firearms = round((suicide_non_firearm/population)*100000,1),
         med_support_enrollment_ratio = (medical_income_assist_enrolled/population) )

saveRDS(
  state_medicaid_suicide_panel , 
  "data/export/state_medicaid-suicide_panel_2008_2010.rds"
)
state_medicaid_suicide_panel_late <- 
  readRDS("data/export/state_medicaid-suicide_panel.rds")

state_medicaid_suicide_panel <- 
  rbind(state_medicaid_suicide_panel,
        state_medicaid_suicide_panel_late) %>%
  arrange(statefip,year)

saveRDS(state_medicaid_suicide_panel, "data/export/state_medicaid-suicide_panel_2008_2018.rds" )
