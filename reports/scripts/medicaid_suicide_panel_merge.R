library(tidyverse)
library(janitor)

state_medicaid_panel <- 
  readRDS("data/export/state_medicaid_elig_panel.rds")
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
    state_medicaid_panel,
    select(cdc_suicide,-population)
  ) %>%
  mutate(suicide_non_firearm = suicide_total - suicide_firearms , 
         crude_rate_non_firearms = round((suicide_non_firearm/population)*100000,1),
         med_support_enrollment_ratio = (medical_income_assist_enrolled/population) )

saveRDS(
  state_medicaid_suicide_panel , 
  "data/export/state_medicaid-suicide_panel.rds"
)
