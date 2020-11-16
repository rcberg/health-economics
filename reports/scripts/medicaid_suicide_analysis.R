library(tidyverse)
library(janitor)

state_medicaid_suicide_panel <- 
  readRDS("data/export/state_medicaid-suicide_panel_2008_2018.rds") %>%
  filter( statefip != 11 )

library(ggplot2)
ggplot( data = state_medicaid_suicide_panel[state_medicaid_suicide_panel$statefip!=11,] , 
        aes( x = year , 
             y = crude_rate_total , 
             color = state) ) + 
  geom_line( )

ggplot( data = state_medicaid_suicide_panel[state_medicaid_suicide_panel$statefip!=11,] , 
        aes( x = year , 
             y = med_chip_elig_ratio , 
             color = state) ) + 
  geom_line( )

ggplot( data = state_medicaid_suicide_panel[state_medicaid_suicide_panel$statefip!=11,] , 
        aes( x = year , 
             y = med_support_enrollment_ratio , 
             color = state) ) + 
  geom_line( )

fwl_function <- 
  function(data , 
           regressor_list , 
           x , 
           y ){
    require(broom)
    require(dplyr)
    tmp_orig_df <- 
      data
    fwl_formula_ls_x <- 
      as.formula(
        paste0(x," ~ ",paste(regressor_list,collapse = " + "))
      )
    fwl_formula_ls_y <- 
      as.formula(
        paste0(y," ~ ",paste(regressor_list,collapse = " + "))
      )
    
    fwl_model_ls_x <- 
      lm( data = data , 
          formula = fwl_formula_ls_x)
    fwl_model_ls_y <- 
      lm( data = data , 
          formula = fwl_formula_ls_y)
    
    x_resid <- 
      augment(fwl_model_ls_x) %>% 
      select(.resid) %>% 
      rename( "x_resid" = ".resid")
    y_resid <- 
      augment(fwl_model_ls_y) %>% 
      select(.resid) %>% 
      rename( "y_resid" = ".resid")
    
    fwl_vars_df <- 
      data.frame( fwl_x = x_resid , 
                  fwl_y = y_resid )
    
    return(fwl_vars_df)
    
  }

med_enrollment_suicide_fwl_df <- 
  fwl_function(data = state_medicaid_suicide_panel,
               regressor_list = c("as.factor(state)*year"),
               x = "med_support_enrollment_ratio",
               y = "crude_rate_total")

med_enrollment_eligibility_fwl_df <- 
  fwl_function(data = state_medicaid_suicide_panel,
               regressor_list = c("as.factor(state)*year"),
               x = "med_chip_elig_ratio",
               y = "med_support_enrollment_ratio")

ggplot( data = med_enrollment_eligibility_fwl_df , 
        aes(x = x_resid , 
            y = y_resid) ) + 
  geom_point() + 
  geom_smooth(method='lm')

ggplot( data = state_medicaid_suicide_panel , 
        aes(x = medicaid_chip_eligible , 
            y = medical_income_assist_enrolled) ) + 
  geom_point() + 
  geom_smooth(method='lm')

ggplot( data = med_enrollment_suicide_fwl_df , 
        aes(x = x_resid , 
            y = y_resid) ) + 
  geom_point() + 
  geom_smooth(method='lm')

#instrumental variables regression 
library(estimatr)
med_enrollment_suicide_iv <- 
  iv_robust( data = state_medicaid_suicide_panel , 
             formula = 
               crude_rate_total ~ 
               med_support_enrollment_ratio + as.factor(state)*year | 
               med_chip_elig_ratio + as.factor(state)*year ,
             clusters = state )

# how good is the ACS survey at approximating enrollment?
medicaid_elig_actual <- 
  read_csv( "data/raw/State_Medicaid_and_CHIP_Applications__Eligibility_Determinations__and_Enrollment_Data.csv" ) %>%
  clean_names()  %>%
  mutate( date = lubridate::mdy(report_date) ,
          year = lubridate::year(date) ) %>%
  select( state_name , 
          date ,
          year ,
          total_medicaid_and_chip_enrollment ) %>%
  rename( state = state_name ) %>%
  group_by( state, year ) %>%
  summarise( actual_enrollment = mean(total_medicaid_and_chip_enrollment))
  
medicaid_estimates_comparison <- 
  medicaid_elig_actual %>%
  left_join( 
    select( state_medicaid_suicide_panel,
            c(state, year, medical_income_assist_enrolled) ) 
    ) %>%
  filter( is.na(medical_income_assist_enrolled) != T,
          is.na(actual_enrollment) != T)

#not too shabby
ggplot( data = medicaid_estimates_comparison , 
        aes( x = medical_income_assist_enrolled , 
             y = actual_enrollment)
) +
  geom_point() + 
  geom_smooth(method='lm')
summary(
  lm(
    data = medicaid_estimates_comparison , 
    formula = actual_enrollment ~ medical_income_assist_enrolled
  )
)
cor(
  medicaid_estimates_comparison$actual_enrollment,
    medicaid_estimates_comparison$medical_income_assist_enrolled 
  )
