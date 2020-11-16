library(data.table)

library(ipumsr)
ddi <- read_ipums_ddi("data/raw/usa_00017.xml")

household_dt <- 
  data.table(readRDS("data/export/medicaid_eligibility_panel.rds"))

state_elig_panel <- 
  household_dt[,
               .(mean_family_income = weighted.mean(ftotinc, w=hhwt) , 
                 medicaid_eligible = sum(hhwt*n_elig),
                 medicaid_chip_eligible = sum(hhwt*n_elig_chip),
                 households = sum(hhwt)
                 ),
               by=.(statefip,year)]

state_income_panel <- 
  data.table(readRDS("data/export/usa_state_income_2011_2018.rds"))
state_population_panel <- 
  data.table(readRDS("data/export/usa_state_pop_2011_2018.rds"))
state_income_pop_data <- 
  merge.data.table(state_income_panel,
        state_population_panel)
state_medicaid_rolls <- 
  data.table(readRDS("data/export/usa_state_medicaid_rolls_2011_2018.rds"))
state_medicaid_rolls_data <- 
  merge.data.table(state_income_pop_data,
                   state_medicaid_rolls)

state_panel_data <- 
  merge.data.table(state_elig_panel,state_medicaid_rolls_data, by=c("statefip","year"))

state_panel_data <- 
  state_panel_data[,`:=`(medicaid_eligibility_ratio = medicaid_eligible/population,
                       med_chip_elig_ratio = medicaid_chip_eligible/population ,
                       medical_income_assist_enrolled = medicaid_enrolled)
                 ][
                       , medicaid_enrolled:=NULL]

saveRDS(state_panel_data, "data/export/state_medicaid_elig_panel.rds" )
