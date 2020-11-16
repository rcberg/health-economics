library(data.table)

library(ipumsr)
ddi <- read_ipums_ddi("data/raw/usa_00017.xml")

usa_dt <- data.table(readRDS("data/export/usa_2011_2018.rds"))
medicaid_rules <- data.table(readRDS("data/export/state_medicaid_eligibility_2011_2020_wide.rds"))
fpl_rules <- data.table(readRDS("data/export/federal_poverty_levels_2011_2020.rds"))

state_pop <- 
  usa_dt[,
         .(
           population = sum(perwt)
           ),
         by=.(statefip,year)]
saveRDS(state_pop, "data/export/usa_state_pop_2011_2018.rds")

state_personalinc <- 
  usa_dt[ inctot > 0 & inctot < 9999999,
         .(
           personal_income = weighted.mean(x=inctot,w=perwt)
         ),
         by=.(statefip,year)]
saveRDS(state_personalinc, "data/export/usa_state_income_2011_2018.rds")

state_medicaid_enrolled <- 
  usa_dt[ hinscaid == 2,
          .(
            medicaid_enrolled = sum(perwt)
          ),
          by=.(statefip,year)]

saveRDS(state_medicaid_enrolled, "data/export/usa_state_medicaid_rolls_2011_2018.rds")

usa_dt <- 
  usa_dt[ poverty > 0 & ftotinc < 9999999,] 

households_dt <- usa_dt[, `:=`(
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

rm(usa_dt)

#fpl_year_func <- 
#  function(i, data= households_dt , fpls = fpl_rules ){
#    data_year <- data[year == i , ]
#    fpls_year <- fpls[year == i , ]
#    data_year_j = data_year[,
#                            hh_fpl := ifelse( famsize == 1 & n_elderly == 1 , 
#                                                     yes = fpls_year[, "fpl_over_65"],
#                                                     no = ifelse(famsize == 2 & n_elderly == 0 ,
#                                                                 yes = fpls_year[, "fpl_2_under_65"],
#                                                                 no = ifelse( famsize == 2 & n_elderly == 2  ,
#                                                                              yes = fpls_year[, "fpl_2_over_65"],
#                                                                              no = ifelse( famsize == 2 & n_elderly < 2  ,
#                                                                                           yes = fpls_year[, "fpl_2"],
#                                                                                           no = ifelse( famsize == 3,
#                                                                                                        yes = fpls_year[, "fpl_3"],
#                                                                                                        no = ifelse( famsize == 4,
#                                                                                                                     yes = fpls_year[, "fpl_4"],
#                                                                                                                     no = ifelse( famsize == 5,
#                                                                                                                                  yes = fpls_year[,"fpl_5"],
#                                                                                                                                  no =ifelse( famsize == 6,
#                                                                                                                                              yes = fpls_year[, "fpl_6"],
#                                                                                                                                              no =ifelse( famsize == 7,
#                                                                                                                                                          yes = fpls_year[, "fpl_7"],
#                                                                                                                                                          no = ifelse( famsize == 8,
#                                                                                                                                                                       yes = fpls_year[, "fpl_8"],
#                                                                                                                                                                       no = ifelse( famsize == 9,
#                                                                                                                                                                                    yes = fpls_year[, "fpl_9"],
#                                                                                                                                                                                    no = fpls_year[ , "fpl_9"] + (famsize - 9)*fpls_year[, "additional_person"]
#                                                                                                                                                                       )
#                                                                                                                                                          )
#                                                                                                                                              )
#                                                                                                                                  )
#                                                                                                                     )
#                                                                                                        )
#                                                                                           )
#                                                                              )
#                                                                 )
#                                                     )
#                            )]
#    return(data_year_j)
#  }
#
#library(future.apply)
#options(future.globals.maxSize=1000*1024^2)
#plan(multiprocess)
#household_computed_poverty = rbindlist(future_lapply(2011:2018, fpl_year_func))
#plan(sequential)
#household_computed_poverty <- household_computed_poverty[,poverty_comp:=ifelse( (hh_inc/as.numeric(hh_fpl))*100 > 501,
#                                                                                501,
#                                                                                (hh_inc/as.numeric(hh_fpl))*100) ]
##this is the top code that IPUMS uses. i might get rid of it but
#
#cor( x = household_computed_poverty$poverty[household_computed_poverty$poverty > 0 ],
#     y = household_computed_poverty$poverty_comp[household_computed_poverty$poverty > 0 ])
#summary(lm( poverty_comp ~ poverty , data = household_computed_poverty[poverty > 0 ,]))
## to show that the measures are very similar
#
#library(ggplot2)
#ggplot( data = household_computed_poverty[poverty > 0 ,
#                                          ][, bin := cut(x = poverty , breaks = 500 )
#                                            ][
#                                            , .(pov = mean(poverty) , 
#                                                pov_comp = mean(poverty_comp) ), by = bin] 
#        ) + 
#  geom_point( aes(x = pov , y = pov_comp) , alpha = 0.2) + 
#  geom_smooth(aes(x = pov , y = pov_comp) , method = 'lm') + 
#  geom_abline( slope = 1 , intercept = 0 , color = 'red' , size = 1 )

household_elig <- 
  merge.data.table(households_dt, medicaid_rules)

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

saveRDS(household_elig_lite , "data/export/medicaid_eligibility_panel.rds")
saveRDS(household_elig , "data/export/medicaid_eligibility_panel_detail.rds")
