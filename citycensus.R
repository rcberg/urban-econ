library( pacman )
p_load( ipumsr , 
        tidyverse , 
        dplyr , 
        ggplot2 )

# ddi_main <- read_ipums_ddi("D:/Economics/Data/Census/usa_00002.xml")
# census_micro_df = readRDS( file = "D:/Economics/Data/Census/usa_00002.rds" ) 
# census_micro_1990 = census_micro_df %>% 
#   filter( YEAR == 1990 ) %>%
#   mutate( puma = STATEFIP*10000 + PUMA )
# 
# rm(census_micro_df)

ddi_80 = read_ipums_ddi("D:/Economics/Data/Census/usa_00004.xml")
census_micro_1980 = read_ipums_micro( ddi_80 )
census_micro_1980 = census_micro_1980 %>%
  mutate( ctygrp1980 = STATEFIP*1000 + CNTYGP98 )
  


czone_xwalk = readRDS("czone_xwalk.rds") %>% 
  filter( year < 1991 )

czone_1980 = czone_xwalk %>%
  filter( year == 1980 ) %>%
  select( -puma )

census_econ_micro_1980 = census_micro_1980 %>%
  filter( AGE > 15 ,
          HHINCOME < 9999999) %>%
  mutate( hs_student = ifelse( EDUC < 7 & AGE < 19 , PERWT , 0 ) ,
          dropout = ifelse( EDUC < 6 & AGE > 17 , PERWT , 0 ) , 
          hs_only = ifelse( EDUC == 6 & AGE > 18 , PERWT , 0) ,
          any_college = ifelse( EDUC > 6 & AGE > 18 , PERWT , 0) ,
          fouryr_college = ifelse( EDUC > 9 & AGE > 18 , PERWT , 0) )

rm(census_micro_1980)

census_econ_1980 = census_econ_micro_1980 %>%
  group_by( STATEFIP , 
            CNTYGP98 ) %>%
  summarise( rent = weighted.mean( RENT ,
                                   w = PERWT) ,
             mortgage_owe = weighted.mean( MORTOTAL ,
                                           w = PERWT) ,
             property_tax = weighted.mean( PROPTXIN ,
                                           w = PERWT) ,
             avg_age = weighted.mean( AGE ,
                                      W = PERWT) ,
             hh_inc = weighted.mean(HHINCOME , 
                                    w = PERWT) ,
             fam_inc = weighted.mean(FTOTINC , 
                                     w = PERWT) ,
             total_pay = weighted.mean(INCWAGE , 
                                       w = PERWT) ,
             wage_pay = weighted.mean(INCWAGE , 
                                      w = PERWT) ,
             capital_pay = weighted.mean(INCINVST , 
                                         w = PERWT) ,
             hrs_avg = weighted.mean(UHRSWORK ,
                                     w = PERWT) ,
             ilf = weighted.mean(LABFORCE==2 ,
                                 w =  PERWT) ,
             unemployed = weighted.mean(EMPSTAT==2 , 
                                        w =  PERWT) ,
             women = weighted.mean(SEX==2 , 
                                   w = PERWT) , 
             black = weighted.mean( RACE==2 , 
                                    w = PERWT) ,
             population = sum( PERWT ) ,
             hs_student =  sum(hs_student ),
             dropout = sum(dropout ) , 
             hs_only = sum(hs_only ) ,
             any_college = sum(any_college ) ,
             fouryr_college = sum(fouryr_college )
  )
      
# czone_1990 = czone_xwalk %>%
#   filter( year == 1990 )

# census_micro_df = readRDS( file = "D:/Economics/Data/Census/usa_00002.rds" ) 

# census_micro_2000 = census_micro_df %>%
#   filter( YEAR == 2000 )

# census_micro_2010 = census_micro_df %>%
#   filter( YEAR == 2010 )
