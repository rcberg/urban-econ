library(pacman)
p_load( ipumsr, 
        broom ,
        haven ,
        dplyr , 
        furrr ,
        purrr , 
        tidyverse ,
        estimatr ,
        tictoc  )

# Data Gathering and Cleaning

setwd("D:/Economics/Data/CPS")
ddi <- read_ipums_ddi("cps_00027.xml")
data <- read_ipums_micro(ddi)


## March Basic data
# mar_df  <- data  %>% 
# filter( ASECFLAG == 2 ) %>% 
# select( -c(ASECWTH, 
#   ASECWT)
# )

# Annual Social and Economic Supplement (March Supplement)

asec_df = data %>% 
  mutate(HFLAG = replace_na(HFLAG, 
                            1 )
         ) %>% 
  filter( ASECFLAG == 1 & HFLAG ==1 ) # See https://cps.ipums.org/cps/three_eighths.shtml


# state FIPS and state names

statecodes = c( 
  1	,  2	,   4	,   5	,   6	,   8	,   9	,   10	,
  11	,   12	,   13	,   15	,   16	,   17	,   18	,
  19	,   20	,   21	,   22	,   23	,   24	,   25	,   
  26	,   27	,   28	,   29	,   30	,   31	,   32	,   
  33	,   34	,   35	,   36	,   37	,   38	,   39	,
  40	,   41	,   42	,   44	,   45	,   46	,   47  ,   
  48	,   49	,   50	,   51	,   53	,   54	,   55	,
  56  
)
statelabs = c(
  "Alabama"	,"Alaska"	,"Arizona"	,"Arkansas"	,"California"	,"Colorado"	,"Connecticut"	,"Delaware"	,
  "District of Columbia"	,"Florida"	,"Georgia"	,"Hawaii"	,"Idaho"	,"Illinois"	,"Indiana"	,"Iowa"	,
  "Kansas"	,"Kentucky"	,"Louisiana"	,"Maine"	,"Maryland"	,"Massachusetts"	,"Michigan"	,"Minnesota"	,
  "Mississippi"	,"Missouri"	,"Montana"	,"Nebraska"	,"Nevada"	,"New Hampshire"	,"New Jersey"	,"New Mexico"	,
  "New York"	,"North Carolina"	,"North Dakota"	,"Ohio" ,"Oklahoma"	,"Oregon"	,"Pennsylvania"	,"Rhode Island"	,
  "South Carolina"	,"South Dakota"	,"Tennessee"	,"Texas"	,"Utah"	,"Vermont"	,"Virginia"	,"Washington"	,
  "West Virginia"	,"Wisconsin"	,"Wyoming" 
)

### Collapsing by state/year

## This process needs to be done, in my experience, because dplyr will either take an incredibly long time
## "summarise"-ing the entire dataset, perhaps it hits some kind of large number limit that prevents it from 
## finishing that. When I break it up into smaller variable sets and (sometimes) year sets and collapse
## those, it will work in reasonable time.

collapse_function_wage = function( i ){
  pay_df_i = asec_df %>% 
    filter(YEAR == i , 
           INCWAGE < 9999998 ) %>% 
    group_by(STATEFIP ,
             METAREA , 
             YEAR ) %>% 
    summarise( wage_pay = weighted.mean(INCWAGE , 
                                        w = ASECWT) )
}
collapse_function_hrs = function( i ){
  hrs_df_i = asec_df %>% 
    filter(YEAR == i , 
           UHRSWORKLY < 999 ) %>% 
    group_by(STATEFIP ,
             METAREA , 
             YEAR ) %>% 
    summarise( hrs_avg = weighted.mean(UHRSWORKLY ,
                                       w = ASECWT ) )
}
collapse_function_lf1 = function(i){
  asec_lf1_i = asec_df %>% 
    filter(AGE>13 & YEAR==i) %>% 
    group_by( STATEFIP ,
              METAREA , 
              YEAR ) %>% 
    summarise( ilf = weighted.mean(LABFORCE==2 ,
                                   w = ASECWT ),
               unemployment = weighted.mean(EMPSTAT==20|
                                              EMPSTAT==21|
                                              EMPSTAT==22 , 
                                            w = ASECWT )
    )
}
collapse_function_lf = function(i){
  asec_lf_i = asec_df %>% 
    filter(AGE>14 & YEAR==i) %>% 
    group_by( STATEFIP ,
              METAREA , 
              YEAR ) %>% 
    summarise( ilf = weighted.mean(LABFORCE==2 , 
                                   w = ASECWT ),
               unemployment = weighted.mean(EMPSTAT==20|
                                              EMPSTAT==21|
                                              EMPSTAT==22 , 
                                            w = ASECWT )
    ) 
}
collapse_function_vet1 = function(i){
  asec_vet1_i = asec_df %>% 
    filter(AGE>14 & YEAR==i) %>% 
    group_by( STATEFIP ,
              METAREA , 
              YEAR ) %>% 
    summarise( vet = weighted.mean( VETSTAT==2 , 
                                    w = ASECWT ))
  
}
collapse_function_vet = function(i){
  asec_vet_i = asec_df %>% 
    filter(AGE>16 & YEAR==i) %>% 
    group_by( STATEFIP ,
              METAREA , 
              YEAR ) %>% 
    summarise( vet = weighted.mean( VETSTAT==2 , 
                                    w = ASECWT )
               )
  
}
collapse_function_care = function(i){
  asec_care_i = asec_df %>% 
    filter(AGE>14 & YEAR==i) %>% 
    group_by( STATEFIP ,
              METAREA , 
              YEAR ) %>% 
    summarise( medicare = weighted.mean(HIMCARE==2 , 
                                        w = ASECWT ) 
               )
  
}
collapse_function_pop = function(i){
  asec_pop_i = asec_df %>% 
    filter(YEAR==i) %>% 
    group_by( STATEFIP ,
              METAREA ,  
              YEAR ) %>% 
    summarise( population = sum(ASECWT) ,
               women = weighted.mean(SEX==2 , 
                                     w = ASECWT) , 
               black = weighted.mean( RACE==200 , 
                                      w = ASECWT) ,
               prime_age_pop = weighted.mean(AGE > 24 & AGE < 66 , 
                                             w = ASECWT) , 
               m_older_age_pop = weighted.mean(AGE > 44 & SEX==1 , 
                                               w = ASECWT) , 
               f_mid_age = weighted.mean(AGE > 44 & AGE < 56 & SEX==2 , 
                                         w = ASECWT) , 
               private = weighted.mean(COVERPI==2 , 
                                       w = ASECWT) ,
               medicaid = weighted.mean(HIMCAID==2, 
                                        w = ASECWT)  ,
               militcare = weighted.mean(HICHAMP==2 ,
                                         w = ASECWT ) 
    )
}

tic()

wage_df = map_dfr(1977:2018 , 
                  collapse_function_wage )
hrs_df = map_dfr(1977:2018 , 
                 collapse_function_hrs ) # this has exactly 2 rows fewer than every other part of the data
asec_lf1 = map_dfr(1977:1989 , 
                   collapse_function_lf1 )
asec_lf = map_dfr(1990:2018 , 
                  collapse_function_lf) %>% 
  bind_rows(asec_lf1) %>% 
  arrange(YEAR , 
          .by_group = TRUE )
asec_vet1 = map_dfr(1977:2005 , 
                    collapse_function_vet1)
asec_vet = map_dfr(2006:2018 , 
                   collapse_function_vet) %>% 
  bind_rows(asec_vet1) %>% 
  arrange(YEAR , .by_group = TRUE )
asec_care = map_dfr(1977:2018 , 
                    collapse_function_care)
asec_pop_a = map_dfr(1977:1997 , 
                     collapse_function_pop )
asec_pop = map_dfr( 1998:2018 , 
                    collapse_function_pop) %>% 
  bind_rows( asec_pop_a ) %>% 
  arrange( YEAR , 
           .by_group = TRUE)
asec_msa = asec_pop %>% 
  bind_cols( asec_care , 
             wage_df , 
             asec_vet , 
             asec_lf ) %>%
  filter(  !(YEAR == 2015 & METAREA == 451 | 
             YEAR == 2015 & METAREA==2001 )  # these met areas are missing hours
         ) %>%
  bind_cols( hrs_df ) %>%
  select( -c(STATEFIP1 , METAREA1 , YEAR1 ,
             STATEFIP2 , METAREA2 , YEAR2 ,
             STATEFIP3 , METAREA3 , YEAR3 ,
             STATEFIP4 , METAREA4 , YEAR4 ,
             STATEFIP5 , METAREA5 , YEAR5 ) ) %>%
  rename( "state" = "STATEFIP" ,
          "year" = "YEAR" ,
          "msa" = "METAREA" )

toc()

asec_msa$state <- factor(asec_msa$state ,
                        levels = statecodes,
                        labels = statelabs
)

saveRDS( asec_msa , 
         file = "asec_msa.rds" )
