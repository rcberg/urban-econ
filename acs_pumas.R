library( pacman )
p_load(ipumsr , 
       tidyverse ,
       haven , 
       dplyr )

setwd("D:/Economics/Data/Census")
ddi = read_ipums_ddi("D:/Economics/Data/Census/usa_00005.xml")
# data = read_ipums_micro( ddi ) %>% filter( YEAR > 2004 )

# saveRDS( data, 
#          "acs_post2005.rds" )

data = readRDS("acs_post2005.rds") %>% 
  mutate( puma = 10000*STATEFIP + PUMA)

demog_data = data %>%
  mutate( student = ifelse( EDUC < 7 & AGE < 19 , PERWT , 0 ) ,
          women = ifelse(SEX==2 , PERWT , 0 ) , 
          black = ifelse( RACE==2 , PERWT , 0 ) ) %>%
  lapply( as.numeric) %>% 
  bind_cols()

saveRDS( demog_data ,
         "acs05_demogs_micro.rds")

acs_05p_demogs = demog_data %>% 
  group_by( YEAR , 
            STATEFIP , 
            puma) %>%
  summarise( students = sum(student) ,
             population = sum( PERWT ) ,
             women = sum( women ), 
             black = sum( black ) ,
             avg_age = weighted.mean( AGE ,
                                      W = PERWT) ,
             n = n()
  ) 

saveRDS( acs_05p_demogs ,
         "acs05_demogs_puma.rds")

rm( demog_data )

lf_data = data %>%
  filter( AGE > 15 ) %>%
  mutate( student = ifelse( EDUC < 7 & AGE < 19 , PERWT , 0 ) ,
          dropout = ifelse( EDUC < 6 & AGE > 17 , PERWT , 0 ) , 
          hs_only = ifelse( EDUC == 6 & AGE > 18 , PERWT , 0) ,
          any_college = ifelse( EDUC > 6 & AGE > 18 , PERWT , 0) ,
          fouryr_college = ifelse( EDUC > 9 & AGE > 18 , PERWT , 0) ,
          ilf = ifelse(LABFORCE==2 , PERWT , 0 ) ,
          unemployed = ifelse(EMPSTAT==2 , PERWT , 0 ) ,
          women = ifelse(SEX==2 , PERWT , 0 ) , 
          black = ifelse( RACE==2 , PERWT , 0 ) 
  ) %>%
  lapply( as.numeric) %>% 
  bind_cols()

saveRDS( lf_data ,
         "acs05_lf_micro.rds")

rm(data)
lf_data = readRDS("D:/Economics/Data/Census/acs05_lf_micro.rds")
acs_05p_labforce_p1 = lf_data %>%
  group_by(YEAR ,
           STATEFIP , 
           puma ) %>%
  summarise(population = sum( PERWT ) ,
            dropout = sum(dropout ) , 
            hs_only = sum(hs_only ) ,
            any_college = sum(any_college ) ,
            fouryr_college = sum(fouryr_college ) ,
            ilf = sum(ilf),
            unemployed = sum(unemployed) ,
            women = sum(women), 
            black = sum(black) ,
            n = n() 
  ) %>%
  rename( year = YEAR )

acs_hours_cnd_fn = function(i){
  acs_labor_cnd = lf_data %>% filter( UHRSWORK > 0 )
  acs_05p_labor_i = acs_labor_cnd %>% 
    filter( YEAR == i )
  
  acs_05p_hours_i = acs_05p_labor_i %>%
    group_by(STATEFIP , 
             puma ) %>%
    summarise( hrs_avg_cnd = weighted.mean(UHRSWORK ,
                                           w = PERWT) ,
               n_L = n() 
    )%>%
    mutate( year = i )
}

acs_inctot_cnd_fn = function(i){  
  acs_income_cnd = lf_data %>% filter( INCTOT > 0 )
  acs_05p_income_i = acs_income_cnd %>% 
    filter( YEAR == i )
  
  acs_05p_inc_i = acs_05p_income_i %>%
    group_by(STATEFIP , 
             puma ) %>%
    summarise( total_pay_cnd = weighted.mean(INCTOT , 
                                              w = PERWT) ,
                n_Y = n() 
    )%>%
    mutate( year = i )
}

acs_wages_cnd_fn = function(i){  
  acs_wage_cnd = lf_data %>% filter( INCWAGE > 0)
  acs_05p_wage_i = acs_wage_cnd %>% 
    filter( YEAR == i )
  
  acs_05p_inc_i = acs_05p_wage_i %>%
    group_by(STATEFIP , 
             puma ) %>%
    summarise( wage_cnd = weighted.mean(INCWAGE , 
                                             w = PERWT) ,
               n_W = n() 
    )%>%
    mutate( year = i )
}  

acs_capit_cnd_fn = function(i){  
  acs_capit_cnd = lf_data %>% filter( INCINVST > 0 )
  acs_05p_capit_i = acs_capit_cnd %>% 
    filter( YEAR == i )
  
  acs_05p_k_i = acs_05p_capit_i %>%
    group_by(STATEFIP , 
             puma ) %>%
    summarise( capitaltotla_cnd = weighted.mean(INCINVST , 
                                             w = PERWT) ,
               n_K = n() 
    )%>%
    mutate( year = i )
}    

acs_capit_cnd = map_dfr(2005:2017 , acs_capit_cnd_fn)
acs_wages_cnd = map_dfr(2005:2017 , acs_wages_cnd_fn)
acs_inctot_cnd = map_dfr(2005:2017 , acs_inctot_cnd_fn)
acs_hours_cnd = map_dfr(2005:2017 , acs_hours_cnd_fn)

acs_05p_labforce_cnd_p1 = merge(acs_hours_cnd , acs_inctot_cnd)
acs_05p_labforce_cnd_p2 = merge( acs_05p_labforce_cnd_p1 , acs_wages_cnd )
acs_05p_labforce_cnd_p3 = merge(acs_05p_labforce_cnd_p2 , acs_capit_cnd)
acs_05p_labforce_cnd = merge( acs_05p_labforce_p1 , acs_05p_labforce_cnd_p3)

rm( acs_05p_labforce_cnd_p1 , 
    acs_05p_labforce_cnd_p2 ,
    acs_05p_labforce_cnd_p3
    )


p_l = acs_hours_cnd %>% filter( year == 2011) %>% ggplot()
hours_sample = p_l + 
  geom_histogram( aes( x = n_L )) +
  labs(x = "N",
  title = "Histogram of Hours Observations per PUMA")

p_y = acs_inctot_cnd %>% filter( year == 2011) %>% ggplot()
income_sample = p_y + 
  geom_histogram( aes( x = n_Y ))+
  labs(x = "N",
       title = "Histogram of Income Observations per PUMA")

p_w = acs_wages_cnd %>% filter( year == 2011) %>% ggplot()
wage_sample = p_w + 
  geom_histogram( aes( x = n_W ))+
  labs(x = "N",
       title = "Histogram of Wage Observations per PUMA")

p_k = acs_capit_cnd %>% filter( year == 2011) %>% ggplot()
capital_sample = p_k + 
  geom_histogram( aes( x = n_K ))+
  labs(x = "N",
       title = "Histogram of Capital Observations per PUMA")


setwd("D:/Economics/Projects/urban-econ/")
hours_sample
ggsave("puma_hrs_n.png" )
income_sample
ggsave("puma_inc_n.png" )
wage_sample
ggsave("puma_wage_n.png" )
capital_sample
ggsave("puma_capit_n.png"  )

rm(acs_capit_cnd ,
   acs_wages_cnd ,
   acs_inctot_cnd ,
   acs_hours_cnd ,
   p)

saveRDS( acs_05p_labforce_cnd , 
         file = "D:/Economics/Data/Census/acs_2005_labforce_cnd.rds" )

rm(list=ls())

acs_demogs = readRDS("D:/Economics/Data/Census/acs05_demogs_puma.rds") 

p = acs_demogs %>% filter( YEAR == 2011) %>% ggplot()
sample = p + 
  geom_histogram( aes( x = n )) +
  labs(x = "N",
       title = "Histogram of Observations per PUMA")

sample
ggsave(plot =  sample , "puma_n.png"  )

acs_demogs = acs_demogs %>% 
  select(-n) %>%
  rename( year = YEAR )
acs_labor = readRDS("D:/Economics/Data/Census/acs_2005_labforce_cnd.rds") %>%
  select( -c(n_L ,
             n_Y ,
             n_K ,
             n_W ) )

acs_laborforce = acs_labor %>% 
  select( -c( population , 
              women ,
              black))

acs_post2005 = merge( acs_demogs , acs_laborforce ) %>%
  rename(state = STATEFIP) %>%
  mutate(lfpr = ilf / population ,
         urate = unemployed / ilf )

