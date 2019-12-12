library(pacman)
p_load( tidyverse , 
        dplyr , 
        ggplot2 ,
        haven)

setwd("D:/Economics/Projects/urban-econ")

acs_post2005 = readRDS("acs_post2005.rds") # getting the census zone ACS data
qcew_czone_yearly = readRDS("qcew_czone_year.rds") # getting QCEW data for robustness

# making the QCEW data compatable with the selection from the ACS 
acs_qcew = acs_post2005  %>%
  filter( year > 2004 & 
            year < 2018 ) %>%
  group_by(czone, 
           year) %>%
  summarise( population = sum(population) , 
             women = sum(women) ,
             black = sum(black) ,
             dropout = sum(dropout) ,
             hs_only = sum(hs_only) ,
             any_college = sum(any_college) ,
             ilf = sum(ilf) ,
             unemployed = sum(unemployed) ,
             avg_age = mean(avg_age) ,
             hrs_avg = mean(hrs_avg_cnd) ,
             total_pay = mean(total_pay_cnd) ,
             capital_pay = mean(capitaltotla_cnd) ,
             wage_pay = mean(wage_cnd)
             ) %>%
  mutate( lfpr = ilf / population , 
          urate = unemployed / ilf ) %>%
  merge(qcew_czone_yearly) %>%
  arrange(czone , year)

# plot of wages
g = ggplot( data = acs_qcew %>% filter(year < 2017 ) ,
            aes( x = log(population) , 
                 y = log(wage_pay) ) )

g + geom_point() +
  geom_smooth( method = "lm") +
  facet_wrap(~year) +
  theme_bw()

# setting up the loop where i'm going to do the regressions for each year the ACS gives me
init_reg_matrix = data.frame( year = (2006:2017) , a = rep(0,12) , b = rep(0,12) )
reg_matrix_acsw = list( init_reg_matrix ,
                   init_reg_matrix ,
                   init_reg_matrix ,
                   init_reg_matrix )

# look at the education distribution to see if it's a composition issue
educ_list = list( "dropout" , 
                  "hs_only" , 
                  "any_college" ,
                  "population" 
                  )

# loops over years, does regressions and fills the coefficient dataframe with the estimates
for(j in 1:length(educ_list)){
  for(t in 2006:2017){
  i = t - 2005
  acs_qcew_tmp = acs_qcew %>% 
    filter( year == t ) %>% 
    select( year , wage_pay , educ_list[[j]] )
  y = unlist(acs_qcew_tmp[2])
  x = unlist(acs_qcew_tmp[3])
  model_tmp = lm( log(y) ~ log(x) )
  reg_matrix_acsw[[j]][i,2] = model_tmp$coefficients[1]
  reg_matrix_acsw[[j]][i,3] = model_tmp$coefficients[2]

  }
  reg_matrix_acsw[[j]]$variable = educ_list[[j]]
}
educ_estimates_acsw = reg_matrix_acsw %>% bind_rows()

# plotting the coefficients
p_acsw = ggplot(data = educ_estimates_acsw
           ) 

p_acsw + geom_line(aes(x = year , 
                   y = b  ,
                  color = variable ) 
               ) +
  theme_bw()

g = ggplot( data = acs_qcew %>% filter(year < 2017 ) ,
            aes( x = log(population) , 
                 y = log(wage_weekly) ) )

g + geom_point() +
  geom_smooth( method = "lm") +
  facet_wrap(~year) +
  labs( x = "Log of population" ,
        y = "Log of average weekly wage" ,
        title = "Population and Wages: 2005 to 2016") +
  theme_bw()


# same as above but using QCEW data for weekly wages instead of census

reg_matrix_qcew = list( init_reg_matrix ,
                        init_reg_matrix ,
                        init_reg_matrix ,
                        init_reg_matrix )

for(j in 1:length(educ_list)){
  for(t in 2006:2017){
    i = t - 2005
    acs_tmp = acs_qcew %>% 
      filter( year == t ) %>% 
      select( year , wage_weekly , educ_list[[j]] )
    y = unlist(acs_tmp[2])
    x = unlist(acs_tmp[3])
    model_tmp = lm( log(y) ~ log(x) )
    reg_matrix_qcew[[j]][i,2] = model_tmp$coefficients[1]
    reg_matrix_qcew[[j]][i,3] = model_tmp$coefficients[2]
    
  }
  reg_matrix_qcew[[j]]$variable = educ_list[[j]]
}
educ_estimates_qcew = reg_matrix_qcew %>% bind_rows()


p_qcew = ggplot(data = educ_estimates_qcew %>% filter( variable != "population" )
) 

p_qcew + geom_line(aes(x = year , 
                       y = b  ,
                       color = variable ) 
) +
  labs( x = "Year" , 
        y = "Intercept Coefficient" , 
        title = "Intercept Term of Wage-Population") +
  theme_bw()

reg_matrix_inc = list( init_reg_matrix ,
                        init_reg_matrix ,
                        init_reg_matrix ,
                        init_reg_matrix )

# etc, looking at total income
for(j in 1:length(educ_list)){
  for(t in 2006:2017){
    i = t - 2005
    acs_tmp = acs_qcew %>% 
      filter( year == t ) %>% 
      select( year , total_pay , educ_list[[j]] )
    y = unlist(acs_tmp[2])
    x = unlist(acs_tmp[3])
    model_tmp = lm( log(y) ~ log(x) )
    reg_matrix_inc[[j]][i,2] = model_tmp$coefficients[1]
    reg_matrix_inc[[j]][i,3] = model_tmp$coefficients[2]
    
  }
  reg_matrix_inc[[j]]$variable = educ_list[[j]]
}
educ_estimates_inc = reg_matrix_inc %>% bind_rows()


p_inc = ggplot(data = educ_estimates_inc
) 

p_inc + geom_line(aes(x = year , 
                       y = b  ,
                       color = variable ) 
) +
  theme_bw()

czones_2015 = acs_qcew %>% 
  filter( year == 2015) %>% 
  select( czone , wage_weekly , wage_pay , total_pay )%>%
  rename( wage_weekly_2015 = wage_weekly ,
          wage_cnd_2015 = wage_pay ,
          total_pay_cnd_2015 = total_pay )

czones_2005 = acs_qcew %>% 
  filter(year == 2005) %>% 
  select( czone , wage_weekly , wage_pay , total_pay ) %>%
  rename( wage_weekly_2005 = wage_weekly ,
          wage_cnd_2005 = wage_pay ,
          total_pay_cnd_2005 = total_pay )
czones_2005 = czones_2005[czones_2005$czone %in% czones_2015$czone, ]


czones_compare = merge(czones_2005 , czones_2015)

acs_qcew_rusty = acs_qcew %>% 
               filter(   czone == 14900|
                         czone == 11600|
                         czone == 12100|
                         czone == 12200|
                         czone == 24100|
                         czone == 19700| 
                         czone == 18600| 
                         czone == 17900|
                         czone == 15200|
                         czone == 12501|
                         czone == 19000|
                         czone == 19100|
                         czone == 13400|
                         czone == 20500|
                         czone == 14000|
                         czone == 18800|
                         czone == 13600|
                         czone == 20800|
                         czone == 17700|
                         czone == 16400|
                         czone == 19200
                           
)

ggplot( data = acs_qcew %>% filter( year == 2005 | year == 2015), 
        aes(y = log(wage_weekly) ,
            x = log(population)
            )
        ) +
  geom_point( aes( color = as.factor(year) )
              ) +
  geom_smooth( method = 'lm' ) +
  labs( title = "US Commuting Zones" ,
        x = "Ln Population" ,
        y = "Ln Average Weekly Wage" ,
        caption = "Population data from ACS; wage data from QCEW") +
  theme_bw() +
  theme( legend.title = element_blank() ,
         text = element_text( size = 17))


cty_czone = read_dta( "cw_cty_czone.dta")

library(dplyr)
library(rvest)

counties = read_html("https://www.nrcs.usda.gov/wps/portal/nrcs/detail/or/programs/?cid=nrcs143_013697") %>%
  html_nodes("#detail > table") %>%
  html_table(fill=TRUE) 

countyfips = counties[[1]] %>%
  rename( stateabb = State)

czone_countynames = countyfips %>%
  rename( cty_fips = FIPS ) %>%
  merge( cty_czone ) %>%
  arrange( stateabb , 
           Name)

#countyfips == 18089|
#countyfips == 26049|
#countyfips == 26077|
#countyfips == 26081|
#countyfips == 26125|
#countyfips == 24100|
#countyfips == 34007| 
#countyfips == 36001| 
#countyfips == 36007|
#countyfips == 39153|
#countyfips == 39113|
#countyfips == 42077|
#countyfips == 42095

acs_qcew %>% 
  filter( year == 2010) %>%
  ggplot( aes(x = log(population) , 
                         y = log(wage_weekly))) +
  geom_point() + 
  geom_smooth(method='lm') +
  labs( x = "Log Population" , 
        y = "Log Avg. Weekly Wage" , 
        caption = "(Data from 2010 ACS and QCEW)") + 
  theme_bw() + 
  theme(text = element_text( size = 17))
