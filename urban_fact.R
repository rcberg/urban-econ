library(pacman)
p_load( tidyverse , 
        dplyr , 
        ggplot2 )
setwd("D:/Economics/Projects/urban-econ")

acs_post2005 = readRDS("acs_post2005.rds") # getting the census zone ACS data
qcew_czone_yearly = readRDS("qcew_czone_year.rds") # getting QCEW data for robustness

# making the QCEW data compatable with the selection from the ACS 
acs_qcew = qcew_czone_yearly %>%
  filter( year > 2004 & 
            year < 2018 ) %>%
  merge(acs_post2005)

# plot of wages
g = ggplot( data = acs_qcew %>% filter(year < 2017 ) ,
            aes( x = log(population) , 
                 y = log(wage_cnd) ) )

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
  acs_atus_tmp = acs_qcew %>% 
    filter( year == t ) %>% 
    select( year , wage_cnd , educ_list[[j]] )
  y = unlist(acs_atus_tmp[2])
  x = unlist(acs_atus_tmp[3])
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
  theme_bw()


# same as above but using QCEW data for weekly wages instead of census

reg_matrix_qcew = list( init_reg_matrix ,
                        init_reg_matrix ,
                        init_reg_matrix ,
                        init_reg_matrix )

for(j in 1:length(educ_list)){
  for(t in 2006:2017){
    i = t - 2005
    acs_atus_tmp = acs_qcew %>% 
      filter( year == t ) %>% 
      select( year , wage_weekly , educ_list[[j]] )
    y = unlist(acs_atus_tmp[2])
    x = unlist(acs_atus_tmp[3])
    model_tmp = lm( log(y) ~ log(x) )
    reg_matrix_qcew[[j]][i,2] = model_tmp$coefficients[1]
    reg_matrix_qcew[[j]][i,3] = model_tmp$coefficients[2]
    
  }
  reg_matrix_qcew[[j]]$variable = educ_list[[j]]
}
educ_estimates_qcew = reg_matrix_qcew %>% bind_rows()


p_qcew = ggplot(data = educ_estimates_qcew
) 

p_qcew + geom_line(aes(x = year , 
                       y = b  ,
                       color = variable ) 
) +
  theme_bw()

reg_matrix_inc = list( init_reg_matrix ,
                        init_reg_matrix ,
                        init_reg_matrix ,
                        init_reg_matrix )

# etc, looking at total income
for(j in 1:length(educ_list)){
  for(t in 2006:2017){
    i = t - 2005
    acs_atus_tmp = acs_qcew %>% 
      filter( year == t ) %>% 
      select( year , total_pay_cnd , educ_list[[j]] )
    y = unlist(acs_atus_tmp[2])
    x = unlist(acs_atus_tmp[3])
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
