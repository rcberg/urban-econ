library(pacman)
p_load( ipumsr ,
        tidyverse ,
        dplyr ,
        ggplot2)

setwd("D:/Economics/Data/ATUS/")
czone_xwalk <- readRDS("D:/Economics/Projects/urban-econ/czone_xwalk.rds")
ddi <- read_ipums_ddi("atus_00002.xml")
data <- read_ipums_micro(ddi) %>%
  mutate( leisure = ACT_SPORTS + ACT_SOCIAL )

timeuse_uncond_df = data %>%
  group_by( YEAR , 
            STATEFIP ,
            COUNTY ) %>%
  summarise( working = weighted.mean( ACT_WORK , 
                                      w = WT06 ) , 
             sporty = weighted.mean( ACT_SPORTS , 
                                     w = WT06 ) ,
             social = weighted.mean( ACT_SOCIAL ,
                                     w = WT06 ) , 
             leisure = weighted.mean( leisure ,
                                      w = WT06 ) ,
             n = n() )

timeuse_cond_df_wrk = data %>%
  filter( ACT_WORK > 0 ) %>%
  group_by( YEAR , 
            STATEFIP ,
            COUNTY ) %>%
  summarise( working = weighted.mean( ACT_WORK , 
                                      w = WT06 ) ,
             n = n() )

timeuse_cond_df_soc = data %>%
  filter( ACT_SOCIAL > 0 ) %>%
  group_by( YEAR , 
            STATEFIP ,
            COUNTY ) %>%
  summarise( social = weighted.mean( ACT_SOCIAL , 
                                      w = WT06 ) ,
             n = n() )

timeuse_cond_df_spt = data %>%
  filter( ACT_SPORTS > 0 ) %>%
  group_by( YEAR , 
            STATEFIP ,
            COUNTY ) %>%
  summarise( sporty = weighted.mean( ACT_SPORTS , 
                                      w = WT06 ) ,
             n = n() )

timeuse_cond_df_leisure = data %>%
  filter( leisure > 0 ) %>%
  group_by( YEAR , 
            STATEFIP ,
            COUNTY ) %>%
  summarise( social = weighted.mean( leisure , 
                                     w = WT06 ) ,
             n = n() )

