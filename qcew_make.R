library(tidyverse)
library(dplyr)
library(reshape)
library(openxlsx)

setwd("D:/Economics/Data/QCEW/")
qcew_xlsx = list.files( pattern = ".xlsx" )

qcew_bind = function(i){
  qcew_i = read.xlsx(i) %>% 
    select(-Status.Code) %>%
    melt( id = c("Area.Code"  ,
                "St"     ,
                "Cnty"        ,
                "Own"     , 
                "NAICS"        ,  
                "Year"     ,
                "Qtr"           ,
                "Area.Type" ,
                "St.Name"        ,
                "Area"       ,
                "Ownership"       ,  
                "Industry"    , 
                "Establishment.Count",
                "Total.Quarterly.Wages",  
                "Average.Weekly.Wage" , 
                "Employment.Location.Quotient.Relative.to.U.S.", 
                "Total.Wage.Location.Quotient.Relative.to.U.S." 
         ) 
    )
  
}

qcew_df = map_dfr( qcew_xlsx , 
                   qcew_bind ) %>%
  dplyr::rename( month = variable ,
                 employment = value )

qcew_df$month = qcew_df$month %>% recode( January.Employment = "1" ,
                                          February.Employment = "2" ,
                                          March.Employment = "3" ,
                                          April.Employment = "4" ,
                                          May.Employment = "5" ,
                                          June.Employment = "6" ,
                                          July.Employment = "7" ,
                                          August.Employment = "8" ,
                                          September.Employment = "9" ,
                                          October.Employment = "10" ,
                                          November.Employment = "11" ,
                                          December.Employment = "12" 
) %>% as.numeric()

saveRDS( qcew_df , file = "qcew_99_19.rds" )
