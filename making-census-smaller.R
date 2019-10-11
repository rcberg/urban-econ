library( ipumsr )

ddi <- read_ipums_ddi("D:/Economics/Data/Census/usa_00002.xml")
data <- read_ipums_micro(ddi)

saveRDS( data ,
         file = "D:/Economics/Data/Census/usa_00002.rds")
