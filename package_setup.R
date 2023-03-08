setwd("C:/R/packages/FDEPgetdata")

library(devtools)
library(roxygen2)
library(RODBC)

document()

setwd("..")

install("FDEPgetdata")

getdata_aq_exclusions_multi_yr("'2020'","'CA18','CA19','CA20'","'CONFINED AQUIFER'")

getdata_aq_exclusions("'CA18'")

getdata_fw_exclusions("'CN20'")

getdata_lake_exclusions("'LL19'")

getdata_results("'CA18','CA19','CA20'")

getdata_trend_results("'AQUIFER','SPRING'","'01-OCT-1998'")

uninstall(pkg = "FDEPgetdata")

uninstall.


import(RODBC, dplyr, tidyr, splitstackshape, stringr, sqldf)
