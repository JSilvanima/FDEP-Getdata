setwd("C:/R/packages/FDEPgetdata")

library(devtools)
library(RODM)
library(roxygen2)
library(RODBC)

document()

setwd("..")

install("FDEPgetdata")

getdata_aq_exclusions_3yr("'CA18','CA19','CA20'")

getdata_aq_exclusions("'CA18'")

getdata_fw_exclusions("'CN20'")

getdata_lake_exclusions("'LL19'")

getdata_results("'CA18','CA19','CA20'")

uninstall(pkg = "FDEPgetdata")

uninstall.


import(RODBC, RODM, dplyr, tidyr, splitstackshape, stringr, sqldf)
