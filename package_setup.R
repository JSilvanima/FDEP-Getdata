#setwd("C:/R/packages/FDEPgetdata")

library(devtools)
library(roxygen2)
library(odbc)
library(sf)
library(dplyr)
library(tidyr)
library(splitstackshape)
library(stringr)
library(sqldf)
library(rstudioapi)

document()

setwd("..")

install("FDEPgetdata")

getdata_aq_exclusions_multi_yr("'2020'","'CA18','CA19','CA20'","'CONFINED AQUIFER'")

getdata_aq_exclusions("'CA18'")

getdata_fw_exclusions("'CN20'")

getdata_lake_exclusions("'LL19'")

getdata_results("'CA18','CA19','CA20'")

getdata_trend_results("'AQUIFER','SPRING'","'01-OCT-1998'","'31-DEC-2022'")

getdata_fw_site_removals(site_evaluations = Exclusions,
                         listframe_directory = "./data",
                         listframe = "Cycle17_streams_coverage_2023")

getdata_lake_site_removals(site_evaluations = Exclusions,
                           listframe_directory = "./data",
                           listframe = "Cycle17_LargeLakes_coverage_2023")

uninstall(pkg = "FDEPgetdata")

uninstall.


import(odbc, dplyr, tidyr, splitstackshape, stringr, sqldf, sf, rstudioapi)
