setwd("C:/R/packages/FDEPgetdata")

library(devtools)
library(RODM)
library(roxygen2)

document()

setwd("..")

install("FDEPgetdata")

getdata_aq_exclusions('CA_EXCLUSIONS_2019')

getdata_results("'CA18'")

uninstall(pkg = "FDEPgetdata")

uninstall.


import(RODBC, RODM, dplyr, tidyr, splitstackshape, stringr)
