setwd("C:/R/packages/FDEPgetdata")

library(devtools)
library(RODM)
library(roxygen2)

document()

setwd("..")

install("FDEPgetdata")

getdata_fw_exclusions('CN_EXCLUSIONS_2020')

getdata_fw_results("'CN2001'")

uninstall(pkg = "FDEPgetdata")

uninstall.
