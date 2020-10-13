#' Function to import exclusions into R dataframe from the FDEP Oracle Database GWIS
#'
#' @title getdata_lake_exclusions
#'
#' @description Creates lake exclusions dataframe from oracle data pull.
#' User will be prompted for the password to the FDEP Oracle Database GWIS.
#'
#' @param arg1 variable passed into SQL select statement to pull data and name data frame
#'
#' @import RODBC
#' @import RODM
#' @export
#' @examples getdata_lake_exclusions('LL_EXCLUSIONS_2020')
#'    entering 'LL_EXCLUSIONS_2020' for arg1 will produce a dataframe for FDEP Status large
#'    lake site exclusions for 2020 site visits.


getdata_lake_exclusions <- function(arg1) {

  # User will enter the name of the oracle table with project exclusions, e.g. LL_EXCLUSIONS_2020.

  # User will then be promoted for the password for the oracle database GWIS_ADMIN

  channel <- odbcConnect("GWIS_ADMIN",uid="GWIS_ADMIN",pwd=rstudioapi::askForPassword("GWIS Password"))

  # Function will then connect to the oracle table export data and pivot it and create
  #   a dataframe named Exclusions.

  Exclusions <- sqlQuery(channel, paste('select * from', arg1, 'order by pk_random_sample_location'))

  #Need to assign DO criteria to regions
  Exclusions$DO_Conc<- ifelse(Exclusions$SCI_DO_BIOREGION_2012=="BIG BEND",34,
                              ifelse(Exclusions$SCI_DO_BIOREGION_2012=="PANHANDLE", 67,
                                     ifelse(Exclusions$SCI_DO_BIOREGION_2012== "PENINSULA", 38,
                                            ifelse(Exclusions$SCI_DO_BIOREGION_2012=="NORTHEAST", 34,
                                                   ifelse(Exclusions$SCI_DO_BIOREGION_2012=="EVERGLADES", 38,NA)))))

  View(Exclusions)

  Exclusions <<- Exclusions

  write.csv(Exclusions,file = (paste(arg1,'.csv')))

}
