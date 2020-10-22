#' Function to import flowing waters site evaluation information into R data frame
#'
#' @title getdata_fw_exclusions
#'
#' @description Creates flowing waters site evaluation dataframe from oracle data pull.
#' User will be prompted for the password to the FDEP Oracle Database GWIS.
#' Total nitrogen (F.A.C. 62-302.531), total phosphorus (F.A.C. 62-302.531),
#' and dissolved oxygen (F.A.C. 62-302.533) criteria are added for each record
#' based on the corresponding nutrient watershed region and bioregion.
#'
#' @param arg1 variable passed into SQL select statement to pull data and name data frame
#'
#' @import RODBC
#' @import RODM
#' @export
#' @examples getdata_fw_exclusions('CN_EXCLUSIONS_2020')
#'    Entering 'CN_EXCLUSIONS_2020' for arg1 will produce a dataframe of
#'    2020 site evaluation information for FDEP Status Canal site selections.

getdata_fw_exclusions <- function(arg1) {

  # User will enter the name of the oracle table with project exclusions, e.g. CN_EXCLUSIONS_2020.

  # User will then be promoted for the password for the oracle database GWIS_ADMIN

  channel <- odbcConnect("GWIS_ADMIN",uid="GWIS_ADMIN",pwd=rstudioapi::askForPassword("GWIS Password"))

  # Function will then connect to the oracle table export data and pivot it and create
  #   a dataframe named Exclusions.

  Exclusions <- sqlQuery(channel, paste('select * from', arg1, 'order by pk_random_sample_location'))


  #Need to assign NNC & DO criteria to regions
  Exclusions$TN_NNC<- ifelse(Exclusions$NUTRIENT_WATERSHED_REGION=="PANHANDLE EAST",1.03,
                             ifelse(Exclusions$NUTRIENT_WATERSHED_REGION=="PANHANDLE WEST", 0.67,
                                    ifelse(Exclusions$NUTRIENT_WATERSHED_REGION== "PENINSULAR", 1.54,
                                           ifelse(Exclusions$NUTRIENT_WATERSHED_REGION=="NORTH CENTRAL", 1.87,
                                                  ifelse(Exclusions$NUTRIENT_WATERSHED_REGION=="WEST CENTRAL", 1.65,NA)))))

  Exclusions$TP_NNC<- ifelse(Exclusions$NUTRIENT_WATERSHED_REGION=="PANHANDLE EAST",0.18,
                             ifelse(Exclusions$NUTRIENT_WATERSHED_REGION=="PANHANDLE WEST", 0.06,
                                    ifelse(Exclusions$NUTRIENT_WATERSHED_REGION== "PENINSULAR", 0.12,
                                           ifelse(Exclusions$NUTRIENT_WATERSHED_REGION=="NORTH CENTRAL", 0.3,
                                                  ifelse(Exclusions$NUTRIENT_WATERSHED_REGION=="WEST CENTRAL", 0.49,NA)))))

  Exclusions$DO_Conc<- ifelse(Exclusions$SCI_DO_BIOREGION_2012=="BIG BEND",34,
                              ifelse(Exclusions$SCI_DO_BIOREGION_2012=="PANHANDLE", 67,
                                     ifelse(Exclusions$SCI_DO_BIOREGION_2012== "PENINSULA", 38,
                                            ifelse(Exclusions$SCI_DO_BIOREGION_2012=="NORTHEAST", 34,
                                                   ifelse(Exclusions$SCI_DO_BIOREGION_2012=="EVERGLADES", 38,NA)))))

  View(Exclusions)

  Exclusions <<- Exclusions

  write.csv(Exclusions,file = (paste(arg1,'.csv')))

}
