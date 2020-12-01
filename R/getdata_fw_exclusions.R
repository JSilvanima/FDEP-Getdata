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
#' @examples getdata_fw_exclusions("'CN18'")
#'    entering "'CA18'" for arg1 will produce a data frame for FDEP Status canals sampled in 2018.
#            getdata_results("'CN18','CN19','CN20'")
#'    entering "'CN18','CN19','CN20'" for arg1 will produce a data frame for FDEP Status canals sampled 2018 - 2020.
#'

getdata_fw_exclusions <- function(arg1) {

  # User will enter the infromation specific to the site evaluations needed for the analysis.  Refer to
  #  example above. -- getdata_fw_exclusions("'CN18'") --

  # User will then be promoted for the password for the oracle database GWIS_ADMIN

  channel <- odbcConnect("GWIS_ADMIN",uid="GWIS_ADMIN",pwd=rstudioapi::askForPassword("GWIS Password"))

  # Function will then connect to the oracle table export data and pivot it and create
  #   a dataframe named Exclusions.

  # 11/30/2020 - Modified CSV file naming. Name is now value of arg1 without quotes.
  # Designated underscore as seperator in paste funcitons.
  # For 3 year analysis, portions of arg2 are seperated by underscores (e.g. CN18_CN19_CN20_Results.csv).

  arg3 <- ifelse(str_length(arg1) > 6, paste(substr(arg1, 2, 5), substr(arg1, 9, 12),
                                             substr(arg1, 16, 19), sep = "_"),substr(arg1, 2, 5))


  Exclusions <- sqlQuery(channel, paste("select * from site_evaluations
            where substr(fk_project,3,4) in (",arg1,")
                      order by pk_random_sample_location"))

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
  write.csv(Exclusions,file = (paste(arg3,"Sites.csv", sep = "_")))

}
