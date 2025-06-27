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
#' @param arg1 variable passed into SQL select statement to pull data and name CSV file output
#'
#' @import odbc
#' @import rstudioapi
#' @export
#' @examples getdata_fw_exclusions("'CN18'")
#'    entering "'CN18'" for arg1 will produce a data frame for FDEP Status canals sampled in 2018.
#'
#'getdata_fw_exclusions("'CN18','CN19','CN20'")
#'    entering "'CN18','CN19','CN20'" for arg1 will produce a data frame for FDEP
#'    Status canals sampled 2018 - 2020.
#'

## Revision history
## 11/30/2020 - Modified CSV file naming. Name is now value of arg1 without quotes.
## 02/17/2023 - Modified CSV file naming to support scenarios where arg1 contains
#               more than 3 resource and year identifiers.
#             - Removed row names from CSV file output.
## 08/27/2024 - Changed package for database connection from RODBC to odbc due to switch to 64-bit R.
#             - Updated code for odbc connection and query.
#             - Added warning message for missing inputs.

getdata_fw_exclusions <- function(arg1 = NA) {

  # Warning message for missing inputs.
  if(is.na(arg1)){
    stop('ERROR - Missing input. Please provide the four-character code(s) for the water resource(s) and year(s) to be used in data pull.')
  }

  # Reformat arg1 for use when naming Export file.
  # Designated underscore as separator in paste functions.
  # For multi-year analysis, portions of arg1 are separated by underscores (e.g. CN18_CN19_CN20_Sites.csv).
  arg2 <- gsub("'", "", arg1)
  arg2 <- gsub(",", "_", arg2)

  # User will enter the information specific to the site evaluations needed for the analysis.  Refer to
  #  example above. -- getdata_fw_exclusions("'CN18'") --

  # User will then be promoted for the password for the oracle database GWIS_ADMIN

  channel64 <- dbConnect(odbc::odbc(),
                            "GWIS_ADMIN_64",
                            UID = "GWIS_ADMIN",
                            PWD = rstudioapi::askForPassword("Enter GWIS Password"),
                            Port = 1523,
                            schema = "GWIS_ADMIN",
                            SVC = "GWIS_ADMIN")

  # Function will then connect to the oracle table export data and create
  #   a dataframe named Exclusions.

  Exclusions <- dbGetQuery(channel64, paste("select * from site_evaluations
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

  # Export Exclusions
  Exclusions <<- Exclusions
  write.csv(Exclusions,file = (paste(arg2,"Sites.csv", sep = "_")), row.names = FALSE)
}
