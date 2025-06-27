#' Function to import lake site evaluation information into R data frame
#'
#' @title getdata_lake_exclusions
#'
#' @description Creates lake site evaluation data frame from oracle data pull.
#' User will be prompted for the password to the FDEP Oracle Database GWIS.
#' Dissolved oxygen (F.A.C. 62-302.533) criteria are added for each record
#' based on the corresponding bioregion.
#'
#' @param arg1 variable passed into SQL select statement to pull data and name CSV file output
#'
#' @import odbc
#' @import rstudioapi
#' @export
#' @examples getdata_lake_exclusions("'LL18'")
#'    entering "'LL18'" for arg1 will produce a data frame for FDEP Status large lakes sampled in 2018.
#'
#'getdata_lake_exclusions("'LL18','LL19','LL20'")
#'    entering "'LL18','LL19','LL20'" for arg1 will produce a data frame for FDEP Status large lakes
#'    sampled 2018 - 2020.
#'

## Revision history
## 11/30/2020 - Modified CSV file naming. Name is now value of arg1 without quotes.
## 02/17/2023 - Modified CSV file naming to support scenarios where arg1 contains
#               more than 3 resource and year identifiers.
#             - Removed row names from CSV file output.
## 08/27/2024 - Changed package for database connection from RODBC to odbc due to switch to 64-bit R.
#             - Updated code for odbc connection and query.
#             - Added warning message for missing inputs.

getdata_lake_exclusions <- function(arg1 = NA) {

  # Warning message for missing inputs.
  if(is.na(arg1)){
    stop('ERROR - Missing input. Please provide the four-character code(s) for the water resource(s) and year(s) to be used in data pull.')
  }

  # Reformat arg1 for use when naming Export file.
  # Designated underscore as separator in paste functions.
  # For multi-year analysis, portions of arg1 are separated by underscores (e.g. LL18_LL19_LL20_Sites.csv).
  arg2 <- gsub("'", "", arg1)
  arg2 <- gsub(",", "_", arg2)

  # User will enter the information specific to the site evaluations needed for the analysis.  Refer to
  #  example above. -- getdata_lake_exclusions("'LL18'") --

  # User will then be promoted for the password for the oracle database GWIS_ADMIN.

  channel64 <- dbConnect(odbc::odbc(),
                            "GWIS_ADMIN_64",
                            UID = "GWIS_ADMIN",
                            PWD = rstudioapi::askForPassword("Enter GWIS Password"),
                            Port = 1523,
                            schema = "GWIS_ADMIN",
                            SVC = "GWIS_ADMIN")

  # Function will then connect to the oracle table export data and create
  #   a data frame named Exclusions.

  Exclusions <- dbGetQuery(channel64, paste("select * from site_evaluations
                                                where substr(fk_project,3,4) in (",arg1,")
                                                order by pk_random_sample_location"))

  # Need to assign DO criteria to regions
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
