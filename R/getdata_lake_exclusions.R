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
#' @import RODBC
#' @export
#' @examples getdata_lake_exclusions("'LL18'")
#'    entering "'LL18'" for arg1 will produce a data frame for FDEP Status large lakes sampled in 2018.
#'
#'getdata_lake_exclusions("'LL18','LL19','LL20'")
#'    entering "'LL18','LL19','LL20'" for arg1 will produce a data frame for FDEP Status large lakes
#'    sampled 2018 - 2020.
#'


getdata_lake_exclusions <- function(arg1) {

  # User will enter the information specific to the site evaluations needed for the analysis.  Refer to
  #  example above. -- getdata_lake_exclusions("'LL18'") --

  # User will then be promoted for the password for the oracle database GWIS_ADMIN

  channel <- odbcConnect("GWIS_ADMIN",uid="GWIS_ADMIN",pwd=rstudioapi::askForPassword("GWIS Password"))

  # Function will then connect to the oracle table export data and create
  #   a data frame named Exclusions.

  # 11/30/2020 - Modified CSV file naming. Name is now value of arg1 without quotes.
  # Designated underscore as separator in paste functions.
  # For multi-year analysis, portions of arg1 are separated by underscores (e.g. LL18_LL19_LL20_Sites.csv).
  # 02/17/2023 - Modified CSV file naming to support scenarios where arg1 contains
  #              more than 3 resource and year identifiers.
  #              Removed row names from CSV file output.

  arg2 <- gsub("'", "", arg1)
  arg2 <- gsub(",", "_", arg2)


  Exclusions <- sqlQuery(channel, paste("select * from site_evaluations
            where substr(fk_project,3,4) in (",arg1,")
                      order by pk_random_sample_location"))

  #Need to assign DO criteria to regions
  Exclusions$DO_Conc<- ifelse(Exclusions$SCI_DO_BIOREGION_2012=="BIG BEND",34,
                              ifelse(Exclusions$SCI_DO_BIOREGION_2012=="PANHANDLE", 67,
                                     ifelse(Exclusions$SCI_DO_BIOREGION_2012== "PENINSULA", 38,
                                            ifelse(Exclusions$SCI_DO_BIOREGION_2012=="NORTHEAST", 34,
                                                   ifelse(Exclusions$SCI_DO_BIOREGION_2012=="EVERGLADES", 38,NA)))))

  View(Exclusions)
  Exclusions <<- Exclusions
  write.csv(Exclusions,file = (paste(arg2,"Sites.csv", sep = "_")), row.names = FALSE)

}
