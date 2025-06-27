#' Function to create Exclusions, Site Evaluations and well removals data frames for 3 year combined analyses.
#' Create date 01/25/2023
#'
#' @title getdata_aq_exclusions_multi_yr
#'
#' @description Creates 3 data frames of unconfined / confined well evaluation data via
#'  oracle data pull. User will be prompted for the password to the FDEP Oracle Database GWIS.
#'    1) A data frame (Exclusions) containing all well evaluations for the period of record.
#'    2) A data frame (well_removals) containing all wells evaluated during the period of
#'       record which are no longer included in the target population. SQL script compares
#'       the primary key for the most recent well list frame to those found in the site
#'       evaluation data pull for the three year period.
#'    3) A data frame (SiteEvaluations) containing the wells which were evaluated and are
#'       present in the well list frame for the most recent year in the period of record.
#'
#' @param arg1 Variable passed into SQL select statement to indicate which year's target population
#' to be used for SQL statement.
#' @param arg2 Variable passed into SQL select statement to indicate which years' site
#' evaluation data to use for comparison to most recent well list frame.
#' @param arg3 Variable passed into SQL select statement to indicate the
#' resource (CONFINED AQUIFER or UNCONFINED AQUIFER) of the target population.
#'
#' @import odbc
#' @import sqldf
#' @import rstudioapi
#' @export
#' @examples getdata_aq_exclusions_multi_yr("'2020'","'CA18','CA19','CA20'","'CONFINED AQUIFER'")
#'    entering "'2020'","'CA18','CA19','CA20'","'CONFINED AQUIFER'" for arg1, arg2, arg3
#'    will produce 1) a data frame for FDEP Status confined aquifer wells
#'    evaluated during 2018-2020, 2) a data frame of those wells which no longer
#'    are in the target population for the year specified in arg1, and 3) a data frame of the wells
#'    which were evaluated during 2018-2020 and are present in the well list frame for the
#'    year specified in arg1.
#'
#'getdata_aq_exclusions_multi_yr("'2020'","'CA09','CA10','CA11','CA18','CA19','CA20'","'CONFINED AQUIFER'")
#'    entering "'2020'","'CA09','CA10','CA11','CA18','CA19','CA20'","'CONFINED AQUIFER'" for arg1, arg2, arg3
#'    will produce 1) a data frame for FDEP Status confined aquifer wells
#'    evaluated during 2009-2011 and 2018-2020, 2) a data frame of those wells which no
#'    longer are in the target population for the year specified in arg1, and 3) a data frame of the wells
#'    which were evaluated and are present in the well list frame for the
#'    for the year specified in arg1.

## Revision history
## 11/30/2020 - Modified CSV file naming. Name is now value of arg1 without quotes.
## 04/25/2021 - Modified data pull because of database table consolidation of
#               annual well listframe tables into one table well_listframe.
## 11/29/2021 - Modified data pull to accommodate wells determined to be
#               wrong resource type.Required addition of third user-supplied argument, 'arg3'.
## 01/25/2023 - Modified data pull to prevent removal of wells from the year
#               with the target population, even if these wells were excluded as well taps
#               wrong resource.
## 02/17/2023 - Modified CSV file naming to support scenarios where arg1 contains
#               more than 3 resource and year identifiers.
#             - Removed row names from CSV file output.
## 08/27/2024 - Changed package for database connection from RODBC to odbc due to switch to 64-bit R.
#             - Updated code for odbc connection and query.
#             - Added warning message for missing inputs.

getdata_aq_exclusions_multi_yr <- function(arg1 = NA, arg2 = NA, arg3 = NA) {

  # Warning message for missing inputs.
  if(is.na(arg1) | is.na(arg2) | is.na(arg3)){
    stop('ERROR - Missing input. Please supply three inputs: 1) target population year, 2) four-character code(s) for the water resource(s) and year(s), 3) target population water resource.')
  }

  # Reformat arg1 for use when naming Export files.
  # Designated underscore as separator in paste functions.
  # Portions of arg2 are separated by underscores (e.g. CA18_CA19_CA20_Sites.csv).
  arg4 <- gsub("'", "", arg2)
  arg4 <- gsub(",", "_", arg4)

  # User will enter the information specific to the site evaluations needed for the analysis.  Refer to
  #  example above. -- getdata_aq_exclusions_multi_yr('2020',"'CA18','CA19','CA20'","'CONFINED AQUIFER'") --

  # User will then be promoted for the password for the oracle database GWIS_ADMIN.

  channel64 <- dbConnect(odbc::odbc(),
                       "GWIS_ADMIN_64",
                       UID = "GWIS_ADMIN",
                       PWD = rstudioapi::askForPassword("Enter GWIS Password"),
                       Port = 1523,
                       schema = "GWIS_ADMIN",
                       SVC = "GWIS_ADMIN")

  # Function connects to the oracle table, export data, and creates
  #   three data frames: Exclusions, well_removals, SiteEvaluations.

  # Create Exclusions data frame.
  Exclusions <- dbGetQuery(channel64,
                           paste("select * from site_evaluations
                                  where substr(fk_project,3,4) in (",arg2,")
                                  order by pk_random_sample_location"))

  View(Exclusions)

  # Export Exclusions
  Exclusions <<- Exclusions
  write.csv(Exclusions,file = (paste(arg4,"Sites.csv", sep = "_")), row.names = FALSE)

  # Create well_removals data frame.
  well_removals <- dbGetQuery(channel64,
                              paste("select * from site_evaluations
                                    where substr(site_evaluations.fk_project,3,4) in (",arg2,")
                                    and fk_well_listframe_id not in (select distinct(fl_id) from well_listframe
                                                                    where listframe_year = ",arg1,"
                                                                    and water_resource = ", arg3, ")"))

  View(well_removals)

  # Export well_removals
  well_removals <<- well_removals
  write.csv(well_removals,file = (paste(arg4,'well_removals.csv', sep = "_")), row.names = FALSE)

  # Create SiteEvaluations - all records from Exclusions that to not match PK_RANDOM_SAMPLE_LOCATION
  # of records in well_removals.
  SiteEvaluations <- sqldf('select * from Exclusions
                          where PK_RANDOM_SAMPLE_LOCATION not in (select PK_RANDOM_SAMPLE_LOCATION from well_removals)')
  View(SiteEvaluations)

  # Export SiteEvaluations
  SiteEvaluations <<- SiteEvaluations
  write.csv(SiteEvaluations,file = (paste(arg4,"SiteEvaluations.csv", sep = "_")), row.names = FALSE)
}
