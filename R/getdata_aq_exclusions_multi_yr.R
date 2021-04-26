#' Function to create Exclusions, Site Evaluations and well removals data frames for 3 year combined analyses.
#' Create date 04/26/2021
#'
#' @title getdata_aq_exclusions_multi_yr
#'
#' @description Creates 3 data frames of unconfined / confined well evaluation data via
#'  oracle data pull. User will be prompted for the password to the FDEP Oracle Database GWIS.
#'    1) A data frame (Exclusions) containing all well evaluations for the period of record.
#'    2) A data frame (well_removals) containing all wells evaluated period of record which are no
#'       longer included in the traget population. SQL script compares the primary key
#'       for the most recent well list frame to those found in the site evaluation data
#'       pull for the three year period.
#'    3) A data frame (SiteEvaluations) containing the wells which were evaluated and are
#'       present in the well list frame for the most recent year in the period of record.
#'  User will be prompted for the password to the FDEP Oracle Database GWIS.
#'
#' @param arg1 Variable passed into SQL select statement to indicate which year's target population
#' to be used for SQL statement.
#' @param arg2 Variable passed into SQL select statement to indicate which years' site
#' evaluation date to used for comparision to most recent well list frame.
#'
#' @import RODBC
#' @import RODM
#' @import sqldf
#' @export
#' @examples getdata_aq_exclusions_3yr("'2020',"'CA18','CA19','CA20'")
#'    entering "'well_listframe_2020',"'CA18','CA19','CA20'" for arg1, arg2
#'    will produce 1) a data frame for FDEP Status confined aquifer wells
#'    evaluated during 2018-2020, 2) a data frame of those wells which no
#'    longer are in the target population, and 3) a data frame of the wells
#'    which were evaluated and are present in the well list frame for the
#'    most recent year in the period of record.
#'
#'    getdata_aq_exclusions_3yr("'2020'","'CA09','CA10','CA11','CA18','CA19','CA20'")
#'    entering "'2020'","'CA09','CA10','CA11','CA18','CA19','CA20'" for arg1, arg2
#'    will produce 1) a data frame for FDEP Status confined aquifer wells
#'    evaluated during 2009-2011 and 2018-2020, 2) a data frame of those wells which no
#'    longer are in the target populatiomn, and 3) a data frame of the wells
#'    which were evaluated and are present in the well list frame for the
#'    most recent year in the period of record.

getdata_aq_exclusions_multi_yr <- function(arg1,arg2) {

  # User will enter the infromation specific to the site evaluations needed for the analysis.  Refer to
  #  example above. -- well_removals('2020',"'CA18','CA19','CA20'") --

  # User will then be promoted for the password for the oracle database GWIS_ADMIN

  channel <- odbcConnect("GWIS_ADMIN",uid="GWIS_ADMIN",pwd=rstudioapi::askForPassword("GWIS Password"))

  # Function connects to the oracle table, export data, and creates
  #   three data frames: Exclusions, well_removals, SiteEvaluations.
  # It also creates a third variable, arg3, for naming file exports

  # 11/30/2020 - Modified CSV file naming. Name is now value of arg2 without quotes.
  # Designated underscore as seperator in paste funcitons.
  # For 3 year analysis, portions of arg2 are seperated by underscores (e.g. CN18_CN19_CN20_Results.csv).

  arg3 <- ifelse(str_length(arg2) > 6, paste(substr(arg2, 2, 5), substr(arg2, 9, 12),
                                             substr(arg2, 16, 19), sep = "_"),substr(arg2, 2, 5))

  Exclusions <- sqlQuery(channel, paste("select * from site_evaluations
            where substr(fk_project,3,4) in (",arg2,")
                      order by pk_random_sample_location"))

  View(Exclusions)
  Exclusions <<- Exclusions
  write.csv(Exclusions,file = (paste(arg3,"Sites.csv", sep = "_")))

  # 04/25/2021 - Modified select pull because of database table consolidation of
  #  annual well listframe tables into one table well_listframe.

  well_removals <- sqlQuery(channel,paste("select * from site_evaluations
          where substr(site_evaluations.fk_project,3,4) in (",arg2,")
          and fk_well_listframe_id not in (select distinct(fl_id) from well_listframe
          where listframe_year = ",arg1,")
          order by Pk_random_sample_location"))


  View(well_removals)
  well_removals <<- well_removals
  write.csv(well_removals,file = (paste(arg3,'well_removals.csv', sep = "_")))

  SiteEvaluations <- sqldf('select * from Exclusions
          where PK_RANDOM_SAMPLE_LOCATION
                         not in (select PK_RANDOM_SAMPLE_LOCATION
                          from well_removals)')
  View(SiteEvaluations)
  SiteEvaluations <<- SiteEvaluations

  write.csv(SiteEvaluations,file = (paste(arg3,"SiteEvaluations.csv", sep = "_")))

}
