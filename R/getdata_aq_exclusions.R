#' Function to import well evaluation information into R data frame for annual
#'  assessments.
#'
#' @title getdata_aq_exclusions
#'
#' @description Creates unconfined / confined well evaluation data frame from
#'  oracle data pull. User will be prompted for the password to the FDEP
#'  Oracle Database GWIS.
#'
#' @param arg1 variable passed into SQL select statement to pull data and name data frame
#'
#' @import RODBC
#' @import RODM
#' @export
#' @examples getdata_aq_exclusions("'CA18'")
#'    entering "'CA18'" for arg1 will produce a data frame for FDEP Status confined
#'    aquifer wells evaluated in 2018.
#'


getdata_aq_exclusions <- function(arg1) {

  # User will enter the infromation specific to the site evaluations needed for the analysis.  Refer to
  #  example above. -- getdata_aq_exclusions("'CA18'") --

  # User will then be promoted for the password for the oracle database GWIS_ADMIN

  channel <- odbcConnect("GWIS_ADMIN",uid="GWIS_ADMIN",pwd=rstudioapi::askForPassword("GWIS Password"))

  # Function will then connect to the oracle table export data and pivot it and create
  #   a data frame named Exclusions.

  # 11/30/2020 - Modified CSV file naming. Name is now value of arg1 without quotes.
  # Designated underscore as seperator in paste funcitons.
  # For 3 year analysis, portions of arg2 are seperated by underscores (e.g. CN18_CN19_CN20_Results.csv).

  arg3 <- ifelse(str_length(arg1) > 6, paste(substr(arg1, 2, 5), substr(arg1, 9, 12),
                                             substr(arg1, 16, 19), sep = "_"),substr(arg1, 2, 5))


  Exclusions <- sqlQuery(channel, paste("select * from site_evaluations
            where substr(fk_project,3,4) in (",arg1,")
                      order by pk_random_sample_location"))

  View(Exclusions)
  Exclusions <<- Exclusions
  write.csv(Exclusions,file = (paste(arg3,"Sites.csv", sep = "_")))

}
