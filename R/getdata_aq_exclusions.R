#' Function to import well evaluation information into R data frame
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
#'    entering "'CA18'" for arg1 will produce a data frame for FDEP Status confined aquifer wells sampled in 2018.
#            getdata_results("'CA18','CA19','CA20'")
#'    entering "'CA18','CA19','CA20'" for arg1 will produce a data frame for FDEP Status confined aquifer wells sampled 2018 - 2020.
#'


getdata_aq_exclusions <- function(arg1) {

  # User will enter the infromation specific to the site evaluations needed for the analysis.  Refer to
  #  example above. -- getdata_aq_exclusions("'CA18'") --

  # User will then be promoted for the password for the oracle database GWIS_ADMIN

  channel <- odbcConnect("GWIS_ADMIN",uid="GWIS_ADMIN",pwd=rstudioapi::askForPassword("GWIS Password"))

  # Function will then connect to the oracle table export data and pivot it and create
  #   a data frame named Exclusions.

  Exclusions <- sqlQuery(channel, paste("select * from site_evaluations
            where substr(fk_project,3,4) in (",arg1,")
                      order by pk_random_sample_location"))

  View(Exclusions)

  Exclusions <<- Exclusions

  write.csv(Exclusions,file = (paste(arg1,'.csv')))

}
