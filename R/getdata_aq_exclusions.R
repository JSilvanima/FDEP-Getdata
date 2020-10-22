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
#' @examples getdata_aq_exclusions('CA_EXCLUSIONS_2020')
#'    Entering 'CA_EXCLUSIONS_2020' for arg1 will produce a data frame
#'    containing 2020 site evaluation information for FDEP Status confined
#'    well selections.


getdata_aq_exclusions <- function(arg1) {

  # User will enter the name of the oracle table with project exclusions, e.g. CA_EXCLUSIONS_2020.

  # User will then be promoted for the password for the oracle database GWIS_ADMIN

  channel <- odbcConnect("GWIS_ADMIN",uid="GWIS_ADMIN",pwd=rstudioapi::askForPassword("GWIS Password"))

  # Function will then connect to the oracle table export data and pivot it and create
  #   a data frame named Exclusions.

  Exclusions <- sqlQuery(channel, paste('select * from', arg1, 'order by pk_random_sample_location'))

  View(Exclusions)

  Exclusions <<- Exclusions

  write.csv(Exclusions,file = (paste(arg1,'.csv')))

}
