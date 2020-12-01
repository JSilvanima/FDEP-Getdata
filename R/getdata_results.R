#' Function to import results into R data frame from the FDEP Oracle Database GWIS
#'
#' @title getdata_results
#'
#' @description Creates results data frame from oracle data pull.
#' User will be prompted for the password to the FDEP Oracle Database GWIS.
#' Replaces measurement values with 'NA' for those measurements having any of
#'  the following fatal data qualifiers: '?,O,N,T,X'.
#' Replaces measurement values with 'NA' for those coliform measurements which
#'  have reported values above the criteria (2 for fecal, 4 for total) and are
#'  listed as below detection ('U' qualifier).
#' Definitions for these codes may be found in FS 62-160.700 Table 1 (Data Qualifier Codes).
#'
#'
#' @param arg2 variable passed into SQL select statement to pull data and name data frame
#' @import RODBC
#' @import RODM
#' @import dplyr
#' @import tidyr
#' @import splitstackshape
#' @import stringr
#' @export
#' @examples getdata_results("'CN18'")
#'    entering "'CN18'" for arg2 will produce a data frame for FDEP Status Canals sampled in 2018.
#'getdata_results("'CN18','CN19','CN20'")
#'    entering "'CN18','CN19','CN20'" for arg2 will produce a data frame for FDEP Status Canals sampled 2018 - 2020.
#'

getdata_results <- function(arg2) {

  # Create odbc connection run data pull script

  channel <- odbcConnect("GWIS_ADMIN",uid="GWIS_ADMIN",pwd=rstudioapi::askForPassword("GWIS Password"))

  # User will enter the infromation specific to the results needed for the analysis.  REefer to
  #  example above. -- getdata_results("'CN18'") --

  # User will then be promoted for the password for the oracle database GWIS_ADMIN

  # A SQL query is then run to pull, pivot, and and create the data frame Results.
  ##11/30/2020 - Changed character replacement from underscore to period. This was needed to make pivot
  # and split work correctly for column names that included numbers (e.g. PCB 1260).
  # Periods will be replaced by underscores after pivot and split is complete.
  Results<-sqlQuery(channel,paste("select fk_station, fk_random_sample_location, collection_date, sample_type, s.matrix,
    replace(replace(replace(replace(replace(replace(replace(replace(replace(parameter,
    chr(39),''),chr(44),''),chr(43),''),chr(45),'.'),chr(32),'.'),chr(40),''),chr(41),''),
    chr(91),''),chr(93),'') parameter,value, value_qualifier
        from t_sample s,t_parameter p,t_result r
              where pk_sample = fk_sample and pk_param_code = fk_param_code
              and fk_param_code <> 99982 and substr(fk_project,3,4) in (",arg2,")
                                  order by fk_random_sample_location"))

  # Replace measurement values with 'NA' for those measurements having any of
  #   the following fatal data qualifiers: '?,O,N,T,X'.
  # Definitions for these codes may be found in FS 62-160.700 Table 1 (Data Qualifier Codes)
  #  link provided here https://www.flrules.org/gateway/RuleNo.asp?title=QUALITY%20ASSURANCE&ID=62-160.700.

  Results$VALUE <- ifelse(grepl('?', Results$VALUE_QUALIFIER, fixed=TRUE), NA, Results$VALUE)
  Results$VALUE <- ifelse(grepl('O', Results$VALUE_QUALIFIER, fixed=TRUE), NA, Results$VALUE)
  Results$VALUE <- ifelse(grepl('N', Results$VALUE_QUALIFIER, fixed=TRUE), NA, Results$VALUE)
  Results$VALUE <- ifelse(grepl('T', Results$VALUE_QUALIFIER, fixed=TRUE), NA, Results$VALUE)
  Results$VALUE <- ifelse(grepl('X', Results$VALUE_QUALIFIER, fixed=TRUE), NA, Results$VALUE)


  ##Create new column with value & qualifier concatenated and seperated by a pipe symbol.
  Results$VALUE_VALUE_QUALIFIER<-paste(Results$VALUE,"|",Results$VALUE_QUALIFIER)

  ##Pivot the results. Should return one column per param with "value|VQ".
  Results_PIVOT<-pivot_wider(Results,id_cols = c(FK_STATION,FK_RANDOM_SAMPLE_LOCATION, COLLECTION_DATE,SAMPLE_TYPE,MATRIX),names_from = PARAMETER,values_from = VALUE_VALUE_QUALIFIER, values_fill = NULL, values_fn = NULL)

  ##Create variable to store number of columns.
  ColCount <- ncol(Results_PIVOT)
  ColCount

  ##Split column 6 through end. Result is two columns for each parameter, value followed by VQ.
  Results_PIVOT_SPLIT  <- cSplit(Results_PIVOT, splitCols = 6:ColCount, "|")

  ##Rename columns with value data by removing the "_1" suffix.
  ##11/30/2020 - Replaced soft depricated function rename_at with gsub.
  names(Results_PIVOT_SPLIT) <- gsub("_1","",names(Results_PIVOT_SPLIT), fixed=TRUE)

  ##Rename columns with VQ data by changing the "_2" suffix to "_VQ".
  ##11/30/2020 - Replaced soft depricated function rename_at with gsub.
  names(Results_PIVOT_SPLIT) <- gsub("_2","_VQ",names(Results_PIVOT_SPLIT), fixed=TRUE)

  ##11/30/2020 - Replace period in column names with single underscore.
  names(Results_PIVOT_SPLIT) <- gsub(".","_",names(Results_PIVOT_SPLIT), fixed=TRUE)

  ##11/30/2020 - Replace double underscore in column names with single underscore.
  names(Results_PIVOT_SPLIT) <- gsub("__","_",names(Results_PIVOT_SPLIT), fixed=TRUE)

  ##Can then rename resulting data table to someting shorter if desired. For example:
  Results <- Results_PIVOT_SPLIT

  Results <<- Results
  View(Results)

  ##11/30/2020 - Modified CSV file naming. Name is now value of arg2 without quotes.
  # Designated underscore as seperator in paste funcitons.
  # For 3 year analysis, portions of arg2 are seperated by underscores (e.g. CN18_CN19_CN20_Results.csv).
  arg3 <- ifelse(str_length(arg2) > 6, paste(substr(arg2, 2, 5), substr(arg2, 9, 12),
         substr(arg2, 16, 19), sep = "_"),substr(arg2, 2, 5))

  write.csv(Results,file = (paste(arg3,"Results.csv", sep = "_")))
}
