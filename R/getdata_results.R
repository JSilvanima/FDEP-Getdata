#' Function to import results into R dataframe from the FDEP Oracle Database GWIS
#'
#' @title getdata_results
#'
#' @description Creates flowing water results dataframe from oracle data pull.
#' User will be prompted for the password to the FDEP Oracle Database GWIS.
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
#'    entering "'CN18'" for arg2 will produce a dataframe for FDEP Status Canals sampled in 2018.
#' getdata_results("'CN18','CN19','CN20'")
#'    entering "'CN18','CN19','CN20'" for arg2 will produce a dataframe for FDEP Status Canals sampled 2018 - 2020.
#'

getdata_results <- function(arg2) {

  # Create odbc connection run data pull script

  channel <- odbcConnect("GWIS_ADMIN",uid="GWIS_ADMIN",pwd=rstudioapi::askForPassword("GWIS Password"))

  # User will enter the infromation specific to the results needed for the analysis.  REefer to
  #  example above. -- getdata_results("'CN18'") --

  # User will then be promoted for the password for the oracle database GWIS_ADMIN

  # A SQL query is then run to pull, pivot, and and create the dataframe Results.

  Results<-sqlQuery(channel,paste("select fk_station, fk_random_sample_location, collection_date, sample_type, s.matrix,
    replace(replace(replace(replace(replace(replace(replace(replace(replace(parameter,
    chr(39),''),chr(44),''),chr(43),''),chr(45),'_'),chr(32),'_'),chr(40),''),chr(41),''),
    chr(91),''),chr(93),'') parameter,value, value_qualifier
        from t_sample s,t_parameter p,t_result r
              where pk_sample = fk_sample and pk_param_code = fk_param_code
              and fk_param_code <> 99982 and substr(fk_project,3,4) in (",arg2,")"))

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
  ##Returns a warning that this function has been soft depricated - will need ot be recoded using suggested method in the future.
  Results_PIVOT_SPLIT_RENAME_V <- Results_PIVOT_SPLIT %>% rename_at(vars(ends_with("_1")),
                                                                    funs(str_replace(., "_1", "")))
  str(Results_PIVOT_SPLIT_RENAME_V)

  ##Rename columns with VQ data by changing the "_2" suffix to "_VQ".
  ##Note this uses the same soft depricated function - will need to be recoded in the future.
  Results_PIVOT_SPLIT_RENAME_V_VQ <- Results_PIVOT_SPLIT_RENAME_V %>% rename_at(vars(ends_with("_2")),
                                                                                funs(str_replace(., "_2", "_VQ")))
  str(Results_PIVOT_SPLIT_RENAME_V_VQ)

  ##9/3/2020 - Replace double underscore in column names with single underscore
  Results_PIVOT_SPLIT_RENAME_V_VQ2 <- Results_PIVOT_SPLIT_RENAME_V_VQ %>% rename_at(vars(contains("__")),
                                                                                    funs(str_replace(., "__", "_")))
  str(Results_PIVOT_SPLIT_RENAME_V_VQ2)

  ##Can then rename resulting data table to someting shorter if desired. For example:
  Results <- Results_PIVOT_SPLIT_RENAME_V_VQ2


  Results <<- Results
  View(Results)

  write.csv(Results, "Results.csv")
}



