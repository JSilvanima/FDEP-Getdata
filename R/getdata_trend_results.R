#' Function to pull GWIS data for trend network data analyses
#' Create date 02/03/2023
#'
#' @title getdata_trend_results
#'
#' @description Creates dataframe of either surface or groundwater trend network site data via
#'  oracle data pull. User will be prompted for the password to the FDEP Oracle Database GWIS.
#'
#' @param arg1 Variable passed into SQL select statement to indicate water resources
#' for data pull.
#' @param arg2 Variable passed into SQL select statement to indicate start date for data pull.
#'
#' @import RODBC
#' @import sqldf
#' @import tidyr
#' @import dplyr
#' @import splitstackshape
#' @import stringr
#'
#' @export
#' @examples getdata_trend_results("'AQUIFER','SPRING'","'01-OCT-1998'")
#'    Entering "'AQUIFER','SPRING'","'01-OCT-1998'" for arg1, arg2
#'    will produce a data frame for all FDEP Trend Network ground water stations
#'    for data collect from 1998 to current, 2) a dataframe of those data which
#'    are duplicated in the GWIS database. That is IF there is more than one value for
#'    each station, collection date, parameter combination, and 3) a dataframe of the
#'    surface water trend data which excludes the duplicates.
#'
#'getdata_trend_results("'CANAL','SPRING RUN','STREAM'","'01-OCT-1998'")
#'    Entering getdata_trend_results("'CANAL','SPRING RUN','STREAM'","'01-OCT-1998'") will produce
#'    three dataframes, similar to the above for all FDEP Trend Network surface water stations.
#'
#'getdata_trend_results("'CANAL'","'01-OCT-1998'")
#'    Entering getdata_trend_results("'CANAL'","'01-OCT-1998'") will produce three
#'    dataframes, similar to the above for only FDEP Trend Network canal stations.
#'

getdata_trend_results <- function(arg1,arg2) {

  # Create odbc connection run data pull script

  channel <- odbcConnect("GWIS_ADMIN",uid="GWIS_ADMIN",pwd=rstudioapi::askForPassword("GWIS Password"))

  # User will enter the information specific to the results needed for the analysis.  Refer to
  #  example above. -- getdata_trend_results("'AQUIFER','SPRING'","''01-OCT-1998'") --
  # User will then be promoted for the password for the oracle database GWIS_ADMIN
  # A SQL query is then run to pull, pivot, and and create the data frame Results.

  Trend_All_Data<-sqlQuery(channel,paste("select pk_station, pk_result, collection_date, sample_type, t_sample.matrix,
    replace(replace(replace(replace(replace(replace(replace(replace(replace(parameter,
    chr(39),''),chr(44),''),chr(43),''),chr(45),'_'),chr(32),'_'),chr(40),''),chr(41),''),
    chr(91),''),chr(93),'') parameter,value, value_qualifier
    from t_station, t_sample,t_parameter,t_result
       where (pk_station = fk_station and pk_sample = fk_sample and pk_param_code = fk_param_code)
        and t_station.waterbody_type in (",arg1,")
        and t_sample.sample_type = 'PRIMARY'
        and t_sample.collection_date > ",arg2,"
        and t_result.value is not null
        and fk_param_code <> 99982
        and SAMPLED_TV_STATIONS = 'A'
  Union
    Select pk_station, pk_result, collection_date, sample_type, t_sample.matrix,
    replace(replace(replace(replace(replace(replace(replace(replace(replace(parameter,
    chr(39),''),chr(44),''),chr(43),''),chr(45),'_'),chr(32),'_'),chr(40),''),chr(41),''),
    chr(91),''),chr(93),'') parameter,value, value_qualifier
    from t_station, t_sample,t_parameter,t_result
       where (pk_station = fk_station and pk_sample = fk_sample and pk_param_code = fk_param_code)
        and t_station.waterbody_type in (",arg1,")
        and t_sample.sample_type = 'PRIMARY'
        and t_sample.collection_date > ",arg2,"
        and t_result.value is not null
        and fk_param_code <> 99982
        and pk_station in (3506,3561,3570)"))

  # Replace measurement values with 'NA' for those measurements having any of
  #   the following fatal data qualifiers: '?,O,N,T,X'.
  # Definitions for these codes may be found in FS 62-160.700 Table 1 (Data Qualifier Codes)
  #  link provided here https://www.flrules.org/gateway/RuleNo.asp?title=QUALITY%20ASSURANCE&ID=62-160.700.
  Trend_All_Data$VALUE <- ifelse(grepl('?', Trend_All_Data$VALUE_QUALIFIER, fixed=TRUE), NA, Trend_All_Data$VALUE)
  Trend_All_Data$VALUE <- ifelse(grepl('O', Trend_All_Data$VALUE_QUALIFIER, fixed=TRUE), NA, Trend_All_Data$VALUE)
  Trend_All_Data$VALUE <- ifelse(grepl('N', Trend_All_Data$VALUE_QUALIFIER, fixed=TRUE), NA, Trend_All_Data$VALUE)
  Trend_All_Data$VALUE <- ifelse(grepl('T', Trend_All_Data$VALUE_QUALIFIER, fixed=TRUE), NA, Trend_All_Data$VALUE)
  Trend_All_Data$VALUE <- ifelse(grepl('X', Trend_All_Data$VALUE_QUALIFIER, fixed=TRUE), NA, Trend_All_Data$VALUE)

  # Export all data retrieved, before removing duplicates.
  Trend_All_Data <<- Trend_All_Data

  # Reformat information in arg1 and arg2 for use in CSV file names.
  arg3 <- gsub("'", "", arg1)
  arg3 <- gsub(",", "_", arg3)
  arg4 <- gsub("'", "", arg2)

  # Export Trend_All_Data.
  write.csv(Trend_All_Data, file = (paste(arg3,arg4,'Trend_All_Data.csv',sep = "_")), row.names = FALSE)

  duplicates <- sqldf('select * from Trend_All_Data
                    where PK_STATION||COLLECTION_DATE||PARAMETER
                    IN (SELECT PK_STATION||COLLECTION_DATE||PARAMETER FROM Trend_All_Data
                        GROUP BY PK_STATION||COLLECTION_DATE||PARAMETER
                        HAVING COUNT(PK_STATION||COLLECTION_DATE||PARAMETER) > 1)
                    order by PK_STATION,COLLECTION_DATE,PARAMETER')
  duplicates <<- duplicates

  # Export duplicates for GWIS corrections.
  write.csv(duplicates, file = (paste(arg3,arg4,'DUPLICATES.csv',sep = "_")), row.names = FALSE)

  # Remove duplicates from Trend_All_Data dataframe
  Results <- sqldf('select * from Trend_All_Data
                    where PK_RESULT not in (select PK_RESULT
                        FROM duplicates)')
  Results_Stacked <<- Results

  # Export data after duplicates have been removed.
  write.csv(Results, file = (paste(arg3,arg4,'Results_Stacked.csv',sep = "_")), row.names = FALSE)

  ##Create new column with value & qualifier concatenated and seperated by a pipe symbol.
  Results$VALUE_VALUE_QUALIFIER<-paste(Results$VALUE,"|",Results$VALUE_QUALIFIER)

  #Now run pivot function on new dataframe

  ##Pivot the results. Should return one column per param with "value|VQ".
  Results_PIVOT<-pivot_wider(Results,id_cols = c(PK_STATION,COLLECTION_DATE,SAMPLE_TYPE,MATRIX),names_from = PARAMETER,values_from = VALUE_VALUE_QUALIFIER, values_fill = NULL, values_fn = NULL)

  ##If the following error is received there are more duplicated results
  #  Warning message:Values from `VALUE_VALUE_QUALIFIER` are not uniquely
  #  identified; output will contain list-cols.
  #  Use `values_fn = list` to suppress this warning.
  #  Use `values_fn = {summary_fun}` to summarise duplicates.
  #  Use the following dplyr code to identify duplicates.
  #  {data} %>%
  #  dplyr::group_by(PK_STATION, COLLECTION_DATE, SAMPLE_TYPE, MATRIX, PARAMETER) %>%
  #  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  #  dplyr::filter(n > 1L)
  #  Search the result for 'c(' to id the duplicates and export them
  #  so that they may be corrected in GWIS.

  ##Create variable to store number of columns.
  ColCount <- ncol(Results_PIVOT)
  ColCount
  ##Split column 5 through end. Result is two columns for each parameter, value followed by VQ.
  Results_PIVOT_SPLIT  <- cSplit(Results_PIVOT, splitCols = 5:ColCount, "|")
  ##Rename columns with value data by removing the "_1" suffix.
  ##11/30/2020 - Replaced soft deprecated function rename_at with gsub.
  ##02/17/2023 - Updated pattern matching in gsub statement to better accommodate analyte names that include numbers.
  names(Results_PIVOT_SPLIT) <- gsub('_1$','',names(Results_PIVOT_SPLIT))
  ##Rename columns with VQ data by changing the "_2" suffix to "_VQ".
  ##11/30/2020 - Replaced soft deprecated function rename_at with gsub.
  ##02/17/2023 - Updated pattern matching in gsub statement to better accommodate analyte names that include numbers.
  names(Results_PIVOT_SPLIT) <- gsub('_2$','_VQ',names(Results_PIVOT_SPLIT))
  ##11/30/2020 - Replace period in column names with single underscore.
  names(Results_PIVOT_SPLIT) <- gsub(".","_",names(Results_PIVOT_SPLIT), fixed=TRUE)
  ##11/30/2020 - Replace double underscore in column names with single underscore.
  names(Results_PIVOT_SPLIT) <- gsub("__","_",names(Results_PIVOT_SPLIT), fixed=TRUE)
  ##Rename Results_PIVOT_SPLIT to Results_Pivoted
  Results_Pivoted <- Results_PIVOT_SPLIT
  Results_Pivoted <<- Results_Pivoted
  View(Results_Pivoted)

  # Export data after duplicates have been removed and data have been pivoted.
  write.csv(Results_Pivoted,file = (paste(arg3,arg4,'Results_Pivoted.csv',sep = "_")), row.names = FALSE)
}
