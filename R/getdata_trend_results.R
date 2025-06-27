#' Function to pull GWIS data for trend network data analyses.
#' Create date 02/03/2023.
#'
#' @title getdata_trend_results
#'
#' @description Creates data frames of either surface or groundwater trend network results data via
#'  oracle data pull. User will be prompted for the password to the FDEP Oracle Database GWIS.
#'  Replaces measurement values with 'NA' for those measurements having any of
#'  the following fatal data qualifiers: '?,O,N,T,X'.
#'  Definitions for these codes may be found in FS 62-160.700 Table 1 (Data Qualifier Codes).
#'
#' @param arg1 Variable passed into SQL select statement to indicate water resources
#' for data pull.
#' @param arg2 Variable passed into SQL select statement to indicate start date for data pull.
#' @param arg3 Variable passed into SQL select statement to indicate end date for data pull.
#'
#' @import odbc
#' @import sqldf
#' @import tidyr
#' @import dplyr
#' @import splitstackshape
#' @import stringr
#' @import rstudioapi
#' @export
#' @examples getdata_trend_results("'AQUIFER','SPRING'","'01-OCT-1998'","'31-DEC-2022'")
#'    Entering "'AQUIFER','SPRING'","'01-OCT-1998'","'31-DEC-2022'" for arg1, arg2, arg3
#'    will produce four data frames:
#'    1) A data frame named "Trend_All_Data" for all FDEP Trend Network groundwater stations
#'    for data collected from October 1, 1998 to December 31, 2022.
#'    2) A data frame named "duplicates" of those data which are duplicated in the GWIS database. Duplicates are defined
#'    as more than one value for each station, collection date, parameter combination.
#'    3) A data frame named "Results_Stacked", in stacked (pivot_longer) format, of the groundwater trend data which
#'    excludes the duplicates.
#'    4) A data frame named "Results_Pivoted", in pivoted (pivot_wider) format, of the groundwater trend data which
#'    excludes the duplicates.
#'
#'getdata_trend_results("'CANAL','SPRING RUN','STREAM'","'01-OCT-1998'","'31-DEC-2022'")
#'    Entering getdata_trend_results("'CANAL','SPRING RUN','STREAM'","'01-OCT-1998'","'31-DEC-2022'")
#'    will produce four data frames, similar to the above for all FDEP Trend Network surface water stations.
#'
#'getdata_trend_results("'CANAL'","'01-OCT-1998'","'31-DEC-2022'")
#'    Entering getdata_trend_results("'CANAL'","'01-OCT-1998'","'31-DEC-2022'") will produce four
#'    data frames, similar to the above for only FDEP Trend Network canal stations.
#'

## Revision history
## 02/17/2023 - Updated pattern matching in gsub statements to better accommodate analyte names
#               that include numbers.
## 08/03/2023 - Modified function by adding variable for user-supplied end date for data retrieval,
#               making retrieval dates inclusive (includes data from the specified dates),
#               and limiting data retrieval to Trend Network projects as defined by
#               t_project.fk_super_project = 'SW-TREND' or 'GW-TREND'.
## 08/27/2024 - Changed package for database connection from RODBC to odbc due to switch to 64-bit R.
#             - Updated code for odbc connection and query.
#             - Added water_resource, param_code, and units to list of fields in data pull.
#             - In pivoted data frame, replace triple and double underscores in column names
#               with single underscore to ensure consistent column naming for all analytes.
#             - Added warning message for missing inputs.

getdata_trend_results <- function(arg1 = NA, arg2 = NA, arg3 = NA) {

  # Warning message for missing inputs.
  if(is.na(arg1) | is.na(arg2) | is.na(arg3)){
    stop('ERROR - Missing input. Please supply three inputs: water resources, start date, end date.')
  }

  # Reformat information in arg1, arg2, and arg3 for use in CSV file names.
  arg4 <- gsub("'", "", arg1)
  arg4 <- gsub(",", "_", arg4)
  arg5 <- gsub("'", "", arg2)
  arg6 <- gsub("'", "", arg3)

  # Create odbc connection run data pull script
  channel64 <- dbConnect(odbc::odbc(),
                            "GWIS_ADMIN_64",
                            UID = "GWIS_ADMIN",
                            PWD = rstudioapi::askForPassword("Enter GWIS Password"),
                            Port = 1523,
                            schema = "GWIS_ADMIN",
                            SVC = "GWIS_ADMIN")

  # User will enter the information specific to the results needed for the analysis.  Refer to
  #  example above. -- getdata_trend_results("'AQUIFER','SPRING'","''01-OCT-1998'") --
  # User will then be promoted for the password for the oracle database GWIS_ADMIN
  # A SQL query is then run to pull, pivot, and create the data frame Trend_All_Data.

  Trend_All_Data<-dbGetQuery(channel64,paste("select pk_station, water_resource, pk_result, fk_project,
    collection_date, sample_type, t_sample.matrix, t_result.fk_param_code,
    replace(replace(replace(replace(replace(replace(replace(replace(replace(parameter,
    chr(39),''),chr(44),''),chr(43),''),chr(45),'_'),chr(32),'_'),chr(40),''),chr(41),''),
    chr(91),''),chr(93),'') parameter,value, value_qualifier, units
    from t_station, t_sample,t_parameter,t_result, t_project
       where pk_station = fk_station
        and pk_sample = fk_sample
        and pk_param_code = fk_param_code
        and pk_project = fk_project
        and t_station.waterbody_type in (",arg1,")
        and t_sample.sample_type = 'PRIMARY'
        and t_sample.collection_date >= ",arg2,"
        and t_sample.collection_date <= ",arg3,"
        and t_project.fk_super_project in ('GW-TREND','SW-TREND')
        and t_result.value is not null
        and fk_param_code <> 99982
        and SAMPLED_TV_STATIONS = 'A'
  Union
    Select pk_station, water_resource, pk_result, fk_project, collection_date, sample_type, t_sample.matrix,
    t_result.fk_param_code,
    replace(replace(replace(replace(replace(replace(replace(replace(replace(parameter,
    chr(39),''),chr(44),''),chr(43),''),chr(45),'_'),chr(32),'_'),chr(40),''),chr(41),''),
    chr(91),''),chr(93),'') parameter,value, value_qualifier, units
    from t_station, t_sample,t_parameter,t_result, t_project
       where pk_station = fk_station
        and pk_sample = fk_sample
        and pk_param_code = fk_param_code
        and pk_project = fk_project
        and t_station.waterbody_type in (",arg1,")
        and t_sample.sample_type = 'PRIMARY'
        and t_sample.collection_date >= ",arg2,"
        and t_sample.collection_date <= ",arg3,"
        and t_project.fk_super_project in ('GW-TREND','SW-TREND')
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

  # Identify duplicate results (multiple records with the same combination of PK_STATION, COLLECTION_DATE, & PARAMETER).
  duplicates <- sqldf('select * from Trend_All_Data
                    where PK_STATION||COLLECTION_DATE||PARAMETER
                    IN (SELECT PK_STATION||COLLECTION_DATE||PARAMETER FROM Trend_All_Data
                        GROUP BY PK_STATION||COLLECTION_DATE||PARAMETER
                        HAVING COUNT(PK_STATION||COLLECTION_DATE||PARAMETER) > 1)
                    order by PK_STATION,COLLECTION_DATE,PARAMETER')

  # Remove duplicates from Trend_All_Data dataframe
  Results <- sqldf('select * from Trend_All_Data
                    where PK_RESULT not in (select PK_RESULT
                        FROM duplicates)')

  # Prepare Trend_All_Data for export.
  # Replace period in parameter names with single underscore.
  Trend_All_Data$PARAMETER <- gsub(".","_",Trend_All_Data$PARAMETER, fixed=TRUE)
  # Replace triple and double underscore in parameter names with single underscore.
  Trend_All_Data$PARAMETER <- gsub("___","_",Trend_All_Data$PARAMETER, fixed=TRUE)
  Trend_All_Data$PARAMETER <- gsub("__","_",Trend_All_Data$PARAMETER, fixed=TRUE)
  # Export all data retrieved, before duplicates were removed.
  Trend_All_Data <<- Trend_All_Data
  # Export Trend_All_Data.
  write.csv(Trend_All_Data, file = (paste(arg4,arg5,arg6,'Trend_All_Data.csv',sep = "_")), row.names = FALSE)

  # Prepare duplicates for export.
  # Replace period in parameter names with single underscore.
  duplicates$PARAMETER <- gsub(".","_",duplicates$PARAMETER, fixed=TRUE)
  # Replace triple and double underscore in parameter names with single underscore.
  duplicates$PARAMETER <- gsub("___","_",duplicates$PARAMETER, fixed=TRUE)
  duplicates$PARAMETER <- gsub("__","_",duplicates$PARAMETER, fixed=TRUE)
  # Export duplicates for GWIS corrections.
  duplicates <<- duplicates
  write.csv(duplicates, file = (paste(arg4,arg5,arg6,'DUPLICATES.csv',sep = "_")), row.names = FALSE)

  # Save a copy of the Results data frame after duplicates have been removed,
  # but before converting to pivoted (pivot_wider) format.
  Results_Stacked <- Results
  # Replace period in parameter names with single underscore.
  Results_Stacked$PARAMETER <- gsub(".","_",Results_Stacked$PARAMETER, fixed=TRUE)
  # Replace triple and double underscore in parameter names with single underscore.
  Results_Stacked$PARAMETER <- gsub("___","_",Results_Stacked$PARAMETER, fixed=TRUE)
  Results_Stacked$PARAMETER <- gsub("__","_",Results_Stacked$PARAMETER, fixed=TRUE)
  # Export data after duplicates have been removed.
  Results_Stacked <<- Results_Stacked
  write.csv(Results_Stacked, file = (paste(arg4,arg5,arg6,'Results_Stacked.csv',sep = "_")), row.names = FALSE)

  ##Create new column with value & qualifier concatenated and seperated by a pipe symbol.
  Results$VALUE_VALUE_QUALIFIER<-paste(Results$VALUE,"|",Results$VALUE_QUALIFIER)

  #Now run pivot function on Results data frame
  ##Pivot the results. Should return one column per param with "value|VQ".
  Results_PIVOT<-pivot_wider(Results,
                             id_cols = c(PK_STATION,WATER_RESOURCE,COLLECTION_DATE,SAMPLE_TYPE,MATRIX),
                             names_from = PARAMETER,
                             values_from = VALUE_VALUE_QUALIFIER,
                             values_fill = NULL,
                             values_fn = NULL)

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

  ## Create variable to store number of columns.
  ColCount <- ncol(Results_PIVOT)
  ColCount
  ## Split column 6 through end. Result is two columns for each parameter, value followed by VQ.
  Results_PIVOT_SPLIT  <- cSplit(Results_PIVOT, splitCols = 6:ColCount, "|")
  ## Rename columns with value data by removing the "_1" suffix.
  names(Results_PIVOT_SPLIT) <- gsub('_1$','',names(Results_PIVOT_SPLIT))
  ## Rename columns with VQ data by changing the "_2" suffix to "_VQ".
  names(Results_PIVOT_SPLIT) <- gsub('_2$','_VQ',names(Results_PIVOT_SPLIT))
  ## Replace period in column names with single underscore.
  names(Results_PIVOT_SPLIT) <- gsub(".","_",names(Results_PIVOT_SPLIT), fixed=TRUE)
  ## Replace triple and double underscore in column names with single underscore.
  names(Results_PIVOT_SPLIT) <- gsub("___","_",names(Results_PIVOT_SPLIT), fixed=TRUE)
  names(Results_PIVOT_SPLIT) <- gsub("__","_",names(Results_PIVOT_SPLIT), fixed=TRUE)
  ## Rename Results_PIVOT_SPLIT to Results_Pivoted
  Results_Pivoted <- Results_PIVOT_SPLIT
  Results_Pivoted <<- Results_Pivoted
  View(Results_Pivoted)

  # Export data after duplicates have been removed and data have been pivoted.
  write.csv(Results_Pivoted,file = (paste(arg4,arg5,arg6,'Results_Pivoted.csv',sep = "_")), row.names = FALSE)
}
