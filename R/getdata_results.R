#' Function to import results into R data frame from the FDEP Oracle Database GWIS
#'
#' @title getdata_results
#'
#' @description Creates two data frames of results data via
#'  oracle data pull.
#'    1) Data frame named "Results" contains results data in pivoted (pivot_wider) format.
#'    2) Data frame named "Results_Stacked" contains results data in stacked (pivot_longer) format.
#' User will be prompted for the password to the FDEP Oracle Database GWIS.
#' Replaces measurement values with 'NA' for those measurements having any of
#'  the following fatal data qualifiers: '?,O,N,T,X'.
#' Definitions for these codes may be found in FS 62-160.700 Table 1 (Data Qualifier Codes).
#' This function produce extraneous columns in the pivoted data frame if there are
#'  multiple values for the same parameter for a single sample.  If this occurs examine the results data frame.
#'  Locate the affected samples and investigate further.  Type c(" in the R Studio search bar
#'  to search the results data frame for the affected samples. Contact data management
#'  team for resolution.
#'
#'
#' @param arg1 variable passed into SQL select statement to pull data and name data export
#' @import odbc
#' @import dplyr
#' @import tidyr
#' @import splitstackshape
#' @import stringr
#' @import rstudioapi
#' @export
#' @examples getdata_results("'CN18'")
#'    entering "'CN18'" for arg1 will produce data frames for FDEP Status Canals sampled in 2018.
#'
#'getdata_results("'CN18','CN19','CN20'")
#'    entering "'CN18','CN19','CN20'" for arg1 will produce data frames for FDEP Status Canals sampled 2018 - 2020.
#'

## Revision history
## 11/30/2020 - Changed character replacement from underscore to period. This was needed to make pivot
#               and split work correctly for column names that included numbers (e.g. PCB 1260).
#               Periods will be replaced by underscores after pivot and split is complete.
#             - Modified CSV file naming. Name is now value of arg1 without quotes.
#               Designated underscore as separator in paste functions.
#               For multi-year analysis, portions of arg1 are separated by underscores
#               (e.g. CN18_CN19_CN20_Results.csv).
#             - Replaced soft deprecated function rename_at with gsub.
## 05/20/2021 - Modified to add fk_sample into data pull to correct 'duplicated' data issue.
## 02/17/2023 - Modified CSV file naming to support scenarios where arg1 contains
#               more than 3 resource and year identifiers.
#             - Removed row names from CSV file output.
#             - Updated pattern matching in gsub statements to better accommodate analyte
#               names that include numbers.
## 08/27/2024 - Changed package for database connection from RODBC to odbc due to switch to 64-bit R.
#             - Updated code for odbc connection and query.
#             - Added water_resource, param_code and units to list of fields in data pull.
#             - Added export of results data in stacked (pivot longer) format.
#             - In pivoted data frame, replace triple and double underscores in column names
#               with single underscore to ensure consistent column naming for all analytes.
#             - Added warning message for missing inputs.

getdata_results <- function(arg1 = NA) {

  # Warning message for missing inputs.
  if(is.na(arg1)){
    stop('ERROR - Missing input. Please provide the four-character code(s) for the water resource(s) and year(s) to be used in data pull.')
  }

  # Reformat arg1 for use when naming Export file.
  arg2 <- gsub("'", "", arg1)
  arg2 <- gsub(",", "_", arg2)

  # Create odbc connection and run data pull script.
  channel64 <- dbConnect(odbc::odbc(),
                            "GWIS_ADMIN_64",
                            UID = "GWIS_ADMIN",
                            PWD = rstudioapi::askForPassword("Enter GWIS Password"),
                            Port = 1523,
                            schema = "GWIS_ADMIN",
                            SVC = "GWIS_ADMIN")

  # User will enter the infromation specific to the results needed for the analysis.  Refer to
  #  example above. -- getdata_results("'CN18'") --
  # User will then be promoted for the password for the oracle database GWIS_ADMIN

  # A SQL query is then run to pull, pivot, and and create the data frame Results.
  Results<-dbGetQuery(channel64,paste("select fk_station, s.fk_random_sample_location, water_resource,
    fk_sample, collection_date, sample_type, s.matrix, r.fk_param_code,
    replace(replace(replace(replace(replace(replace(replace(replace(replace(parameter,
    chr(39),''),chr(44),''),chr(43),''),chr(45),'.'),chr(32),'.'),chr(40),''),chr(41),''),
    chr(91),''),chr(93),'') parameter,value, value_qualifier, units
        from t_station, t_sample s,t_parameter p,t_result r
        where pk_station = fk_station
        and pk_sample = fk_sample
        and pk_param_code = fk_param_code
        and fk_param_code <> 99982
        and substr(fk_project,3,4) in (",arg1,")
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

  # Save a copy of the Results data frame before converting to pivoted (pivot_wider) format.
  Results_Stacked <- Results
  # Replace period in parameter names with single underscore.
  Results_Stacked$PARAMETER <- gsub(".","_",Results_Stacked$PARAMETER, fixed=TRUE)
  # Replace triple and double underscore in parameter names with single underscore.
  Results_Stacked$PARAMETER <- gsub("___","_",Results_Stacked$PARAMETER, fixed=TRUE)
  Results_Stacked$PARAMETER <- gsub("__","_",Results_Stacked$PARAMETER, fixed=TRUE)

  # Export a copy of the Results data frame as a CSV file. Data frame is in stacked
  # (pivot_longer) format, and has not yet been converted to pivoted (pivot_wider) format.
  Results_Stacked <<- Results_Stacked
  write.csv(Results_Stacked,file = (paste(arg2,"Results_Stacked.csv", sep = "_")), row.names = FALSE)

  # Prepare for conversion to pivoted format.
  # Create new column with value & qualifier concatenated and separated by a pipe symbol.
  Results$VALUE_VALUE_QUALIFIER<-paste(Results$VALUE,"|",Results$VALUE_QUALIFIER)

  # Pivot the results. Should return one column per param with "value|VQ".
  Results_PIVOT<-pivot_wider(Results,
                             id_cols = c(FK_STATION,FK_RANDOM_SAMPLE_LOCATION,WATER_RESOURCE,FK_SAMPLE,COLLECTION_DATE,SAMPLE_TYPE,MATRIX),
                             names_from = PARAMETER,
                             values_from = VALUE_VALUE_QUALIFIER,
                             values_fill = NULL,
                             values_fn = NULL)

  # Create variable to store number of columns.
  ColCount <- ncol(Results_PIVOT)
  ColCount

  # Split column 8 through end. Result is two columns for each parameter, value followed by VQ.
  Results_PIVOT_SPLIT  <- cSplit(Results_PIVOT, splitCols = 8:ColCount, "|")

  # Rename columns with value data by removing the "_1" suffix.
  names(Results_PIVOT_SPLIT) <- gsub('_1$','',names(Results_PIVOT_SPLIT))

  # Rename columns with VQ data by changing the "_2" suffix to "_VQ".
  names(Results_PIVOT_SPLIT) <- gsub('_2$','_VQ',names(Results_PIVOT_SPLIT))

  # Replace period in column names with single underscore.
  names(Results_PIVOT_SPLIT) <- gsub(".","_",names(Results_PIVOT_SPLIT), fixed=TRUE)

  # Replace triple and double underscore in column names with single underscore.
  names(Results_PIVOT_SPLIT) <- gsub("___","_",names(Results_PIVOT_SPLIT), fixed=TRUE)
  names(Results_PIVOT_SPLIT) <- gsub("__","_",names(Results_PIVOT_SPLIT), fixed=TRUE)

  ##Rename pivoted data table.
  Results <- Results_PIVOT_SPLIT

  # Export a copy of the Results data frame as a CSV file.
  # Data frame is in pivoted (pivot_wider) format.
  Results <<- Results
  View(Results)
  write.csv(Results,file = (paste(arg2,"Results.csv", sep = "_")), row.names = FALSE)
}
