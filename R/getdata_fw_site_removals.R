#' Function to identify sites which are not co-located with features in
#' a user-supplied line GIS coverage.
#'
#' @title getdata_fw_site_removals
#'
#' @description Identifies sites, from a user-identified data frame of site evaluation
#' data, which are not co-located with features in a user-supplied line GIS coverage.
#' Co-located is defined as points located within 50m of flowing waters line features.
#' There are three expected outputs.
#' 1) Data frame in simple features (sf) object format with sites to be retained for analysis.
#' The data frame will be named SITES_WITHIN_50M, preceded by the two-letter abbreviation(s)
#' for the site evaluation data's water resource(s).
#' 2) Data frame in sf object format with sites to be removed from the Status Network analyses.
#' The data frame will be named SITES_NOT_WITHIN_50M, preceded by the two-letter abbreviation(s)
#' for the site evaluation data's water resource(s).
#' 3) A .CSV file saved in the R project working directory, containing sites to be removed from the
#' Status Network analyses. The .csv file name will be SITES_NOT_WITHIN_50M.csv, preceded by the
#' two-letter abbreviation(s) for the site evaluation data's water resource(s).
#'
#' @param site_evaluations  Name of data frame with site evaluation data. Though not required,
#'                          the site evaluation data typically includes data from multiple years.
#'                          Site evaluation data should be retrieved using
#'                          FDEPgetdata::getdata_fw_exclusions
#' @param listframe_directory  Folder path for line shapefile of listframe data to be used for analysis.
#'                             Default is ".", which indicates that the shapefile is
#'                             located in the main folder of the R project working directory.
#' @param listframe  Name of the line shapefile of listframe data to be used for analysis.
#' @param sites_crs  Coordinate Reference System (CRS/EPSG) Code for sites evaluation data latitude and longitude.
#'                   Default is CRS 4269 (NAD83).
#' @import sf
#' @export
#' @examples getdata_fw_site_removals(site_evaluations = Exclusions,
#'                            listframe_directory = "./data",
#'                            listframe = "Cycle17_streams_coverage_2023")
#' 		Entering information for a data frame with stream site evaluaiton data, and a stream listframe shapefile
#' 		will produce a data frame with sites to be retained (those located within 50m of the listframe line features),
#' 		a seconds data frame with sites to be removed (those located greater than 50m from the listframe line features),
#' 		and a .CSV file export of the information from the data frame for the sites to be removed.
#'

## Revision history.
#  10/09/2024 getdata_fw_site_removals function created.

getdata_fw_site_removals <- function(site_evaluations = NA,
                                     listframe_directory = ".",
                                     listframe = NA,
                                     sites_crs = 4269) {

# Warning message for missing inputs.
if(length(site_evaluations)<=1){
  stop('ERROR - Missing input. Please provide name of site_evaluations data frame.')
  } # end if statement
if(is.na(listframe[1])){
    stop('ERROR - Missing input. Please provide name of listframe shapefile enclosed in quotes.')
  } # end if statement

# Create new data frame from site_evaluations.
SITES<-site_evaluations

# Determine water_resource from site_evaluation data.
water_resource <- sort(unique(substr(SITES$PK_RANDOM_SAMPLE_LOCATION, 4,5)))

# Convert location data from DDMMSS.SSS to Decimal degrees
deg <- floor(SITES$RANDOM_LATITUDE/10000)
min <- floor((SITES$RANDOM_LATITUDE - deg*10000)/100)
sec <- SITES$RANDOM_LATITUDE - deg*10000 - min*100
SITES$latdd <- deg + min/60 + sec/3600
deg <- floor(SITES$RANDOM_LONGITUDE/10000)
min <- floor((SITES$RANDOM_LONGITUDE - deg*10000)/100)
sec <- SITES$RANDOM_LONGITUDE - deg*10000 - min*100
SITES$londd <- deg + min/60 + sec/3600
# Change londd to negative because all FL data are in western hemisphere.
SITES$londd <- -SITES$londd

# Create sf object and transform to Albers projection for analysis
#  This codes utilizes Coordinate Reference System (CRS/EPSG) Codes.
#  More information on these codes is found here:
#  https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf.
#  The code used for imported site_evaluations data is CRS 4269, for NAD 83 coordinate system,
#  unless otherwise specified by the user. Data must be transformed to a projected CRS for analyses
#  involving distance measurements. The Florida Albers projection CRS 3087 is used.
#  Units for CRS 3087 are meters.
SITES_SF<- st_as_sf(SITES, coords = c("londd", "latdd"), remove = FALSE, crs = sites_crs)
SITES_SF <- st_transform(SITES_SF, crs = 3087)

# keep xy coords as variables
SITES_SF$xcoord <- st_coordinates(SITES_SF)[, "X"]
SITES_SF$ycoord <- st_coordinates(SITES_SF)[, "Y"]

# Create simple features (sf) object from shapefile of line or polygon features representing
# the listframe coverage. Change projection for Zones sf object to Florida Albers HARN(CRS code 3087).
LISTFRAME_SF <- st_read(dsn=listframe_directory, layer=listframe)
LISTFRAME_SF <- st_transform(LISTFRAME_SF, crs = 3087)

# Determine which points in SITES_SF are located within 50m of line features in LISTFRAME_SF.
WITHIN_50M_T_F <- st_is_within_distance(SITES_SF, LISTFRAME_SF, dist = 50, sparse = TRUE)
SITES_WITHIN_50M <- SITES_SF[lengths(WITHIN_50M_T_F) > 0,]
SITES_NOT_WITHIN_50M <- SITES_SF[!lengths(WITHIN_50M_T_F) > 0,]
# Export the results
assign(x=paste0(paste0(water_resource, collapse = '_'),'_SITES_NOT_WITHIN_50M'),
       value=SITES_NOT_WITHIN_50M,
       envir = globalenv())
write.csv(SITES_NOT_WITHIN_50M,
          paste0(paste0(water_resource, collapse = '_'),'_SITES_NOT_WITHIN_50M.csv'),
          row.names = FALSE)
assign(x=paste0(paste0(water_resource, collapse = '_'),'_SITES_WITHIN_50M'),
       value=SITES_WITHIN_50M,
       envir = globalenv())
} #####  end function
