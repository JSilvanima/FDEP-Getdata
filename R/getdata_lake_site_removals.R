#' Function to identify sites which are not co-located with features in
#' a user-supplied polygon GIS coverage.
#'
#' @title getdata_lake_site_removals
#'
#' @description Identifies sites, from a user-identified data frame of site evaluation
#' data, which are not co-located with features in a user-supplied polygon GIS coverage.
#' Co-located is defined as site evaluation points intersecting lake polygons.
#' There are three expected outputs.
#' 1) Data frame in simple features (sf) object format with sites to be retained for analysis.
#' For lakes resources, the data frame will be named SITES_INTERSECTS, preceded by the two-letter
#' abbreviation(s) for the site evaluation data's water resource(s).
#' 2) Data frame in sf object format with sites to be removed from the Status Network analyses.
#' For lakes resources, the data frame will be named SITES_NOT_INTERSECTS, preceded by the
#' two-letter abbreviation(s) for the site evaluation data's water resource(s).
#' 3) A .CSV file saved in the R project working directory, containing sites to be removed from the
#' Status Network analyses. For lakes resources, the .CSV file will be named SITES_NOT_INTERSECTS.csv,
#' preceded by the two-letter abbreviation(s) for the site evaluation data's water resource(s).
#'
#' @param site_evaluations  Name of data frame with site evaluation data. The site evaluation data must
#'                          include data from multiple years (typically three).
#'                          Site evaluation data should be retrieved using
#'                          FDEPgetdata::getdata_lake_exclusions.
#' @param listframe_directory  Folder path for polygon shapefile of listframe data to be used for analysis.
#'                             Default is ".", which indicates that the shapefile is
#'                             located in the main folder of the R project working directory.
#' @param listframe  Name of the polygon shapefile of listframe data to be used for analysis.
#' @param sites_crs  Coordinate Reference System (CRS/EPSG) Code for sites evaluation data latitude and longitude.
#'                   Default is CRS 4269 (NAD83).
#' @import sf
#' @export
#' @examples getdata_lake_site_removals(site_evaluations = Exclusions,
#'                            listframe_directory = "./data",
#'                            listframe = "Cycle17_LargeLakes_coverage_2023")
#' 		Entering information for a data frame with large lake site evaluation data, and a large lake listframe shapefile
#' 		will produce a data frame with sites to be retained (those that intersect the listframe polygon features),
#' 		a seconds data frame with sites to be removed (those that do not intersect the listframe polygon features),
#' 		and a .CSV file export of the information from the data frame for the sites to be removed.

## Revision history.
#  10/09/2024 getdata_lake_site_removals function created.

getdata_lake_site_removals <- function(site_evaluations = NA,
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

# Determine which points in SITES_SF intersect LISTFRAME_SF.
INTERSECT_T_F <- st_intersects(SITES_SF, LISTFRAME_SF, sparse = TRUE)
SITES_INTERSECTS <-SITES_SF[lengths(INTERSECT_T_F) > 0,]
SITES_NOT_INTERSECTS <-SITES_SF[!lengths(INTERSECT_T_F) > 0,]
# Export the results
assign(x=paste0(paste0(water_resource, collapse = '_'),'_SITES_NOT_INTERSECTS'),
       value=SITES_NOT_INTERSECTS,
       envir = globalenv())
write.csv(SITES_NOT_INTERSECTS,
          paste0(paste0(water_resource, collapse = '_'),'_SITES_NOT_INTERSECTS.csv'),
          row.names = FALSE)
assign(x=paste0(paste0(water_resource, collapse = '_'),'_SITES_INTERSECTS'),
       value=SITES_INTERSECTS,
       envir = globalenv())
} #####  end function
