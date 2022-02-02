#' Measure Infrastructure characteristics
#'
#' @description Measre infrastrcutre dimensions
#'
#' @param infra a sf dataframe of infrastructure
#' @param path_dem path to the DEM raster
#' @return a dataframe of
#' @export

measure_infrastucture <- function(infra,
                                  path_dem = "D:/GitHub/SDCA-tool/sdca-data-prep/data/UK-dem-50m-4326-Int16.tif"){
  
  infra <- sf::st_as_sf(infra)
  
  infra$length <- as.numeric(sf::st_length(infra))
  
  # Get elevations
  
  elevations <- try(extract_elevations(infra$geometry, path_dem), silent = TRUE)
  if("try-error" %in% class(elevations)){
    elevations <- infra
    elevations$elevation <- 0
    elevations$road <- 0
    elevations$difference <- 0
  }
  
  elevations$slope <- 45
  elevations$width <- 19
  
  elevations <- cut_fill(elevations)
  
  total_cut <- sum(elevations$volume[elevations$volume > 0])
  total_fill <- sum(elevations$volume[elevations$volume < 0]) * -1
  net_earth_needed <- total_fill - total_cut #-ve materail needs removing +ve material needs to be brought to site
  
  infra$total_cut <- total_cut
  infra$total_fill <- total_fill
  infra$net_earth_needed <- net_earth_needed
  
  return(infra)
}
