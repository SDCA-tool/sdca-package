#' Measure Infrastructure characteristics
#'
#' @description Measre infrastrcutre dimensions
#'
#' @param infra a sf dataframe of infrastructure
#' @param rast_dem stars raster DEM raster
#' @param rast_bedrock stars raster bedrock raster
#' @param rast_superficial stars raster superficial raster
#' @param rast_landcover stars raster landcover raster
#' @return a dataframe of
#' @export

measure_infrastucture <- function(infra, 
                                  rast_dem, 
                                  rast_bedrock, 
                                  rast_superficial, 
                                  rast_landcover){
  
  
  # R 3.6 Bug?
  infra <- as.data.frame(infra)
  infra <- sf::st_as_sf(infra, crs = 4326)
  
  infra$length <- as.numeric(sf::st_length(infra))
  
  # Get data from rasters
  raster_data = extract_rasters(infra, 
                                rast_dem, 
                                rast_bedrock, 
                                rast_superficial, 
                                rast_landcover)
  
  # Calculate the gradient of the road
  raster_data = cap_gradient(raster_data, max_gradient = 1.5)
  
  #Calculate the volume of the cut / fill
  
  
  
  
  
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
