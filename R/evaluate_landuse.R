#' Evaluate emissions from change in land cover
#'
#' @description Emissions produced by building over natural land
#'
#' @param infra_data a data frame from `extract_rasters`
#' @param width width of infrastructure assumed for lines
#' @return a total emissions estimate in kgCO2
#' @export

evaluate_landcover <- function(infra_data, width = 19){
  
  # Check input
  if(is.na(width)){
    width <- 1
  }
  
  # Identify climate Zones
  utils::data("climate_zones", envir=environment())
  
  inter <- sf::st_intersects(infra_data, climate_zones)
  inter <- lengths(inter)
  infra_data$warm_climate <- inter > 0
  
  # Calculate distances
  # TODO: We also do this for the cut/fill so wasted computation
  coords <- sf::st_coordinates(infra_data)
  infra_data <- sf::st_drop_geometry(infra_data)
  if(nrow(infra_data) > 1){
    infra_data$distance <- geodist::geodist(coords, sequential = TRUE, pad = TRUE)
  } else {
    infra_data$distance <- 1
  }
  
  # Calculate emissions
  infra_data$landcover_emissions <- ifelse(infra_data$warm_climate,
                                            infra_data$distance * width * infra_data$warm_temp_wet,
                                            infra_data$distance * width * infra_data$cold_temp_wet)
  
  emissions_total <- sum(infra_data$landcover_emissions, na.rm = TRUE)
  
  # Convert from tonnes per hectare to kgCO2 per m2
  emissions_total <- emissions_total / 10
  
  emissions_total <- data.frame(emissions_total = emissions_total)
  return(emissions_total)
}
