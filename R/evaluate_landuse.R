#' Evaluate emissions from change in land cover
#'
#' @description Emissions produced by building over natural land
#'
#' @param infra a sf dataframe of linestrings
#' @param path_landcover path to landcover map
#' @param width width of infrastructure assumed for lines
#' @return a total emissions estimate in kgCO2
#' @export

evaluate_landcover <- function(infra, path_landcover, width = 19){
  
  landcover <- stars::read_stars(path_landcover)
  line_split <- sf::st_segmentize(infra, dfMaxLength = 50)
  line_split <- sf::st_cast(line_split, "POINT")
  
  landcover_types <- stars::st_extract(landcover, at = line_split)
  names(landcover_types) <- c("landcover_id","geometry")
  
  utils::data("landcover_factors", envir=environment())
  
  landcover_types <- dplyr::left_join(landcover_types, 
                                      landcover_factors, 
                                      by = "landcover_id")
  #TODO: Select correct region factors based on location
  landcover_types$total_emissions <- 50 * width * landcover_types$cold_temp_wet
  
  emissions_total <- sum(landcover_types$total_emissions, na.rm = TRUE)
  
  
  return(emissions_total)
  
}