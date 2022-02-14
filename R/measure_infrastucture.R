#' Measure infrastructure
#'
#' @description Internal function that is to be run on row at a time
#'
#' @param infra  data frame with infrastructure properties
#' @param assets a pre-subset data frame
#' @param intervention_assets_parameters a pre-subset data frame
#' @param components a pre-subset data frame
#' @param carbon_factors a pre-subset data frame
#' @param material_sites a data frame of the nearest material sites
#' @param rast_dem path to dem
#' @param rast_bedrock path to bedrock
#' @param rast_superficial path to superfical
#' @param rast_landcover path to landcover
#' @return a json string as a character 
#' @examples
#' \dontrun{
#' evaluate_materials()
#' }
#' @export

measure_infrastucture <- function(infra,
                               assets,
                               intervention_assets_parameters,
                               components, 
                               carbon_factors,
                               material_sites,
                               rast_dem, 
                               rast_bedrock, 
                               rast_superficial, 
                               rast_landcover){
  
  # For each piece of infrastructure make the following calculations
  # 1 - measure dimensions and extract relevant spatial data
  # 2 - estimate material requirements and emissions
  # 3 - (conditional) estimate earthwork requirements
  # 4 - estimate land cover change and emissions
  
  # Step 1: Measure infrastructure and get data
  # R 3.6 Bug?
  infra <- as.data.frame(infra)
  infra <- sf::st_as_sf(infra, crs = 4326)
  infra$length <- as.numeric(sf::st_length(infra))
  
  #TODO; Get correct with of infrastructure
  infra$width = 19
  
  # Get data from the rasters
  infra_data <- extract_rasters(infra, 
                                rast_dem, 
                                rast_bedrock, 
                                rast_superficial, 
                                rast_landcover)
  
  
  # Step 2: Estimate Materials required
  # Drop unneeded info
  assets <- assets[assets$intervention %in% infra$intervention,]
  components <- components[components$intervention_asset %in% assets$asset,]
  carbon_factors <- carbon_factors[carbon_factors$cf_name %in% components$cf_name,]
  
  # Join together
  combined <- dplyr::left_join(assets, 
                               components, 
                               by = c("asset" = "intervention_asset"))
  combined <- dplyr::left_join(combined, 
                               carbon_factors, 
                               by = c("cf_name" = "cf_name"))
  
  
  # Materials Emissions
  mat_res = cacualte_materials(infra, combined)
  
  material_emissions <- mat_res$material_emissions
  materials_itemised <- mat_res$materials_itemised
  
  # Step 3: Calculate Earthworks
  if(TRUE){
    
    # Choose Max gradient as a %
    if(infra$mode_class == "Rail"){
      max_gradient = 1.5
    } else {
      max_gradient = 8.3
    }
    
    infra_data <- cap_gradient(infra_data, max_gradient = max_gradient)
    
    #Calculate the cut / fill emissions
    cut_fill_emissions = cut_fill(infra_data, width = infra$width)
  }
  
  
  #TODO finished up this fucntion with new strucutre and results
  
  
  
  results = list(headline, combined)
  names(results) = c("headline","itemised")
  
  return(results)
  
}

get_asset_dimension <- function(asset_unit, 
                                length_m,
                                default_area,
                                default_number){
  weight = vapply(asset_unit, 
                  get_asset_dimension_int, 
                  FUN.VALUE = 1.1,
                  USE.NAMES = FALSE)
  return(weight * length_m)
}

get_asset_dimension_int <- function(unit){
  if(unit == "km"){
    return(0.001)
  }
  if(unit == "m"){
    return(1)
  }
  return(0)
}
