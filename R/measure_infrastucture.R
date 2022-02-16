#' Measure infrastructure
#'
#' @description Internal function that is to be run on row at a time
#'
#' @param infra  data frame with infrastructure properties
#' @param assets a pre-subset data frame
#' @param assets_parameters a pre-subset data frame
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
                               assets_parameters,
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
  
  # Drop unneeded info
  assets <- assets[assets$intervention %in% infra$intervention,]
  components <- components[components$asset %in% assets$asset,]
  if("data.frame" %in% class(carbon_factors)){
    carbon_factors <- carbon_factors[carbon_factors$cf_name %in% components$cf_name,]
  } else{
    carbon_factors = data.frame(cf_name = NA,
                                material_type = NA,
                                carbon_factor = NA,
                                carbon_factor_units = NA,
                                input_unit = NA,
                                category = NA)
  }
  if("data.frame" %in% class(assets_parameters)){
    assets_parameters <- assets_parameters[assets_parameters$asset %in% assets$asset,]
  }
  
  
  
  # Set up main rules
  
  # Skip the special onward travel case as no construction occurs
  if(infra$infrastructure_type == "onward_travel"){
    results = list(NULL, NULL, NULL, NULL)
    names(results) = c("material_emissions", "materials_itemised", "landcover_emissions", "cut_fill_emissions")
    return(results)
  }
  
  # TODO: Not all lines nead cut_fill add logic
  if(any(grepl("cutting", infra$intervention),grepl("embankment", infra$intervention))){
    do_cut_fill <- TRUE
  } else {
    do_cut_fill <- FALSE
  }
  
  # Step 1: Measure infrastructure and get data
  # R 3.6 Bug?
  infra <- as.data.frame(infra)
  infra <- sf::st_as_sf(infra, crs = 4326)
  infra$length <- as.numeric(sf::st_length(infra))
  
  #TODO; Get correct with of infrastructure
  if("data.frame" %in% class(assets_parameters)){
    if(nrow(assets_parameters) > 0){
      widths <- assets_parameters[assets_parameters$parameter == "width",]
      infra$width = widths$default[1]
    } else {
      infra$width = 19
    }
  } else {
    infra$width = 19
  }
  
  
  # Get data from the rasters
  infra_data <- extract_rasters(infra, 
                                rast_dem, 
                                rast_bedrock, 
                                rast_superficial, 
                                rast_landcover)
  
  
  # Step 2: Estimate Materials required
  
  # TODO: Method for no descrete data
  # Join together
  combined <- dplyr::left_join(assets, 
                               components, 
                               by = c("asset" = "asset"))
  
  
  combined <- dplyr::left_join(combined, 
                               carbon_factors, 
                               by = c("cf_name" = "cf_name"))
  
  
  # Materials Emissions
  mat_res = cacualte_materials(infra, combined, material_sites)
  
  material_emissions <- mat_res$material_emissions
  materials_itemised <- mat_res$materials_itemised
  
  # Step 3: Calculate Earthworks
  if(do_cut_fill){
    
    # Choose Max gradient as a %
    if(infra$mode_class == "Rail"){
      max_gradient = 1.5
    } else {
      max_gradient = 8.3
    }
    
    gradient_data <- cap_gradient(infra_data, max_gradient = max_gradient)
    
    #Calculate the cut / fill emissions
    cut_fill_emissions = cut_fill(gradient_data, width = infra$width)
  } else {
    cut_fill_emissions = data.frame(
      total_cut = 0,
      total_fill = 0,
      carbon_cut = 0,
      carbon_processing = 0,
      carbon_fill = 0,
      material_disposal = 0,
      material_brought_in = 0
    )
  }
  
  # Step 4: Land Cover Emissions
  landcover_emissions = evaluate_landcover(infra_data, width = infra$width)
  
  # Step Last: Put together the results and return.
  
  
  
  results = list(material_emissions, materials_itemised, landcover_emissions, cut_fill_emissions)
  names(results) = c("material_emissions", "materials_itemised", "landcover_emissions", "cut_fill_emissions")
  
  return(results)
  
}

# get_asset_dimension <- function(asset_unit, 
#                                 length_m,
#                                 default_area,
#                                 default_number){
#   weight = vapply(asset_unit, 
#                   get_asset_dimension_int, 
#                   FUN.VALUE = 1.1,
#                   USE.NAMES = FALSE)
#   return(weight * length_m)
# }
# 
# get_asset_dimension_int <- function(unit){
#   if(unit == "km"){
#     return(0.001)
#   }
#   if(unit == "m"){
#     return(1)
#   }
#   return(0)
# }
