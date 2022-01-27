#' Evaluate Materials
#'
#' @description Process the four key tables and get carbon factors for materials
#'
#' @param infra  data frame with infrastructure properties
#' @param interventions a pre-subset data frame
#' @param intervention_assets a pre-subset data frame
#' @param asset_components a pre-subset data frame
#' @param carbon_factors a pre-subset data frame
#' @return a json string as a character 
#' @examples
#' \dontrun{
#' evaluate_materials()
#' }
#' @export

evaluate_materials <- function(infra,
                               interventions, 
                               intervention_assets, 
                               asset_components, 
                               carbon_factors){
  
  # Step 1: Drop unneeded info
  intervention_assets <- intervention_assets[,c("intervention",
                                                "asset",
                                                "asset_unit",
                                                "asset_parameters",
                                                "user_entered_parameters",
                                                "tool_extracted_parameters",
                                                "tool_calculated_parameters",
                                                "area_unit",
                                                "area_default",
                                                "diameter_unit",
                                                "diameter_default",
                                                "length_unit",            
                                                "length_default",
                                                "number_unit",
                                                "number_default",          
                                                "span_unit",
                                                "span_default",
                                                "volume_unit",           
                                                "volume_default",
                                                "width_unit",
                                                "width_default")]
  
  asset_components <- asset_components[,c("intervention_asset",
                                          "item",
                                          "cf_name",
                                          "input_unit",
                                          "quantity",
                                          "A5",
                                          "asset_lifetime",
                                          "replacements_during_lifetime",
                                          "no_granular_data_A1.A3",
                                          "no_granular_data_A4",
                                          "no_granular_data_B2",
                                          "no_granular_data_B4")]
  
  carbon_factors <- carbon_factors[,c("cf_name",
                                      "carbon_factor",
                                      "input_unit",
                                      "carbon_factor_units")]
  # Step 2: Join together the datasets
  
  combined <- dplyr::left_join(intervention_assets, 
                               asset_components, 
                               by = c("asset" = "intervention_asset"))
  combined <- dplyr::left_join(combined, 
                               carbon_factors, 
                               by = c("cf_name" = "cf_name"))
  
  # Step 3: Add in dimensions
  combined$asset_dimension <- get_asset_dimension(combined$asset_unit, infra$length)
  combined$asset_dimension <- ifelse(combined$asset_unit == "number",
                                     combined$number_default,
                                     combined$asset_dimension)
  combined$asset_dimension <- ifelse(combined$asset_unit == "km2",
                                     combined$area_default,
                                     combined$asset_dimension)
  combined$asset_dimension <- as.numeric(combined$asset_dimension) # Will remove non-numeric data
  
  combined$quantity_total <- combined$quantity * combined$asset_dimension
  combined$emissions_total <- combined$quantity_total * combined$carbon_factor
  combined$emissions_total <- round(combined$emissions_total, 2)
  
  
  
  
  
  
  
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
