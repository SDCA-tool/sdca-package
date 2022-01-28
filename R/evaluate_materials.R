#' Evaluate Materials
#'
#' @description Process the four key tables and get carbon factors for materials
#'
#' @param infra  data frame with infrastructure properties
#' @param interventions a pre-subset data frame
#' @param intervention_assets a pre-subset data frame
#' @param asset_components a pre-subset data frame
#' @param carbon_factors a pre-subset data frame
#' @param material_sites a data frame of the nearest material sites
#' @return a json string as a character 
#' @examples
#' \dontrun{
#' evaluate_materials()
#' }
#' @export

evaluate_materials <- function(infra,
                               interventions, 
                               intervention_assets,
                               intervention_assets_parameters,
                               asset_components, 
                               carbon_factors,
                               material_sites){
  
  # Step 1: Drop unneeded info
  intervention_assets <- intervention_assets[,c("intervention",
                                                "asset",
                                                "asset_unit",
                                                "asset_parameters",
                                                "user_entered_parameters",
                                                "tool_extracted_parameters",
                                                "tool_calculated_parameters")]
  
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
                                      "material_type",
                                      "carbon_factor_units")]
  # Step 2: Join together the datasets
  
  combined <- dplyr::left_join(intervention_assets, 
                               asset_components, 
                               by = c("asset" = "intervention_asset"))
  combined <- dplyr::left_join(combined, 
                               carbon_factors, 
                               by = c("cf_name" = "cf_name"))
  
  # Step 3: Add in dimensions
  infra <- measure_infrastucture(infra)
  #combined$asset_dimension <- get_asset_dimension(combined$asset_unit, infra$length)
  
  # combined$asset_dimension <- ifelse(combined$asset_unit == "number",
  #                                    combined$number_default,
  #                                    combined$asset_dimension)
  # combined$asset_dimension <- ifelse(combined$asset_unit == "km2",
  #                                    combined$area_default,
  #                                    combined$asset_dimension)
  #combined$asset_dimension <- as.numeric(combined$asset_dimension) # Will remove non-numeric data
  
  combined$quantity_total <- combined$quantity * infra$length
  combined$emissions_total <- combined$quantity_total * combined$carbon_factor
  combined$emissions_total <- round(combined$emissions_total, 2)
  
  #A1-3 Emissions & A5 Emissions
  
  A1_3_emissions = sum(combined$emissions_total, na.rm = TRUE)
  A5_emissions = sum(combined$A5 * infra$length, na.rm = TRUE)
  
  #A4 Emissions
  combined = dplyr::left_join(combined, material_sites, by = c("material_type" = "Material_Types"))
  combined$distance_km  = combined$distance_km * 1.4 # Add factor of 1.4 for circuity
  combined$distance_km[is.na(combined$distance_km)] = 50 #Assume 50km for unknown distances
  
  
  combined$A4 = round(combined$distance_km * combined$quantity_total / 1000 * 0.00010749 +
    combined$distance_km * 0.0000874,2)

  A4_emissions = sum(combined$A4, na.rm = TRUE)
  
  #B4 Assume same as construction * replacements
  
  
  
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
