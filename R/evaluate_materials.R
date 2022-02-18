#' Calculate Materials
#'
#' @description Internal function that calculates material emissions
#'
#' @param infra  data frame with infrastructure properties
#' @param combined a pre-joined data frame
#' @param material_sites nearest material site for project
#' @return a list of data frames
#' @examples
#' \dontrun{
#' evaluate_materials()
#' }
#' @export
#' 
cacualte_materials <- function(infra, combined, material_sites){
  
  # Steps 
  # 1 - Calculate quantity of materials required and emission (A1-3)
  # 2 - Summarise amount of material types required for transport (A4)
  # 3 - Calculate emissions from contribution process (A5)
  # 4 - Calculate emissions from replacements over lifetime (B4)
  
  combined$quantity_total <- combined$quantity * infra$length
  combined$emissions_total <- combined$quantity_total * combined$carbon_factor
  
  #A1-3 Emissions & A5 Emissions
  A1_3_emissions = sum(combined$emissions_total, na.rm = TRUE) + 
    sum(combined$no_granular_data_A1.A3, na.rm = TRUE) * infra$length
  A5_emissions = sum(combined$A5 * infra$length, na.rm = TRUE) 
  
  #A4 Emissions
  combined = dplyr::left_join(combined, material_sites, by = c("material_type" = "Material_Types"))
  combined$distance_km  = combined$distance_km * 1.4 # Add factor of 1.4 for circuity
  combined$distance_km[is.na(combined$distance_km)] = 50 #Assume 50km for unknown distances
  
  # Emission factors laden and unladen lorry
  # 0.00010749 tCO2e/t.km
  # 0.0000874 tCO2e/t.km
  combined$A4 = (combined$distance_km * combined$quantity_total / 1000 * 0.00010749 +
                        combined$distance_km * 0.0000874) * 1000
  
  A4_emissions = sum(combined$A4, na.rm = TRUE) + 
    sum(combined$no_granular_data_A4, na.rm = TRUE) * infra$length
  
  #B4 Assume same as construction * replacements
  combined$B4 = combined$A4 * combined$replacements_during_lifetime
  
  B4_emissions = sum(combined$B4, na.rm = TRUE) + 
    sum(combined$no_granular_data_B4, na.rm = TRUE) * infra$length
  
  # Make Detailed Emission Table
  combined = combined[,c("intervention","asset","item",
                         "quantity_total","input_unit.x",
                         "emissions_total","A4",
                         "A5","B4")]
  names(combined) = c("intervention","asset","item",
                      "quantity","quantity_units",
                      "A1_3","A4",
                      "A5","B4")
  combined = combined[order(combined$A1_3, decreasing = TRUE),]
  
  headline = data.frame(A1_3_emissions = A1_3_emissions,
                        A4_emissions = A4_emissions,
                        A5_emissions = A5_emissions,
                        B4_emissions = B4_emissions,
                        stringsAsFactors = FALSE)
  
  res <- list(headline, combined)
  names(res) <- c("material_emissions","materials_itemised")
  return(res)
}
