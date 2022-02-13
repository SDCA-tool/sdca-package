#' Assumed emission factors of modes of transport
#'
#' Taken from the 2021 DEFRA emission factors, emissions in kgco2e/km
#'
#' @format A data frame with 1 rows and 8 variables:
#' @source \url{https://www.gov.uk/government/publications/greenhouse-gas-reporting-conversion-factors-2021}
"emissions_factors"

#' Assumed mode shift for types of infrastructure
#'
#' Based on SDCA research
#'
#' @format A data frame with 6 rows and 9 variables:
"mode_shifts"

#' Emission factors for change in land use which match the CORINE land cover map
#'
#' Based on SDCA research
#' 
#' Factors in kgCo2 per m2 of land changed to settlement (i.e. built on)
#' Two environmental zones included cold_temp_wet  and warm_temp_wet
#' UK is mostly cold_temp_wet except for south west and London.
#'
#' @format A data frame with 37 rows and 4 variables:
"landcover_factors"

#' Emission factors for bedrock types
#'
#' Based on SDCA research
#' 
#' @format A data frame with 86 rows and 15 variables:
"bedrock"


#' Emission factors for superficial geology
#'
#' Based on SDCA research
#' 
#'
#' @format A data frame with 8 rows and 16 variables:
"superficial"