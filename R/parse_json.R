#' Parse JSON
#'
#' @description Function that take JSON from the API and convert into R format
#'
#' @param json a json string as a character
#' @return a named list of data frames and other objects
#' @examples
#' \dontrun{
#' parse_json("jsonhere")
#' }
#' @export
parse_json <- function(json){
  dat <- jsonlite::fromJSON(json, simplifyVector = TRUE)
  expected_names <- c("user_input","intervention_assets",
                      "intervention_assets_parameters",
                      "asset_components","carbon_factors","desire_lines",
                      "path_dem","path_landcover","material_sites")
  if(!all(expected_names %in% names(dat))){
    stop("Some objects are missing from the input JSON: ",
            paste(expected_names[!expected_names %in% names(dat)], 
                  collapse = ", "))
  }
  dat$user_input <- geojsonsf::geojson_sf(dat$user_input)
  dat$desire_lines <- geojsonsf::geojson_sf(dat$desire_lines)
  return(dat)
}
