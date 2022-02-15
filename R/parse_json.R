#' Parse JSON
#'
#' @description Function that take JSON from the API and convert into R format
#'
#' @param json a json string as a character
#' @param file logical, if TRUE json is passed as path to a file. Else assumed to contain json
#' @return a named list of data frames and other objects
#' @examples
#' \dontrun{
#' parse_json("jsonhere")
#' }
#' @export
parse_json <- function(json, file = TRUE){
  if(file){
    dat <- jsonlite::read_json(json, simplifyVector = TRUE)
  } else {
    dat <- jsonlite::fromJSON(json, simplifyVector = TRUE)
  }
  expected_names <- c("user_input","assets",
                      "assets_parameters",
                      "components","carbon_factors","desire_lines",
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
