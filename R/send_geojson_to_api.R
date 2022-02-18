#' Send to API
#'
#' @description Function that take JSON from the API and convert into R format
#'
#' @param path path to geojson
#' @param url API url
#' @param showinput logical, if TRUE input to R package is returned else resutls
#'   are returned
#' @return a named list of data frames and other objects
#' @examples
#' \dontrun{
#' parse_json("jsonhere")
#' }
#' @export

geojson_api <- function(path, 
                        url = "https://dev.carbon.place/api/v1/locations.json?geojson=",
                        showinput = TRUE){
  # Reag GEOJSON
  json <- geojsonsf::geojson_sf(path)
  json <- geojsonsf::sf_geojson(json)
  
  # Build URL
  url <- paste0(url,json)
  if(showinput){
    url <- paste0(url,"&showinput=true")
  }
  url <- utils::URLencode(url)
  
  text <- curl::curl_fetch_memory(url)
  text <- rawToChar(text$content)
  
  dat <- jsonlite::fromJSON(text)
  dat$user_input <- geojsonsf::geojson_sf(dat$user_input)
  dat$desire_lines <- geojsonsf::geojson_sf(dat$desire_lines)
  
  return(dat)
  
}

