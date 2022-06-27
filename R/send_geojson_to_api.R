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
  #json <- geojsonsf::geojson_sf(path)
  #json <- geojsonsf::sf_geojson(json)
  
  json = readLines(path)
  json = gsub("\t","",json, fixed = TRUE)
  json = paste(json, collapse = "")
  json = utils::URLencode(json, reserved = TRUE)
  
  # Build URL
  url2 <- paste0(url,json)
  if(showinput){
    url2 <- paste0(url2,"&showinput=true")
  }
  #url2 <- utils::URLencode(url2, reserved = TRUE)
  
  # text <- httr::GET(url2)
  # text <- httr::content(text)
  text <- curl::curl_fetch_memory(url2)
  text <- rawToChar(text$content)
  
  dat <- jsonlite::fromJSON(text)
  dat$user_input <- geojsonsf::geojson_sf(dat$user_input)
  dat$desire_lines <- geojsonsf::geojson_sf(dat$desire_lines)
  
  # Change raster paths
  dat$path_dem <- paste0(.libPaths()[1],"/sdca/exdata/UKdem.tif")
  dat$path_landcover <- paste0(.libPaths()[1],"/sdca/exdata/landcover.tif")
  dat$path_bedrock <- paste0(.libPaths()[1],"/sdca/exdata/bedrock.tif")
  dat$path_superficial <- paste0(.libPaths()[1],"/sdca/exdata/superficial.tif")
  
  return(dat)
  
}

