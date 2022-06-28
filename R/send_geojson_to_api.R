#' Send to API
#'
#' @description Function that take JSON from the API and convert into R format
#'
#' @param path path to geojson downloaded from website
#' @param url API url
#' @param showinput logical, if TRUE input to R package is returned else resutls
#'   are returned
#' @param path_dem path to the terrain raster, defaults to location set by `download_rasters()`
#' @param path_landcover path to the land cover raster, defaults to location set by `download_rasters()`
#' @param path_bedrock path to the bedrock raster, defaults to location set by `download_rasters()`
#' @param path_superficial path to the superficial raster, defaults to location set by `download_rasters()`
#' @return a named list of data frames and other objects
#' @examples
#' \dontrun{
#' parse_json("jsonhere")
#' }
#' @export

geojson_api <- function(path, 
                        url = "https://dev.carbon.place/api/v1/locations.json?geojson=",
                        showinput = TRUE,
                        path_dem = paste0(.libPaths()[1],"/sdca/exdata/UKdem.tif"),
                        path_landcover = paste0(.libPaths()[1],"/sdca/exdata/landcover.tif"),
                        path_bedrock = paste0(.libPaths()[1],"/sdca/exdata/bedrock.tif"),
                        path_superficial = paste0(.libPaths()[1],"/sdca/exdata/superficial.tif")){
  # Reag GEOJSON
  #json <- geojsonsf::geojson_sf(path)
  #json <- geojsonsf::sf_geojson(json)
  
  suppressWarnings(json <- readLines(path))
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
  if(file.exists(path_dem)){
    dat$path_dem <- path_dem
  } else{
    warning("could not find terrain raster\n")
  }
  if(file.exists(path_landcover)){
    dat$path_landcover <- path_landcover
  } else{
    warning("could not find land cover raster\n")
  }
  if(file.exists(path_bedrock)){
    dat$path_bedrock <- path_bedrock
  } else{
    warning("could not find bedrock raster\n")
  }
  if(file.exists(path_superficial)){
    dat$path_superficial <- path_superficial
  } else{
    warning("could not find superficial raster\n")
  }

  return(dat)
  
}

