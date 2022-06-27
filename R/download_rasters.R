#' Download Rasters for local use
#'
#' When running the package locally you need the raster data. This function
#' download the data and stores it in the package folder.
#'
#' @examples
#' \dontrun{
#' download_rasters()
#' }
#' @export

download_rasters <- function(){
  
  path_git <- "https://github.com/SDCA-tool/sdca-data/releases/download/map_data/"
  path_lib = c(.libPaths()[1])
  
  message("Downloading Bedrock")
  utils::download.file(url = paste0(path_git,"bedrock.tif.zip"), 
                destfile = paste0(path_lib,"/sdca/exdata/", "bedrock.tif.zip"), 
                mode = "wb")
  
  message("Downloading Land Cover")
  utils::download.file(url = paste0(path_git,"landcover.tif.zip"), 
                destfile = paste0(path_lib,"/sdca/exdata/", "landcover.tif.zip"), 
                mode = "wb")
  
  message("Downloading Superficial Bedrock")
  utils::download.file(url = paste0(path_git,"superficial.tif.zip"), 
                destfile = paste0(path_lib,"/sdca/exdata/", "superficial.tif.zip"), 
                mode = "wb")
  
  message("Downloading Terrain")
  utils::download.file(url = paste0(path_git,"UKdem.tif.zip"), 
                destfile = paste0(path_lib,"/sdca/exdata/", "UKdem.tif.zip"), 
                mode = "wb")
  
  message("Unzipping Bedrock")
  utils::unzip(paste0(path_lib,"/sdca/exdata/", "bedrock.tif.zip"),
               exdir = paste0(path_lib,"/sdca/exdata"))
  unlink(paste0(path_lib,"/sdca/exdata/", "bedrock.tif.zip"))
  
  message("Unzipping Land Cover")
  utils::unzip(paste0(path_lib,"/sdca/exdata/", "landcover.tif.zip"),
               exdir = paste0(path_lib,"/sdca/exdata"))
  unlink(paste0(path_lib,"/sdca/exdata/", "landcover.tif.zip"))
  
  message("Unzipping Superficial Bedrock")
  utils::unzip(paste0(path_lib,"/sdca/exdata/", "superficial.tif.zip"),
               exdir = paste0(path_lib,"/sdca/exdata"))
  unlink(paste0(path_lib,"/sdca/exdata/", "superficial.tif.zip"))
  
  message("Unzipping Terrain")
  utils::unzip(paste0(path_lib,"/sdca/exdata/", "UKdem.tif.zip"),
               exdir = paste0(path_lib,"/sdca/exdata"))
  unlink(paste0(path_lib,"/sdca/exdata/", "UKdem.tif.zip"))
}

