#' Process results
#'
#' @description Main function that the webtool uses to process resutls from the API
#'
#' @param args a json string as a character
#' @return a json string as a character 
#' @examples
#' \dontrun{
#' process_results("jsonhere")
#' }
#' @export


process_results = function(args) {
  
  t_start <- Sys.time()
  
  pas2080 <- data.frame(
    pas2080_code = c("A1-3","A4","A5",
                     "B1","B2","B3","B4","B5","B6","B7","B8","B9",
                     "C1","C2","C3","C4"),
    emissions = c(214192,54066,52729.2,0,6341,0,360,0,0,0,0,-4500,0,0,0,0),
    emissions_high = c(214192,54066,52729.2,0,6341,0,360,0,0,0,0,-4500,0,0,0,0)*1.1,
    emissions_low = c(214192,54066,52729.2,0,6341,0,360,0,0,0,0,-4500,0,0,0,0)*0.9,
    confidence = c("medium","medium","medium",
                   "not calculated","medium","medium","not calculated","not calculated",
                   "not calculated","not calculated","not calculated","low",
                   "not calculated","not calculated","not calculated","not calculated"),
    notes = "notes go here"
    )
  
  timeseries <- data.frame(year = 2022:2050,
                           emissions = c(327689,rep(-4500, 28)))
  timeseries$emissions_cumulative <- cumsum(timeseries$emissions)

  emissions_whole_life <- 187689
  payback_time <- 81
  netzero_compatible <- "No"
  comments <- "Construction carbon exceeds carbon saving by 2050, does not decarbonise fast enough"
  
  # Geometry to be plotted on the map
  geometry <- sf::st_sfc(list(sf::st_point(c(0,51.5))), crs = 4326)
  geometry <- sf::st_as_sf(data.frame(id = 1,
                                      message= "Test Geometry",
                                      type = "warning",
                                      geometry = geometry))
  
  geometry <- geojsonsf::sf_geojson(geometry)
  
  t_end <- Sys.time()
  processing_time <- as.numeric(difftime(t_end, t_start, units = "secs"))
  
  results <- list(pas2080,
                  timeseries,
                  payback_time,
                  emissions_whole_life,
                  netzero_compatible,
                  comments,
                  geometry,
                  processing_time)
  
  names(results) <- c("pas2080",
                      "timeseries",
                      "payback_time",
                      "emissions_whole_life",
                      "netzero_compatible",
                      "comments",
                      "geometry",
                      "processing_time")
  
  # Test if martin has updated the API
  args <- try(parse_json(args), silent = TRUE)
  if("try-error" %in% class(args)){
    args = gsub("[\r\n]", "", args[1])
    results$error <- args
  } else {
    results$error <- "API seems to be working. Enable the R package!"
  }
  
  
  results <- jsonlite::toJSON(results)
  return(results)
}
