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
  
  dat = parse_json(args)
  
  res_materials = evaluate_materials(infra = dat$user_input,
                                     intervention_assets = dat$intervention_assets,
                                     intervention_assets_parameters = dat$intervention_assets_parameters,
                                     asset_components = dat$asset_components,
                                     carbon_factors = dat$carbon_factors,
                                     material_sites = dat$material_sites)
  
  res_demand = estimate_travel_demand(infra = dat$user_input,
                                      desire = dat$desire_lines)
  
  
  
  emissions = c(res_materials$A1_3_emissions,
                res_materials$A4_emissions,
                res_materials$A5_emissions,
                0,
                1,
                0,
                res_materials$B4_emissions,
                0,0,0,0,
                res_demand$emissions_net,
                0,0,0,0)
  
  emissions <- emissions / 1000
  
  pas2080 <- data.frame(
    pas2080_code = c("A1-3","A4","A5",
                     "B1","B2","B3","B4","B5","B6","B7","B8","B9",
                     "C1","C2","C3","C4"),
    emissions = emissions,
    emissions_high = emissions*1.1,
    emissions_low = emissions*0.9,
    confidence = c("medium","medium","medium",
                   "not calculated","medium","medium","not calculated","not calculated",
                   "not calculated","not calculated","not calculated","low",
                   "not calculated","not calculated","not calculated","not calculated"),
    notes = "notes go here"
  )
  
  timeseries <- data.frame(year = 2022:2100,
                           emissions = round(c(sum(emissions, na.rm = TRUE) - res_demand$emissions_net/1000,
                                         rep(res_demand$emissions_net/1000, 78))))
  timeseries$emissions_cumulative <- cumsum(timeseries$emissions)
  
  emissions_whole_life <- sum(timeseries$emissions, na.rm = TRUE)
  
  
  payback_time <- round((sum(emissions, na.rm = TRUE) - 
                           res_demand$emissions_net/1000) / 
                          ( - res_demand$emissions_net/1000))
  if(payback_time < 0){
    payback_time <- 9999999
  }
  netzero_compatible <- ifelse(payback_time > 28,"no","yes")
  if(payback_time < 18){
    comments <- "Project pays back well before 2050"
  } else if(payback_time < 28){
    comments <- "Project pays back slowly but before 2050"
  } else if(payback_time < 38){
    comments <- "Project pays back slowly but after 2050"
  } else if(payback_time < 78){
    comments <- "Project pays back very slowly"
  } else if(payback_time == 9999999){
    comments <- "Project permenantly increases the UK's carbon footprint"
  } else{
    comments <- "Project never pays back"
  }
  
  
  
  
  
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
  
  results <- jsonlite::toJSON(results)
  return(results)
}



process_results2 = function(args) {
  
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
