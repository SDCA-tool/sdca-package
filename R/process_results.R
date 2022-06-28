#' Process results
#'
#' @description Main function that the webtool uses to process resutls from the API
#'
#' @param args a json string as a character
#' @param file logical, is json a file path
#' @param local logical, are we running locally or on the server
#' @return a json string as a character 
#' @examples
#' \dontrun{
#' process_results("jsonhere")
#' }
#' @export
process_results = function(args, file = FALSE, local = FALSE) {
  
  t_start <- Sys.time()
  
  # Step 1: Parse the Input JSON
  if(local){
    dat = args
  } else {
    dat = parse_json(args, file = file)
  }
  
  
  # Step 2: Check inputs
  checkmate::assert_data_frame(dat$user_input, min.rows = 1)
  checkmate::assert_data_frame(dat$assets, min.rows = 1)
  #checkmate::assert_data_frame(dat$assets_parameters, min.rows = 0)
  checkmate::assert_data_frame(dat$components, min.rows = 1)
  #checkmate::assert_data_frame(dat$carbon_factors, min.rows = 1) # Can be empty
  checkmate::assert_data_frame(dat$material_sites, nrows = 11, ncol = 2)
  checkmate::assert_character(dat$path_dem, len = 1)
  checkmate::assert_file_exists(dat$path_dem)
  checkmate::assert_character(dat$path_landcover, len = 1)
  checkmate::assert_file_exists(dat$path_landcover)
  checkmate::assert_character(dat$path_bedrock, len = 1)
  checkmate::assert_file_exists(dat$path_bedrock)
  checkmate::assert_character(dat$path_superficial, len = 1)
  checkmate::assert_file_exists(dat$path_superficial)
  
  # Get Rasters
  rast_dem <- stars::read_stars(dat$path_dem)
  rast_bedrock <- stars::read_stars(dat$path_bedrock)
  rast_superficial <- stars::read_stars(dat$path_superficial)
  rast_landcover <- stars::read_stars(dat$path_landcover)
  
  
  # Step 3: Calculate construction emissions
  # Can consider each geometry separately so iterate over each one
  # TODO: Process designed for lines need point/polygon solution
  
  # Calculate Embodied Carbon for each row in Infra
  # Emissions in kgCO2
  infra_list = dat$user_input
  infra_list = split(infra_list, seq_len(nrow(infra_list)))
  
  construction_emissions = lapply(infra_list, 
                           measure_infrastucture, 
                           assets = dat$assets,
                           assets_parameters = dat$assets_parameters,
                           components = dat$components,
                           carbon_factors = dat$carbon_factors,
                           material_sites = dat$material_sites,
                           rast_dem = rast_dem,
                           rast_bedrock = rast_bedrock,
                           rast_superficial = rast_superficial,
                           rast_landcover = rast_landcover)
  
  material_emissions <- lapply(construction_emissions, function(x){x$material_emissions})
  material_emissions <- dplyr::bind_rows(material_emissions, .id = "intervention_id")
  
  materials_itemised <- lapply(construction_emissions, function(x){x$materials_itemised})
  materials_itemised <- dplyr::bind_rows(materials_itemised, .id = "intervention_id")
  
  landcover_emissions <- lapply(construction_emissions, function(x){x$landcover_emissions})
  landcover_emissions <- dplyr::bind_rows(landcover_emissions, .id = "intervention_id")
  
  cut_fill_emissions <- lapply(construction_emissions, function(x){x$cut_fill_emissions})
  cut_fill_emissions <- dplyr::bind_rows(cut_fill_emissions, .id = "intervention_id")
  
  geometry_errors <- lapply(construction_emissions, function(x){x$geometry_errors})
  geometry_errors <- dplyr::bind_rows(geometry_errors)
  
  # Add Up construction emissions
  material_emissions_total = material_emissions
  material_emissions_total$intervention_id = 1
  material_emissions_total = dplyr::group_by(material_emissions_total, intervention_id)
  material_emissions_total = dplyr::summarise_all(material_emissions_total, sum)
  
  landcover_emissions_total = landcover_emissions
  landcover_emissions_total$intervention_id = 1
  landcover_emissions_total = dplyr::group_by(landcover_emissions_total, intervention_id)
  landcover_emissions_total = dplyr::summarise_all(landcover_emissions_total, sum)
  
  cut_fill_emissions_total = cut_fill_emissions
  cut_fill_emissions_total$intervention_id = 1
  cut_fill_emissions_total = dplyr::group_by(cut_fill_emissions_total, intervention_id)
  cut_fill_emissions_total = dplyr::summarise_all(cut_fill_emissions_total, sum)
  
  # Calculate Mode Shift and Induced Demand
  # Emissions in kgCO2 per year
  if(nrow(dat$desire_lines) > 0){
    res_demand = estimate_travel_demand(infra = dat$user_input,
                                        desire = dat$desire_lines)
  } else {
    res_demand <- make_empty_demand()
    
  }
  
  # Do we need to do make a demand error?
  if(sum(res_demand$emissions_total$before) == 0){
    # Make an error message
    demand_check <- sf::st_centroid(sf::st_combine(dat$user_input))
    demand_check <- sf::st_as_sf(data.frame(id = 1,
                                            message = "Could not find any travel demand in this location, perhaps the area is too remote",
                                            type = "error",
                                            geometry = demand_check
    ))
    if(nrow(geometry_errors) > 0){
      geometry_errors <- rbind(geometry_errors, demand_check)
    } else{
      geometry_errors <- demand_check
    }
    
    
  }
  
  
  demand_emissions = res_demand$emissions_total
  # Convert to tonnes for website
  demand_emissions$changeemissions_low <- round(demand_emissions$changeemissions_low / 1000)
  demand_emissions$changeemissions_average <- round(demand_emissions$changeemissions_average / 1000)
  demand_emissions$changeemissions_high <- round(demand_emissions$changeemissions_high / 1000)
  
  emissions = c(landcover_emissions_total$emissions_total,
                 material_emissions_total$A1_3_emissions,
                material_emissions_total$A4_emissions,
                material_emissions_total$A5_emissions +
                  cut_fill_emissions_total$carbon_cut +
                  cut_fill_emissions_total$carbon_processing +
                  cut_fill_emissions_total$carbon_fill,
                0,
                material_emissions_total$B2_emissions,
                0,
                material_emissions_total$B4_emissions,
                0,0,0,0,
                res_demand$emissions_increase,
                0,0,0,0)
  
  emissions_low = c(landcover_emissions_total$emissions_total,
                     material_emissions_total$A1_3_emissions,
                material_emissions_total$A4_emissions,
                material_emissions_total$A5_emissions +
                  cut_fill_emissions_total$carbon_cut +
                  cut_fill_emissions_total$carbon_processing +
                  cut_fill_emissions_total$carbon_fill,
                0,
                material_emissions_total$B2_emissions,
                0,
                material_emissions_total$B4_emissions,
                0,0,0,0,
                res_demand$emissions_increase_low,
                0,0,0,0)
  
  emissions_high = c(landcover_emissions_total$emissions_total,
                      material_emissions_total$A1_3_emissions,
                material_emissions_total$A4_emissions,
                material_emissions_total$A5_emissions +
                  cut_fill_emissions_total$carbon_cut +
                  cut_fill_emissions_total$carbon_processing +
                  cut_fill_emissions_total$carbon_fill,
                0,
                material_emissions_total$B2_emissions,
                0,
                material_emissions_total$B4_emissions,
                0,0,0,0,
                res_demand$emissions_increase_high,
                0,0,0,0)
  
  emissions <- emissions / 1000 # COnvert to Tonnes CO2 for output
  emissions_low <- emissions_low / 1000
  emissions_high <- emissions_high / 1000
  
  pas2080 <- data.frame(
    pas2080_code = c("A0","A1-3","A4","A5",
                     "B1","B2","B3","B4","B5","B6","B7","B8","B9",
                     "C1","C2","C3","C4"),
    emissions = emissions,
    emissions_high = emissions_high,
    emissions_low = emissions_low,
    confidence = c("medium","medium","medium","medium",
                   "not calculated","medium","not calculated","medium","not calculated",
                   "not calculated","not calculated","not calculated","low",
                   "not calculated","not calculated","not calculated","not calculated")
  )
  pas2080$emissions <- dplyr::if_else(pas2080$emissions > 2,round(pas2080$emissions), round(pas2080$emissions, 2))
  pas2080$emissions_high <- dplyr::if_else(pas2080$emissions_high > 2,round(pas2080$emissions_high), round(pas2080$emissions_high, 2))
  pas2080$emissions_low <- dplyr::if_else(pas2080$emissions_low > 2,round(pas2080$emissions_low), round(pas2080$emissions_low, 2))
  
  timeseries <- data.frame(year = 2022:2100,
                           emissions = round(c(sum(emissions[c(1:11,13:16)], na.rm = TRUE) - res_demand$emissions_net/1000,
                                         rep(res_demand$emissions_net/1000, 78))),
                           emissions_low = round(c(sum(emissions_low[c(1:11,13:16)], na.rm = TRUE) - res_demand$emissions_net_low/1000,
                                               rep(res_demand$emissions_net_low/1000, 78))),
                           emissions_high = round(c(sum(emissions_high[c(1:11,13:16)], na.rm = TRUE) - res_demand$emissions_net_high/1000,
                                               rep(res_demand$emissions_net_high/1000, 78))))
  timeseries$emissions_cumulative <- cumsum(timeseries$emissions)
  timeseries$emissions_cumulative_low <- cumsum(timeseries$emissions_low)
  timeseries$emissions_cumulative_high <- cumsum(timeseries$emissions_high)
  
  # Headline Results
  
  emissions_upfront <- sum(pas2080$emissions[1:4], na.rm = TRUE)
  emissions_upfront_low <- sum(pas2080$emissions_low[1:4], na.rm = TRUE)
  emissions_upfront_high <- sum(pas2080$emissions_high[1:4], na.rm = TRUE)
  
  emissions_whole_life <- sum(pas2080$emissions, na.rm = TRUE)
  emissions_whole_life_low <- sum(pas2080$emissions_low, na.rm = TRUE)
  emissions_whole_life_high <- sum(pas2080$emissions_high, na.rm = TRUE)
  
  emissions_whole_life_benefits <- sum(timeseries$emissions, na.rm = TRUE)
  emissions_whole_life_benefits_low <- sum(timeseries$emissions_low, na.rm = TRUE)
  emissions_whole_life_benefits_high <- sum(timeseries$emissions_high, na.rm = TRUE)
  
  
  payback_time <- round(sum(emissions[c(1:11,13:16)], na.rm = TRUE) / 
                          ( - res_demand$emissions_net/1000))
  payback_time_low <- round(sum(emissions_low[c(1:11,13:16)], na.rm = TRUE) / 
                          ( - res_demand$emissions_net_low/1000))
  payback_time_high <- round(sum(emissions_high[c(1:11,13:16)], na.rm = TRUE) / 
                              ( - res_demand$emissions_net_high/1000))
  
  
  if(payback_time < 0){
    payback_time <- "Never"
  }
  if(payback_time_low < 0){
    payback_time <- "Never"
  }
  if(payback_time_high < 0){
    payback_time <- "Never"
  }
  
  if(is.character(payback_time)){
    netzero_compatible <- "no"
    comments <- "Project permanently increases the UK's carbon footprint"
  } else if(payback_time < 18){
    netzero_compatible <- "yes"
    comments <- "Project pays back well before 2050"
  } else if(payback_time < 28){
    netzero_compatible <- "yes"
    comments <- "Project pays back slowly but before 2050"
  } else if(payback_time < 38){
    netzero_compatible <- "no"
    comments <- "Project pays back slowly but after 2050"
  } else if(payback_time < 150){
    netzero_compatible <- "no"
    comments <- "Project pays back very slowly"
  } else{
    netzero_compatible <- "no"
    comments <- "Project never pays back"
  }
  
  
  # Geometry to be plotted on the map
  if(nrow(geometry_errors) > 0){
    geometry_errors <- geojsonsf::sf_geojson(geometry_errors)
  } else {
    geometry_errors <- '{"type": "FeatureCollection","features": []}'
  }
  
  t_end <- Sys.time()
  processing_time <- as.numeric(difftime(t_end, t_start, units = "secs"))
  
  results <- list(netzero_compatible,
                  payback_time,
                  payback_time_low,
                  payback_time_high,
                  emissions_upfront,
                  emissions_upfront_low,
                  emissions_upfront_high,
                  emissions_whole_life,
                  emissions_whole_life_low,
                  emissions_whole_life_high,
                  emissions_whole_life_benefits,
                  emissions_whole_life_benefits_low,
                  emissions_whole_life_benefits_high,
                  comments,
                  processing_time,
                  pas2080,
                  timeseries,
                  demand_emissions,
                  materials_itemised,
                  landcover_emissions,
                  cut_fill_emissions,
                  geometry_errors)
  
  names(results) <- c("netzero_compatible",
                      "payback_time",
                      "payback_time_low",
                      "payback_time_high",
                      "emissions_upfront",
                      "emissions_upfront_low",
                      "emissions_upfront_high",
                      "emissions_whole_life",
                      "emissions_whole_life_low",
                      "emissions_whole_life_high",
                      "emissions_whole_life_benefits",
                      "emissions_whole_life_benefits_low",
                      "emissions_whole_life_benefits_high",
                      "comments",
                      "processing_time",
                      "pas2080",
                      "timeseries",
                      "demand_change",
                      "itemised_emissions",
                      "landcover_emissions",
                      "cut_fill_emissions",
                      "geometry")
  
  results <- jsonlite::toJSON(results,
                              simplifyDataFrame = FALSE, 
                              simplifyVector = FALSE, 
                              na = "null")
  return(results)
}



