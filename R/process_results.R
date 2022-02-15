#' Process results
#'
#' @description Main function that the webtool uses to process resutls from the API
#'
#' @param args a json string as a character
#' @param file logical, is json a file path
#' @return a json string as a character 
#' @examples
#' \dontrun{
#' process_results("jsonhere")
#' }
#' @export
process_results = function(args, file = FALSE) {
  
  t_start <- Sys.time()
  # Step 1: Parse the Input JSON
  dat = parse_json(args, file)
  
  # Step 2: Check inputs
  checkmate::assert_data_frame(dat$user_input, min.rows = 1)
  checkmate::assert_data_frame(dat$intervention_assets, min.rows = 1)
  checkmate::assert_data_frame(dat$intervention_assets_parameters, min.rows = 0)
  checkmate::assert_data_frame(dat$asset_components, min.rows = 1)
  checkmate::assert_data_frame(dat$carbon_factors, min.rows = 1)
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
                           assets = dat$intervention_assets,
                           intervention_assets_parameters = dat$intervention_assets_parameters,
                           components = dat$asset_components,
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
  res_demand = estimate_travel_demand(infra = dat$user_input,
                                      desire = dat$desire_lines)
  
  
  emissions = c(material_emissions_total$A1_3_emissions,
                material_emissions_total$A4_emissions,
                material_emissions_total$A5_emissions +
                  cut_fill_emissions_total$carbon_cut +
                  cut_fill_emissions_total$carbon_processing +
                  cut_fill_emissions_total$carbon_fill +
                  landcover_emissions_total$emissions_total,
                0,
                1,
                0,
                material_emissions_total$B4_emissions,
                0,0,0,0,
                res_demand$emissions_increase * 120,
                0,0,0,0)
  
  emissions <- emissions / 1000 # COnvert to Tonnes CO2 for output
  
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
  pas2080$emissions <- round(pas2080$emissions)
  pas2080$emissions_high <- round(pas2080$emissions_high)
  pas2080$emissions_low <- round(pas2080$emissions_low)
  
  timeseries <- data.frame(year = 2022:2100,
                           emissions = round(c(sum(emissions[c(1:11,13:16)], na.rm = TRUE) - res_demand$emissions_net/1000,
                                         rep(res_demand$emissions_net/1000, 78))))
  timeseries$emissions_cumulative <- cumsum(timeseries$emissions)
  
  emissions_whole_life <- sum(timeseries$emissions, na.rm = TRUE)
  
  
  payback_time <- round(sum(emissions[c(1:11,13:16)], na.rm = TRUE) / 
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
  } else if(payback_time < 150){
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
  
  results <- list(netzero_compatible,
                  payback_time,
                  emissions_whole_life,
                  comments,
                  processing_time,
                  pas2080,
                  timeseries,
                  res_demand$emissions_total,
                  materials_itemised,
                  landcover_emissions,
                  cut_fill_emissions,
                  geometry)
  
  names(results) <- c("netzero_compatible",
                      "payback_time",
                      "emissions_whole_life",
                      "comments",
                      "processing_time",
                      "pas2080",
                      "timeseries",
                      "demand_change",
                      "itemised_emissions",
                      "landcover_emissions",
                      "cut_fill_emissions",
                      "geometry")
  
  results <- jsonlite::toJSON(results)
  return(results)
}



