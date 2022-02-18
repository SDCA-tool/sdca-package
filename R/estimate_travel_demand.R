#' Estimate Travel Demand
#'
#' @description Estimate the travel deamnd from desire lines
#'
#' @param infra a sf dataframe of infrastucutre
#' @param desire as sf dataframe of desire lines
#' @param stop_buff_dist distance in metres to buffer stops
#' @param infra_buff_dist_ratio distance in metres to buffer infrastructure
#' @return a dataframe of
#' @export

estimate_travel_demand <- function(infra, 
                                   desire, 
                                   stop_buff_dist = 3000,
                                   infra_buff_dist_ratio = 7){
  
  desire = desire[desire$from != desire$to, ]
  
  # Bug in R 3.6?
  desire = as.data.frame(desire)
  desire = sf::st_as_sf(desire, crs = 4326)
  
  # Split out Lines and non-lines
  infra_lines = infra[sf::st_geometry_type(infra$geometry) == "LINESTRING",]
  infra_stops = infra[sf::st_geometry_type(infra$geometry) == "POINT",]
  infra_stops = infra_stops[grepl("station",
                                  infra_stops$intervention, 
                                  ignore.case = TRUE),]
  
  if(nrow(infra_stops) > 0){
    # Station Mode - Demand is between stations
    onward_lines = infra_lines[infra_lines$intervention == "onward_travel",]
    if(nrow(onward_lines) > 0){
      onward_lines = sf::st_cast(onward_lines$geometry, "POINT")
      buff_stops = sf::st_buffer(c(infra_stops$geometry,onward_lines), stop_buff_dist)
    } else {
      buff_stops = sf::st_buffer(infra_stops, stop_buff_dist)
    }
    
    desire_points = suppressWarnings(sf::st_cast(desire[,c("from","to")], "POINT"))
    desire_points$msoa = ifelse(rep(c(TRUE,FALSE), nrow(desire)), 
                                desire_points$from, desire_points$to)
    desire_points = desire_points[!duplicated(desire_points$geometry),]
    desire_points = desire_points[buff_stops, op = sf::st_intersects]
    
    #plot(desire$geometry)
    desire = desire[desire$from %in% desire_points$msoa &
                      desire$to %in% desire_points$msoa, ]
    #plot(foo$geometry, col = "red", add = TRUE)
    #plot(buff_stops, add = TRUE)
    
  } else {
    # Road Mode - Demand is along infrastructure
    
    
    #Get straight line from infra
    infra_straight = linestring_to_line(infra_lines)
    
    # infra_buff_dist is ratio of length
    infra_buff_dist = sum(as.numeric(sf::st_length(infra))) / infra_buff_dist_ratio
    if(infra_buff_dist < 3000){
      infra_buff_dist = 3000
    }
    
    
    buff_straight = sf::st_buffer(infra_straight, 
                                  infra_buff_dist,
                                  endCapStyle = "SQUARE")
    
    # plot(desire$geometry)
    desire = desire[buff_straight, op = sf::st_within,]
    
    if(nrow(desire) == 0){
      return(make_empty_demand())
    } 
    
    # Get Bearings
    infra_straight$bearing = line_to_bearing(infra_straight)
    desire$bearing = line_to_bearing(desire)
    
    # Select those +/- 30 degrees in radians
    desire$parallel = (desire$bearing <= infra_straight$bearing + 0.523599 &
                         desire$bearing >= infra_straight$bearing - 0.523599)
    
    # plot(desire["parallel"])
    # plot(infra_lines$geometry, add = TRUE, col = "red", lwd = 3)
    desire <- desire[desire$parallel,]
    
  }
  
  # Check again for empty results
  if(nrow(desire) == 0){
    return(make_empty_demand())
  }
  
  
  desire$length_km <- as.numeric(sf::st_length(desire)) / 1000
  desire <- sf::st_drop_geometry(desire)
  
  #If active travel only consider short desire lines
  if(all(infra$mode_class %in% c("Active travel","onward_travel"))){
    desire <- desire[desire$length_km < 10,]
  }
  
  desire$from <- NULL
  desire$to <- NULL
  desire$bearing <- NULL
  desire$parallel <- NULL
  desire$all <- NULL
  
  names(desire)[1:8] <- paste0(names(desire)[1:8],"_before")
  
  # Estimate New mode splits
  mode_shifts <- NULL
  utils::data("mode_shifts", envir=environment())
  mode_shifts <- mode_shifts[mode_shifts$mode %in% infra$mode, ]
  mode_shifts <- mode_shifts[mode_shifts$intervention_class %in% infra$intervention_class, ]
  
  #Loop over each mode
  for(md in c("walk","cycle","drive","passenger","rail","bus")){
    induceddemand_low = desire[,paste0(md,"_before")] * mode_shifts$induceddemand_low[mode_shifts$travel_mode == md]
    induceddemand_average = desire[,paste0(md,"_before")] * mode_shifts$induceddemand_average[mode_shifts$travel_mode == md]
    induceddemand_high = desire[,paste0(md,"_before")] * mode_shifts$induceddemand_high[mode_shifts$travel_mode == md]
    
    modeshift_low = -desire[,paste0(md,"_before")] * mode_shifts$modeshift_low[mode_shifts$travel_mode == md]
    modeshift_average = -desire[,paste0(md,"_before")] * mode_shifts$modeshift_average[mode_shifts$travel_mode == md]
    modeshift_high = -desire[,paste0(md,"_before")] * mode_shifts$modeshift_high[mode_shifts$travel_mode == md]
    
    desire[,paste0(md,"_after-low")] = desire[,paste0(md,"_before")] + modeshift_low
    desire[,paste0(md,"_after-average")] = desire[,paste0(md,"_before")] + modeshift_average
    desire[,paste0(md,"_after-high")] = desire[,paste0(md,"_before")] + modeshift_high
    
    desire[,paste0(md,"_induceddemand-low")] = induceddemand_low
    desire[,paste0(md,"_induceddemand-average")] = induceddemand_average
    desire[,paste0(md,"_induceddemand-high")] = induceddemand_high
    
    #Change in number of travellers
    desire[,paste0(md,"_change-low")] = desire[,paste0(md,"_after-low")] - desire[,paste0(md,"_before")]
    desire[,paste0(md,"_change-average")] = desire[,paste0(md,"_after-average")] - desire[,paste0(md,"_before")]
    desire[,paste0(md,"_change-high")] = desire[,paste0(md,"_after-high")] - desire[,paste0(md,"_before")]
  }
  
  # Get the total change in trips and add to the infrastructure mode type
  desire$total_shifted_travellers_low = rowSums(desire[,grepl("_change-low",names(desire))])
  desire$total_shifted_travellers_average = rowSums(desire[,grepl("_change-average",names(desire))])
  desire$total_shifted_travellers_high = rowSums(desire[,grepl("_change-high",names(desire))])
  
  # Add the new travellers to the correct mode
  md = unique(mode_shifts$mode)
  if(md %in% c("High speed rail","Rail","Light Rail")){
    md = "rail"
  } else if(md %in% c("Walking")){
    md = "walk"
  } else if (md %in% c("Bus")){
    md = "bus"
  } else if (md %in% c("Road - Minor","Road - Major")){
    md = "drive"
  } else if (md %in% c("Bicycle")){
    md = "cycle"
  } else {
    stop("Unknown mode for demand modelling")
  }
  
  # Best case is lots of mode shift and little induced demand
  # For cars special case as mode shift is bad if high
  # Worst case is the opposite
  if(md == "drive"){
    desire[,paste0(md,"_after-low")] = desire[,paste0(md,"_after-low")] + 
      desire$total_shifted_travellers_high + desire[,paste0(md,"_induceddemand-high")]
    desire[,paste0(md,"_after-average")] = desire[,paste0(md,"_after-average")] + 
      desire$total_shifted_travellers_average + desire[,paste0(md,"_induceddemand-average")]
    desire[,paste0(md,"_after-high")] = desire[,paste0(md,"_after-high")] + 
      desire$total_shifted_travellers_low + desire[,paste0(md,"_induceddemand-low")]
  } else {
    desire[,paste0(md,"_after-low")] = desire[,paste0(md,"_after-low")] - 
      desire$total_shifted_travellers_low + desire[,paste0(md,"_induceddemand-high")]
    desire[,paste0(md,"_after-average")] = desire[,paste0(md,"_after-average")] - 
      desire$total_shifted_travellers_average + desire[,paste0(md,"_induceddemand-average")]
    desire[,paste0(md,"_after-high")] = desire[,paste0(md,"_after-high")] - 
      desire$total_shifted_travellers_high + desire[,paste0(md,"_induceddemand-low")]
  }
  
  # Update the change in travellers
  desire[,paste0(md,"_change-low")] = desire[,paste0(md,"_after-low")] - desire[,paste0(md,"_before")]
  desire[,paste0(md,"_change-average")] = desire[,paste0(md,"_after-average")] - desire[,paste0(md,"_before")]
  desire[,paste0(md,"_change-high")] = desire[,paste0(md,"_after-high")] - desire[,paste0(md,"_before")]
  
  # Estimate change in km and emissions
  # 1.4 circuity factor
  for(md in c("walk","cycle","drive","passenger","rail","bus")){
    desire[,paste0(md,"_changekm-low")] = desire[,paste0(md,"_change-low")] * desire$length_km * 1.4
    desire[,paste0(md,"_changekm-average")] = desire[,paste0(md,"_change-average")] * desire$length_km * 1.4 
    desire[,paste0(md,"_changekm-high")] = desire[,paste0(md,"_change-high")] * desire$length_km * 1.4
  }
  
  
  # Emission factors kg/km DEFRA 2020
  utils::data("emissions_factors", envir=environment())
  
  # Also switch to years of emissions
  for(md in c("walk","cycle","drive","passenger","rail","bus")){
    desire[,paste0(md,"_changeemissions-low")] = desire[,paste0(md,"_changekm-low")] * emissions_factors[,md] * 365
    desire[,paste0(md,"_changeemissions-average")] = desire[,paste0(md,"_changekm-average")] * emissions_factors[,md] * 365
    desire[,paste0(md,"_changeemissions-high")] = desire[,paste0(md,"_changekm-high")] * emissions_factors[,md] * 365
  }

  desire$length_km <- NULL
  
  # Total Emissions in kgco2e / year
  emissions_total <- dplyr::summarise_all(desire, .funs = sum, na.rm = TRUE)
  emissions_total <- tidyr::pivot_longer(emissions_total, 
                          cols = tidyr::everything(),
                          names_to = c("mode",".value"),
                          names_pattern = "(.*)_(.*)")
  
  emissions_total <- emissions_total[emissions_total$mode != "total_shifted_travellers",]
  emissions_total <- emissions_total[,c("mode","before","after-low","after-average","after-high",
                                        "changeemissions-low","changeemissions-average","changeemissions-high")]
  names(emissions_total) <- c("mode","before","after_low","after_average","after_high",
                              "changeemissions_low","changeemissions_average","changeemissions_high")
  
  
  emissions_increase <- sum(emissions_total$`changeemissions_average`[emissions_total$`changeemissions_average` > 0], na.rm = TRUE)
  emissions_decrease <- sum(emissions_total$`changeemissions_average`[emissions_total$`changeemissions_average` < 0], na.rm = TRUE)
  emissions_net <- emissions_increase + emissions_decrease
  
  emissions_increase_low <- sum(emissions_total$`changeemissions_low`[emissions_total$`changeemissions_low` > 0], na.rm = TRUE)
  emissions_decrease_low <- sum(emissions_total$`changeemissions_low`[emissions_total$`changeemissions_low` < 0], na.rm = TRUE)
  emissions_net_low <- emissions_increase_low + emissions_decrease_low
  
  emissions_increase_high <- sum(emissions_total$changeemissions_high[emissions_total$changeemissions_high > 0], na.rm = TRUE)
  emissions_decrease_high <- sum(emissions_total$changeemissions_high[emissions_total$changeemissions_high < 0], na.rm = TRUE)
  emissions_net_high <- emissions_increase_high + emissions_decrease_high
  
  emissions_total[2:ncol(emissions_total)] <- lapply(emissions_total[2:ncol(emissions_total)], round)
  
  res <- list(emissions_increase, emissions_decrease, emissions_net,
              emissions_increase_low, emissions_decrease_low, emissions_net_low,
              emissions_increase_high, emissions_decrease_high, emissions_net_high,
              emissions_total)
  names(res) <- c("emissions_increase", "emissions_decrease", "emissions_net",
                  "emissions_increase_low", "emissions_decrease_low", "emissions_net_low",
                  "emissions_increase_high", "emissions_decrease_high", "emissions_net_high",
                  "emissions_total")
  
  return(res)
}

#' Convert linestring into straight line
#'
#'
#' @param l linestring
#' @return a sf dataframe
linestring_to_line <- function(l){
  crs <- sf::st_crs(l)
  l <- sf::st_coordinates(l)
  l <- l[,c("X","Y")]
  l <- l[c(1,nrow(l)),]
  l <- sf::st_linestring(l)
  l <- sf::st_as_sfc(list(l), crs = crs)
  l <- sf::st_as_sf(data.frame(geometry = l))
}


#' Convert lines to bearings
#'
#' Only works for straight lines
#'
#' @param l linestring
#' @param simplify logical, make all positive and rotate 90 degrees
#' @return numeric between -pi and +pi
line_to_bearing <- function(l, simplify = TRUE){
  p <- sf::st_cast(sf::st_geometry(l), "POINT")
  bearing <- lwgeom::st_geod_azimuth(p)
  bearing <- bearing[seq(1,length(bearing), by = 2)]
  bearing <- as.numeric(bearing)
  if(simplify){
    bearing <- ifelse(bearing >= 0, 
          bearing,
          bearing +3.141593) + 1.570796
  }
  return(bearing)
}


