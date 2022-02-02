#' Estimate Travel Demand
#'
#' @description Estimate the travel deamnd from desire lines
#'
#' @param infra a sf dataframe of infrastucutre
#' @param desire as sf dataframe of desire lines
#' @param stop_buff_dist distance in metres to buffer stops
#' @param infra_buff_dist distance in metres to buffer infrastructure
#' @return a dataframe of
#' @export

estimate_travel_demand <- function(infra, 
                                   desire, 
                                   stop_buff_dist = 3000,
                                   infra_buff_dist = 3000){
  
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
    
    buff_straight = sf::st_buffer(infra_straight, 
                                  infra_buff_dist,
                                  endCapStyle = "SQUARE")
    
    # plot(desire$geometry)
    desire = desire[buff_straight, op = sf::st_within,]
    
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
  
  desire$length_km <- as.numeric(sf::st_length(desire)) / 1000
  desire <- sf::st_drop_geometry(desire)
  
  desire$from <- NULL
  desire$to <- NULL
  desire$bearing <- NULL
  desire$parallel <- NULL
  desire$all <- NULL
  
  names(desire)[1:8] <- paste0(names(desire)[1:8],"_before")
  
  # Estimate New mode splits
  mode_shifts <- NULL
  utils::data("mode_shifts", envir=environment())
  mode_shifts <- mode_shifts[mode_shifts$infrastructure == "railway", ]
  
  # Estimate new number of trips
  desire$cycle_after <- round(desire$cycle_before * (mode_shifts$cycle + 100)/100, 0)
  desire$drive_after <- round(desire$drive_before * (mode_shifts$drive + 100)/100, 0)
  desire$passenger_after <- round(desire$passenger_before * (mode_shifts$passenger + 100)/100, 0)
  desire$walk_after <- round(desire$walk_before * (mode_shifts$walk + 100)/100, 0)
  desire$rail_after <- round(desire$rail_before * (mode_shifts$rail + 100)/100, 0)
  desire$bus_after <- round(desire$bus_before * (mode_shifts$bus + 100)/100, 0)
  desire$lgv_after <- round(desire$lgv_before * (mode_shifts$lgv + 100)/100, 0)
  desire$hgv_after <- round(desire$hgv_before * (mode_shifts$hgv + 100)/100, 0)
  
  # Change in number of trips
  desire$cycle_change <- desire$cycle_after - desire$cycle_before
  desire$drive_change <- desire$drive_after - desire$drive_before
  desire$passenger_change <- desire$passenger_after - desire$passenger_before
  desire$walk_change <- desire$walk_after - desire$walk_before
  desire$rail_change <- desire$rail_after - desire$rail_before
  desire$bus_change <- desire$bus_after - desire$bus_before
  desire$lgv_change <- desire$lgv_after - desire$lgv_before
  desire$hgv_change <- desire$hgv_after - desire$hgv_before
  
  # Move those trips to rail
  # TODO: Relace with mulimodal solution
  desire$rail_change <- desire$rail_change - 
    desire$cycle_change - 
    desire$drive_change - 
    desire$passenger_change - 
    desire$walk_change - 
    desire$bus_change -
    desire$lgv_change -
    desire$hgv_change
  
  # Add some induced demand
  induced_demand <- 10 # 10% induced demand on top of mode shift
  
  desire$rail_change <- round(desire$rail_change * (1 + induced_demand/100),0)
  
  desire$rail_after <- desire$rail_change - desire$rail_before
  
  # Change in trip km, x1.4 to account for circuity
  desire$cycle_changekm <- desire$cycle_change * desire$length_km * 1.4
  desire$drive_changekm <- desire$drive_change * desire$length_km * 1.4
  desire$passenger_changekm <- desire$passenger_change * desire$length_km * 1.4
  desire$walk_changekm <- desire$walk_change * desire$length_km * 1.4
  desire$rail_changekm <- desire$rail_change * desire$length_km * 1.4
  desire$bus_changekm <- desire$bus_change * desire$length_km * 1.4
  desire$lgv_changekm <- desire$lgv_change * desire$length_km * 1.4
  desire$hgv_changekm <- desire$hgv_change * desire$length_km * 1.4
  
  # Emission factors kg/km DEFRA 2020
  utils::data("emissions_factors", envir=environment())
  # emissions_factors <- data.frame(cycle = 0,
  #                           drive = 0.17431,
  #                           passenger = 0,
  #                           walk = 0,
  #                           rail = 0.03549 ,
  #                           bus = 0.10227,
  #                           lgv = 0.24116,
  #                           hgv = 0.86407)
  
  # Change in emissions 
  desire$cycle_changeemissions = desire$cycle_changekm * emissions_factors$cycle * 365
  desire$drive_changeemissions = desire$drive_changekm * emissions_factors$drive * 365
  desire$passenger_changeemissions = desire$passenger_changekm * emissions_factors$passenger * 365
  desire$walk_changeemissions = desire$walk_changekm * emissions_factors$walk * 365
  desire$rail_changeemissions = desire$rail_changekm * emissions_factors$rail * 365
  desire$bus_changeemissions = desire$bus_changekm * emissions_factors$bus * 365
  desire$lgv_changeemissions = desire$lgv_changekm * emissions_factors$lgv * 365
  desire$hgv_changeemissions = desire$hgv_changekm * emissions_factors$hgv * 365
  
  
  desire$length_km <- NULL
  
  # Total Emissions in kgco2e / year
  emissions_total <- dplyr::summarise_all(desire, .funs = sum)
  emissions_total <- tidyr::pivot_longer(emissions_total, 
                          cols = tidyr::everything(),
                          names_to = c("mode",".value"),
                          names_pattern = "(.*)_(.*)")
  
  emissions_increase <- sum(emissions_total$changeemissions[emissions_total$changeemissions > 0])
  emissions_decrease <- sum(emissions_total$changeemissions[emissions_total$changeemissions < 0])
  emissions_net <- emissions_increase + emissions_decrease
  
  
  res <- list(emissions_increase, emissions_decrease, emissions_net,emissions_total)
  names(res) <- c("emissions_increase", "emissions_decrease", "emissions_net","emissions_total")
  
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


