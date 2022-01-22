#' Estimate Travel Demand
#'
#' @description Estimate the travel deamnd from desire lines
#'
#' @param infra a sf dataframe of infrastucutre
#' @param desire as sf dataframe of desire lines
#' @return a dataframe of
#' @examples
#' \dontrun{
#' process_results("jsonhere")
#' }
#' @export

estimate_travel_demand <- function(infra, desire){
  
  #Get straight line from infra
  infra_straight = linestring_to_line(infra)
  buff_straight = sf::st_buffer(infra_straight, 2000)
  desire = desire[buff_straight, op = sf::st_within,]
  
  # Get Bearings
  infra_straight$bearing = line_to_bearing(infra_straight)
  infra_straight$bearing = ifelse(infra_straight$bearing >= 0, 
                                      infra_straight$bearing,
                                      infra_straight$bearing +3.141593)
  infra_straight$bearing = infra_straight$bearing + 1.570796
  desire$bearing = line_to_bearing(desire)
  desire$bearing = ifelse(desire$bearing >= 0, 
                          desire$bearing,
                          desire$bearing +3.141593) 
  desire$bearing = desire$bearing + 1.570796
  # Select those +/- 30 degrees in radians
  desire$parallel = (desire$bearing <= infra_straight$bearing + 0.523599 &
                             desire$bearing >= infra_straight$bearing - 0.523599)
  
  #qtm(desire, lines.col = "parallel") + qtm(infra_straight, col= "red") + qtm(buff, fill = NULL)
  desire <- desire[desire$parallel,]
  desire$length <- as.numeric(sf::st_length(desire)) / 1000
  
  # Estimate New mode splits
  mode_shifts <- data.frame(cycle = -2,
                            drive = -15,
                            passenger = -15,
                            walk = 0,
                            rail = 0,
                            bus = -30,
                            lgv = 0,
                            hgv = 0)
  
  # Estimate new number of trips
  desire$cycle_new <- round(desire$cycle * (mode_shifts$cycle + 100)/100, 0)
  desire$drive_new <- round(desire$drive * (mode_shifts$drive + 100)/100, 0)
  desire$passenger_new <- round(desire$passenger * (mode_shifts$passenger + 100)/100, 0)
  desire$walk_new <- round(desire$walk * (mode_shifts$walk + 100)/100, 0)
  desire$rail_new <- round(desire$rail * (mode_shifts$rail + 100)/100, 0)
  desire$bus_new <- round(desire$bus * (mode_shifts$bus + 100)/100, 0)
  desire$lgv_new <- round(desire$lgv * (mode_shifts$lgv + 100)/100, 0)
  desire$hgv_new <- round(desire$hgv * (mode_shifts$hgv + 100)/100, 0)
  
  # Change in number of trips
  desire$cycle_change <- desire$cycle_new - desire$cycle
  desire$drive_change <- desire$drive_new - desire$drive
  desire$passenger_change <- desire$passenger_new - desire$passenger
  desire$walk_change <- desire$walk_new - desire$walk
  desire$rail_change <- desire$rail_new - desire$rail
  desire$bus_change <- desire$bus_new - desire$bus
  desire$lgv_change <- desire$lgv_new - desire$lgv
  desire$hgv_change <- desire$hgv_new - desire$hgv
  
  # Move those trips to rail
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
  
  # Change in trip km, x1.4 to account for circuity
  desire$cycle_change_km <- desire$cycle_change * desire$length * 1.4
  desire$drive_change_km <- desire$drive_change * desire$length * 1.4
  desire$passenger_change_km <- desire$passenger_change * desire$length * 1.4
  desire$walk_change_km <- desire$walk_change * desire$length * 1.4
  desire$rail_change_km <- desire$rail_change * desire$length * 1.4
  desire$bus_change_km <- desire$bus_change * desire$length * 1.4
  desire$lgv_change_km <- desire$lgv_change * desire$length * 1.4
  desire$hgv_change_km <- desire$hgv_change * desire$length * 1.4
  
  # Emission factors kg/km DEFRA 2020
  emissions_factors <- data.frame(cycle = 0,
                            drive = 0.17431,
                            passenger = 0,
                            walk = 0,
                            rail = 0.03549 ,
                            bus = 0.10227,
                            lgv = 0.24116,
                            hgv = 0.86407)
  
  # Change in emissions 
  desire$cycle_change_emissions = desire$cycle_change_km * emissions_factors$cycle * 365
  desire$drive_change_emissions = desire$drive_change_km * emissions_factors$drive * 365
  desire$passenger_change_emissions = desire$passenger_change_km * emissions_factors$passenger * 365
  desire$walk_change_emissions = desire$walk_change_km * emissions_factors$walk * 365
  desire$rail_change_emissions = desire$rail_change_km * emissions_factors$rail * 365
  desire$bus_change_emissions = desire$bus_change_km * emissions_factors$bus * 365
  desire$lgv_change_emissions = desire$lgv_change_km * emissions_factors$lgv * 365
  desire$hgv_change_emissions = desire$hgv_change_km * emissions_factors$hgv * 365
  
  # Total Emissions in kgco2e / year
  emissions_total <- sf::st_drop_geometry(desire)
  emissions_total <- emissions_total[,c("cycle_change_emissions",
                                        "drive_change_emissions",
                                        "passenger_change_emissions",
                                        "walk_change_emissions",
                                        "rail_change_emissions",
                                        "bus_change_emissions",
                                        "lgv_change_emissions",
                                        "hgv_change_emissions")]
  emissions_total <- dplyr::summarise_all(emissions_total, .funs = sum)
  emissions_total <- tidyr::pivot_longer(emissions_total, cols = tidyr::everything())
  emissions_increase <- sum(emissions_total$value[emissions_total$value > 0])
  emissions_decrease <- sum(emissions_total$value[emissions_total$value < 0])
  emissions_net <- emissions_increase - emissions_decrease
  
  
  return(emissions_net)
    # desire_total <- sf::st_drop_geometry(desire)
    # desire_total <- desire_total[,c("cycle","drive","passenger","walk","rail","bus","lgv","hgv")]
    # desire_total <- dplyr::summarise_all(desire_total, .funs = sum)
  
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
#' @return numeric between -pi and +pi
line_to_bearing <- function(l){
  p <- sf::st_cast(sf::st_geometry(l), "POINT")
  bearing <- lwgeom::st_geod_azimuth(p)
  bearing <- bearing[seq(1,length(bearing), by = 2)]
  return(as.numeric(bearing))
}
