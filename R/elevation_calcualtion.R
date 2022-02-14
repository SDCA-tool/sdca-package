#' Extract elevations
#'
#' @description extract elevations of a line
#'
#' @param line a sf dataframe of linestrings
#' @param path_dem path to dem
#' @return a dataframe of heights
#' @export

extract_elevations <- function(line, path_dem){
  
  dem <- stars::read_stars(path_dem)
  line_split <- sf::st_segmentize(line, dfMaxLength = 50)
  line_split <- sf::st_cast(line_split, "POINT")

  heights <- stars::st_extract(dem, at = line_split)
  names(heights) <- c("elevation","geometry")
  heights$step <- seq(0, nrow(heights) - 1)
  gradient <- (heights$elevation[nrow(heights)] - heights$elevation[1])/(nrow(heights))
  heights$road <- heights$step * gradient + heights$elevation[1]
  heights$difference <- heights$elevation - heights$road
  coords <- sf::st_coordinates(heights)
  heights <- sf::st_drop_geometry(heights)
  heights <- cbind(heights, coords)
  return(heights)
}

#' Extract data from rasters
#'
#' @description extract elevations of a line
#'
#' @param infra a sf dataframe
#' @param rast_dem stars raster of  dem
#' @param rast_bedrock stars raster of  bedrock
#' @param rast_superficial stars raster of  superficial
#' @param rast_landcover stars raster of  landcover
#' @return a dataframe of heights
#' @export

extract_rasters <- function(infra, 
                            rast_dem, 
                            rast_bedrock, 
                            rast_superficial, 
                            rast_landcover){
  
  line_split <- sf::st_segmentize(infra, dfMaxLength = 50)
  line_split <- sf::st_cast(line_split, "POINT")
  
  vals_dem <- sf::st_drop_geometry(stars::st_extract(rast_dem, at = line_split))
  vals_bedrock <- sf::st_drop_geometry(stars::st_extract(rast_bedrock, at = line_split))
  vals_superficial <- sf::st_drop_geometry(stars::st_extract(rast_superficial, at = line_split))
  vals_landcover <- sf::st_drop_geometry(stars::st_extract(rast_landcover, at = line_split))
  
  names(vals_dem) = "elevation"
  names(vals_bedrock) = "bedrock"
  names(vals_superficial) = "superficial"
  names(vals_landcover) = "landcover"
  
  utils::data("bedrock", envir=environment())
  utils::data("superficial", envir=environment())
  utils::data("landcover_factors", envir=environment())
  
  names(bedrock) <- paste0("br_",names(bedrock))
  names(superficial) <- paste0("sp_",names(superficial))
  
  vals_bedrock = dplyr::left_join(vals_bedrock, bedrock, by = c("bedrock" = "br_id"))
  vals_superficial = dplyr::left_join(vals_superficial, superficial, by = c("superficial" = "sp_id"))
  vals_landcover = dplyr::left_join(vals_landcover, landcover_factors, by = c("landcover" = "landcover_id"))
  
  vals_fin = cbind(vals_dem, vals_bedrock, vals_superficial, vals_landcover)
  vals_fin$geometry = line_split$geometry
  vals_fin = sf::st_as_sf(vals_fin)
  
  return(vals_fin)
}





#' Infer cut and fill
#'
#' @description extract elevations of a line
#'
#' @param elevations a dataframe of elevation data
#' @param width width of infrastrucutre
#' @return a dataframe of cut and fill.
#' @export
cut_fill <- function(raster_data, width = 19){
  
  raster_data$sp_thickness[is.na(raster_data$sp_thickness)] <- 0
  
  # Calculate the Cut Volume
  # +ve difference means cut, -ve means fill
  raster_data$cut_depth_sp = ifelse(raster_data$difference > raster_data$sp_thickness,
                                    raster_data$sp_thickness,
                                    raster_data$difference)
  raster_data$cut_depth_sp[raster_data$cut_depth_sp < 0] = 0
  
  raster_data$cut_depth_br = ifelse(raster_data$difference > raster_data$sp_thickness,
                                    raster_data$difference - raster_data$sp_thickness,
                                    0)
  raster_data$cut_depth_br[raster_data$cut_depth_br < 0] = 0
  
  raster_data$cut_area_br = (raster_data$cut_depth_br * width) + 
    (raster_data$cut_depth_br * raster_data$cut_depth_br / tan(raster_data$br_angle * pi/180))
  
  raster_data$cut_width_sp = raster_data$cut_depth_br / tan(raster_data$br_angle * pi/180) + width
  
  raster_data$cut_area_sp = (raster_data$cut_depth_sp * raster_data$cut_width_sp) + 
    (raster_data$cut_depth_sp * raster_data$cut_depth_sp / tan(raster_data$sp_angle * pi/180))
  
  raster_data$cut_volume_br = raster_data$cut_area_br * raster_data$distance
  raster_data$cut_volume_sp = raster_data$cut_area_sp * raster_data$distance
  
  
  # Calculate the volume of materials from the cut that can be used in fill
  raster_data$br_type_0_vol = raster_data$br_type_0 * raster_data$cut_volume_br
  raster_data$br_type_1_vol = raster_data$br_type_1 * raster_data$cut_volume_br
  raster_data$br_type_2_vol = raster_data$br_type_2 * raster_data$cut_volume_br
  raster_data$br_type_3_vol = raster_data$br_type_3 * raster_data$cut_volume_br
  raster_data$br_type_6_vol = raster_data$br_type_6 * raster_data$cut_volume_br
  raster_data$br_type_7_vol = raster_data$br_type_7 * raster_data$cut_volume_br
  raster_data$br_type_8_vol = raster_data$br_type_8 * raster_data$cut_volume_br
  raster_data$br_type_9_vol = raster_data$br_type_9 * raster_data$cut_volume_br
  
  raster_data$sp_type_0_vol = raster_data$sp_type_0 * raster_data$cut_volume_sp
  raster_data$sp_type_1_vol = raster_data$sp_type_1 * raster_data$cut_volume_sp
  raster_data$sp_type_2_vol = raster_data$sp_type_2 * raster_data$cut_volume_sp
  raster_data$sp_type_3_vol = raster_data$sp_type_3 * raster_data$cut_volume_sp
  raster_data$sp_type_6_vol = raster_data$sp_type_6 * raster_data$cut_volume_sp
  raster_data$sp_type_7_vol = raster_data$sp_type_7 * raster_data$cut_volume_sp
  raster_data$sp_type_8_vol = raster_data$sp_type_8 * raster_data$cut_volume_sp
  raster_data$sp_type_9_vol = raster_data$sp_type_9 * raster_data$cut_volume_sp
  
  # Calculate carbon for cut
  raster_data$br_cut_carbon = raster_data$cut_volume_br * raster_data$br_cut
  raster_data$sp_cut_carbon = raster_data$cut_volume_sp * raster_data$sp_cut
  
  # Calculate the fill requirements
  raster_data$fill_height = ifelse(raster_data$difference < 0,
                                    -raster_data$difference, 0)
  
  raster_data$fill_area = (raster_data$fill_height * width) + 
    (raster_data$fill_height * raster_data$fill_height * 1.732051)
  
  raster_data$fill_area_start = width + 3.464102 * raster_data$fill_height
  raster_data$fill_area_cap = 2 * raster_data$fill_height + width/2
  raster_data$fill_area_general = raster_data$fill_area - 
    raster_data$fill_area_start - 
    raster_data$fill_area_cap
  
  raster_data$fill_area_general = ifelse(raster_data$fill_area_general < 0,0,
                                         raster_data$fill_area_general)
  
  raster_data$fill_volume_start = raster_data$fill_area_start * raster_data$distance
  raster_data$fill_volume_cap = raster_data$fill_area_cap * raster_data$distance
  raster_data$fill_volume_general = raster_data$fill_area_general * raster_data$distance
  
  # Sum values
  cut_fill_totals = colSums(raster_data[,c("br_type_0_vol","br_type_1_vol",
                                           "br_type_2_vol","br_type_3_vol",
                                           "br_type_6_vol","br_type_7_vol",
                                           "br_type_8_vol","br_type_9_vol",
                                           "sp_type_0_vol","sp_type_1_vol",
                                           "sp_type_2_vol","sp_type_3_vol",
                                           "sp_type_6_vol","sp_type_7_vol",
                                           "sp_type_8_vol","sp_type_9_vol",
                                           "fill_volume_start","fill_volume_cap",
                                           "fill_volume_general","br_cut_carbon",
                                           "sp_cut_carbon")], na.rm = TRUE)
  
  # Calculate Available Fill
  fill_start_available = cut_fill_totals["br_type_7_vol"]/3 + 
    cut_fill_totals["sp_type_7_vol"]/3 + cut_fill_totals["br_type_6_vol"]/2 + 
    cut_fill_totals["sp_type_6_vol"]/2
  
  
  fill_cap_available = cut_fill_totals["br_type_7_vol"]/3 + 
    cut_fill_totals["sp_type_7_vol"]/3 + cut_fill_totals["br_type_6_vol"]/2 + 
    cut_fill_totals["sp_type_6_vol"]/2
  
  fill_general_available = cut_fill_totals["br_type_0_vol"] + 
    cut_fill_totals["sp_type_0_vol"] + 
    cut_fill_totals["br_type_1_vol"] + cut_fill_totals["sp_type_1_vol"] +
    cut_fill_totals["br_type_2_vol"] + cut_fill_totals["sp_type_2_vol"] +
    cut_fill_totals["br_type_3_vol"] + cut_fill_totals["sp_type_3_vol"] +
    cut_fill_totals["br_type_7_vol"]/3 + cut_fill_totals["sp_type_7_vol"]/3
  
  fill_offsite_disposal = cut_fill_totals["br_type_8_vol"] + 
    cut_fill_totals["sp_type_8_vol"] +
    cut_fill_totals["br_type_9_vol"] + 
    cut_fill_totals["sp_type_9_vol"]
  
  fill_start_needed = cut_fill_totals["fill_volume_start"]
  fill_cap_needed = cut_fill_totals["fill_volume_cap"]
  fill_general_needed = cut_fill_totals["fill_volume_general"]
  
  fill_start_net = fill_start_needed - fill_start_available
  fill_cap_net = fill_cap_needed - fill_cap_available
  fill_general_net = fill_general_needed - fill_general_available
  
  # Cacualte material to be brought onsite/removed
  # TODO: convert material on/off site to kg once density data availalbe
  
  
  material_onsite = c(fill_start_net, fill_cap_net, fill_general_net)
  material_onsite = ifelse(material_onsite > 0,material_onsite,0)
  material_onsite = sum(material_onsite, na.rm = TRUE) * 2000 # assumed density
  
  material_offsite = c(fill_start_net, fill_cap_net, fill_general_net)
  material_offsite = ifelse(material_offsite < 0,-material_offsite,0)
  material_offsite = sum(material_offsite, na.rm = TRUE) * 2000 #assumed density
  
  # Calculate the carbon for the processing
  # Only process the proportion of the cut used in the fill
  # TODO: Come up with a better way to allocate materials to ensure optimal use of material
  
  #raster_data$br_processing_carbon = raster_data$cut_volume_br * raster_data$br_processing
  #raster_data$sp_processing_carbon = raster_data$cut_volume_sp * raster_data$sp_processing # no superficial require processing
  
  # Proportion of cut use
  fill_start_propotion = fill_start_needed / fill_start_available
  fill_cap_propotion = fill_cap_needed / fill_cap_available
  fill_general_propotion = fill_general_needed / fill_general_available
  
  if(is.infinite(fill_start_propotion)){
    fill_start_propotion <- 0
  }
  if(is.infinite(fill_cap_propotion)){
    fill_cap_propotion <- 0
  }
  if(is.infinite(fill_general_propotion)){
    fill_general_propotion <- 0
  }
  
  if(fill_start_propotion > 1){
    fill_start_propotion <- 1
  }
  if(fill_cap_propotion > 1){
    fill_cap_propotion <- 1
  }
  if(fill_general_propotion > 1){
    fill_general_propotion <- 1
  }
  
  
  # Processing volumne and carbon emissions
  processing_volume = fill_start_available * fill_start_propotion +
    fill_cap_available * fill_cap_propotion +
    fill_general_available * fill_general_propotion
  
  processing_carbon = processing_volume * mean(raster_data$br_processing, na.rm = TRUE)
  
  # Fill Emissions
  # For imported material
  # TODO: Get better numbers from Will
  fill_emissions_general_average = 0.6
  fill_emissions_cap_average = 0.9
  fill_emissions_start_average = 0.9
  
  # Calculate fill emissions for reused material
  fill_reused_carbon = processing_volume * mean(raster_data$br_fill, na.rm = TRUE)
  fill_import_start_carbon = fill_start_net * fill_emissions_start_average
  fill_import_cap_carbon = fill_cap_net * fill_emissions_cap_average
  fill_import_general_carbon = fill_start_net * fill_emissions_general_average
  
  
  
  
  
  res = data.frame(
    total_cut = sum(raster_data$cut_volume_br, raster_data$cut_volume_sp, na.rm = TRUE),
    total_fill = sum(cut_fill_totals[c("fill_volume_start","fill_volume_cap",
                                     "fill_volume_general")], na.rm = TRUE),
    carbon_cut = sum(raster_data$br_cut_carbon, raster_data$sp_cut_carbon, na.rm = TRUE),
    carbon_processing = processing_carbon,
    carbon_fill = sum(fill_reused_carbon, fill_import_start_carbon, fill_import_cap_carbon, fill_import_general_carbon),
    material_disposal = material_offsite,
    material_brought_in = material_onsite
  )
  rownames(res) = seq(1,nrow(res))
  
  return(res)
}


#' Allocate cut and fill with maximum gradient
#'
#' @description extract elevations of a line and work our road gradient
#' 
#' @param raster_data a sf dataframe of points and data from the raster
#' @param max_gradient maximum gradient as %
#' @return a dataframe of heights
#' @export
cap_gradient <- function(raster_data, max_gradient = 1.5){
  
  coords <- sf::st_coordinates(raster_data)
  raster_data <- sf::st_drop_geometry(raster_data)
  raster_data$distance <- geodist::geodist(coords, sequential = TRUE, pad = TRUE)
  raster_data$change_elevation <- c(NA, diff(raster_data$elevation))
  raster_data$gradient <- raster_data$change_elevation / raster_data$distance * 100
  
  new_elevation <- list()
  
  for(i in 1:nrow(raster_data)){
    if(i == 1){
      new_elevation[[1]] <- raster_data$elevation[1]
    } else {
      from_elevation <- new_elevation[[i-1]]
      to_elvation <- raster_data$elevation[i]
      dist <- raster_data$distance[i]
      grad <- (to_elvation - from_elevation)/dist * 100
      if(grad >= 0){
        if(grad > max_gradient){
          new_elevation[[i]] <- from_elevation + max_gradient/100 * dist
        } else {
          new_elevation[[i]] <- to_elvation
        }
      } else {
        if(grad < -max_gradient){
          new_elevation[[i]] <- from_elevation - max_gradient/100 * dist
        } else {
          new_elevation[[i]] <- to_elvation
        }
      }
    }
  }
  raster_data$road <- unlist(new_elevation)
  raster_data$difference <- raster_data$elevation - raster_data$road #+ve means cut

  return(raster_data)
}



