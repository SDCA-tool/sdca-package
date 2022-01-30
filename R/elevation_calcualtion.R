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


#' Infer cut and fill
#'
#' @description extract elevations of a line
#'
#' @param elevations a dataframe of elevation data
#' @return a dataframe of cut and fill.
#' @export
cut_fill <- function(elevations){
  
  # Volue of the cut/fill is box plus two triangles
  # Box area is difference * width
  # Two triangles are difference * differnce * tan(slope)
  elevations$areas <-  (elevations$difference * elevations$width) +
    (elevations$difference * elevations$difference * tan(elevations$slope * pi/180))
  elevations$volume <- elevations$areas * 50
  
  return(elevations)
}


#' Allocate cut and fill with maximum gradient
#'
#' @param line a sf dataframe of linestrings
#' @param path_dem path to dem
#' @param max_gradient maximum gradient as %
#' @return a dataframe of heights
#' @export
cap_gradient <- function(line, path_dem, max_gradient = 1.5){
  
  dem <- stars::read_stars(path_dem)
  line_split <- sf::st_segmentize(line, dfMaxLength = 50)
  line_split <- sf::st_cast(line_split, "POINT")
  
  heights <- stars::st_extract(dem, at = line_split)
  names(heights) <- c("elevation","geometry")
  coords <- sf::st_coordinates(heights)
  heights <- sf::st_drop_geometry(heights)
  heights$distance <- geodist::geodist(coords, sequential = TRUE, pad = TRUE)
  heights$change_elevation <- c(NA, diff(heights$elevation))
  heights$gradient <- heights$change_elevation / heights$distance * 100
  
  new_elevation <- list()
  
  for(i in 1:nrow(heights)){
    if(i == 1){
      new_elevation[[1]] <- heights$elevation[1]
    } else {
      from_elevation <- new_elevation[[i-1]]
      to_elvation <- heights$elevation[i]
      dist <- heights$distance[i]
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
  heights$road <- unlist(new_elevation)
  heights$difference <- heights$elevation - heights$road
  
  
  return(heights)
}



