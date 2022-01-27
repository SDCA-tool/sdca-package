#' Extract elevations
#'
#' @description extract elevations of a line
#'
#' @param line a sf dataframe of linestrings
#' @param path_dem path to dem
#' @return a dataframe of heights
#' @examples
#' \dontrun{
#' 
#' }
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
#' @examples
#' \dontrun{
#' 
#' }
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
