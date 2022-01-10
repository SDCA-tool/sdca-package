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
  
  pas2080 <- data.frame(
    pas2080_code = c("A1","A2","A3","A4","A5",
                     "B1","B2","B3","B4","B5","B6","B7","B8","B9",
                     "C1","c2","C3","C4"),
    emissions = c(1:18),
    emissions_high = c(1:18)*2,
    emissions_low = c(1:18)*0.5,
    confidence = "low",
    notes = "notes go here"
    )
  
  timeseries <- data.frame(year = 2022:2050,
                           emissions = c(10000,rep(-1000, 28)))
  timeseries$emissions_cumulative <- cumsum(timeseries$emissions)

  emissions_whole_life <- -18000
  payback_time <- 10
  netzero_compatiable <- "yes"
  comments <- "Test data, this is meaningless"
  
  
  
  results <- list(pas2080,timeseries,payback_time,emissions_whole_life,netzero_compatiable,comments)
  
  names(results) <- c("pas2080","timeseries","payback_time","emissions_whole_life","netzero_compatiable","comments")
  
  
  results <- jsonlite::toJSON(results)
  return(results)
}