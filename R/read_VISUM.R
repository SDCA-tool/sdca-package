#' Read VISUM matrix files
#'
#' @description Import the v type VISUM matrix
#'
#' @param x a file path
#' @examples
#' \dontrun{
#' test_function(2)
#' }
#' @export
read_visum <- function(x){
  dat <- readLines(x)
  
  objrows <- (1:length(dat))[substr(dat,1,5) == "* Obj"]
  objend <- (1:length(dat))[substr(dat,1,6) == "$NAMES"] - 1
  
  numbs <- data.frame(start = objrows, 
                      end = c(objrows[2:length(objrows)],objend))
  
  fact <- dat[grep("* Factor",dat[1:100]) + 1]
  fact <- as.numeric(fact)
  
  res <- list()
  res_tots <- list()
  for(i in 1:nrow(numbs)){
    sub <- dat[seq(numbs$start[i] + 1,numbs$end[i] - 1)]
    sub <- strsplit(sub," ")
    sub <- unlist(sub)
    sub <- sub[sub != ""]
    sub <- as.numeric(sub)
    sub_total <- dat[numbs$start[i]]
    sub_total <- gsub("* Obj ","",sub_total, fixed = TRUE)
    sub_total <- strsplit(sub_total," Sum = ")
    res[[i]] <- sub
    res_tots[[i]] <- sub_total
  }
  res <- unlist(res)
  mat <- matrix(res, ncol = nrow(numbs))
  mat[mat == 1e+15] <- NA
  mat <- mat * fact
  
  res_names <- sapply(res_tots, function(y){y[[1]][1]})
  res_sums <- sapply(res_tots, function(y){y[[1]][2]})
  
  rownames(mat) = res_names
  colnames(mat) = res_names
  
  return(mat)
}



