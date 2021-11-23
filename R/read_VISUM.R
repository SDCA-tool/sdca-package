
read_visum <- function(x){
  foo <- readLines(x)
  
  first <- substr(foo,1,1)
  table(first)
  
  objrows <- (1:length(foo))[substr(foo,1,5) == "* Obj"]
  objend <- (1:length(foo))[substr(foo,1,6) == "$NAMES"] - 1
  
  numbs <- data.frame(start = objrows, 
                      end = c(objrows[2:length(objrows)],objend))
  
  res <- list()
  for(i in 1:nrow(numbs)){
    sub <- foo[seq(numbs$start[i] + 1,numbs$end[i] - 1)]
    sub <- strsplit(sub," ")
    sub <- unlist(sub)
    sub <- sub[sub != ""]
    sub <- as.numeric(sub)
    res[[i]] <- sub
  }
  res2 <- unlist(res)
  mat <- matrix(res2, ncol = nrow(numbs))
  
  return(mat)
}


