#' convert character columns back to numeric if they were meant to be numbers
#'
#' removes some things like percent sign and less than sign and N/A and the word miles too
#' 
#' @param x data.frame from ejscreen api output 
#'
#' @return data.frame
#'
makenumericdfFORSHINY <- function(x) {
  
  cleanit <- function(z) {
    as.data.frame(lapply(z, function(y) (
      gsub('th', '', 
           gsub('<', '', 
                gsub(' miles', '', 
                     gsub('N/A','', 
                          gsub('%','', 
                               gsub(',', '', y)) )))))),
      stringsAsFactors = FALSE)
  }
  
  clean <- cleanit(x)

for (i in 1:NCOL(x)) {
  if (all(is.na(as.numeric(clean[,i]))) & !all(is.na(clean[,i]))) {
    # was not all NA but got forced to NA via as.numeric means was a real character col
  } else {
    clean[, i] <- as.numeric(clean[, i])
  }
}
return(clean)    
}
