#' helper function
#'
#' @param data data.table
#' @param fieldname colname to check
#'
#' @return logical
#' @export
#'
hasfield <- function(data, fieldname){
  soughtidx <- which(names(data) == fieldname)
  return(length(soughtidx) > 0)
}
