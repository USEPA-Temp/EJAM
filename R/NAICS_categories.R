#' see the top level (or other) industrial categories in the NAICS list
#' 
#' See \url{https://www.census.gov/naics/}
#'
#' @param digits default is 2-digits NAICS, which is the top level, but could say 3 or more
#' @param dataset Should default to the dataset called NAICS, installed with this package. 
#'   see \link{NAICS}  Check attr(NAICS, 'year')
#'
#' @export
#'
#' @seealso \link{NAICS_find}  \link{NAICS}
NAICS_categories <- function(digits=2, dataset=ifelse(exists(NAICS),NAICS,NULL)) {
  if (is.null(dataset)) {warning('missing NAICS dataset'); return(NA)}
  cbind(cbind(dataset[nchar(as.character(dataset)) == digits]))
}

