#' see the top level (or other) industrial categories in the NAICS list
#'
#' @param digits default is 2-digits NAICS, which is the top level, but could say 3 or more
#' @param NAICS Should default to the dataset installed with this package. see \link{NAICS}
#'
#' @export
#'
#' @seealso \link{NAICS_find}  \link{NAICS}
NAICS_categories <- function(digits=2, NAICS=ifelse(exists(NAICS),NAICS,NULL)) {
  if (is.null(NAICS)) {warning('missing NAICS dataset'); return(NA)}
  cbind(cbind(NAICS[nchar(as.character(NAICS)) == digits]))
}

