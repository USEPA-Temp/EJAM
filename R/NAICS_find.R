#' Search for an industrial sector in the list of NAICS codes, see subsectors
#'
#' Just a utility, quick way to view NAICS industrial sectors that contain queried word or phrase,
#' but can also see all the subcategories within the matching one.
#'
#' @details
#'  NOTE: By default, this shows the highest level category (smallest number of digits in NAICS, like 2 or 3)
#'  that matches (contains) the query via grep(),
#'  and also shows all the children categories (subcategories within that category).
#'  By default, it does NOT only show the entries that match the query.
#'  So NAICS_find('soap') shows "325612 - Polish and Other Sanitation Good Manufacturing",
#'  and others, not just "3256 - Soap, Cleaning Compound, and Toilet Preparation Manufacturing",
#'  because 3256 matches 'soap' and 325612 is a subcategory of 3256.
#'
#' @param matchonly default is FALSE which shows
#' @param query a single word or phrase such as "chemical manufacturing" or "cement"
#' @param NAICS Should default to the dataset installed with this package. see \link{NAICS}
#' @param ignore.case default TRUE, ignoring whether query is upper or lower case
#'
#' @seealso  \link{NAICS_categories} \link{NAICS}
#' @examples
#'  NAICS_find('pulp')
#'  NAICS_find('paper')
#' @export
#'
NAICS_find <- function(query, matchonly=FALSE, NAICS=ifelse(exists(NAICS),NAICS,NULL), ignore.case=TRUE) {
  if (is.null(NAICS)) {warning('missing NAICS dataset'); return(NA)}
  if (matchonly) {

    suppressWarnings( rownum <- which(grepl(query, names(NAICS), ignore.case = ignore.case)))
    if (is.infinite(rownum)) {
      return(NA)
    } else {
      # prefix <- NAICS[[rownum]]
      found <- rownum
    }

  } else {

    suppressWarnings( rownum <- min(which(grepl(query, names(NAICS), ignore.case = ignore.case))) )
    if (is.infinite(rownum)) {
      return(NA)
    } else {
      prefix <- NAICS[[rownum]]
      found <- substr(names(NAICS),1,nchar(prefix)) == prefix
    }
  }

  return( cbind(NAICS[found]) )
}
