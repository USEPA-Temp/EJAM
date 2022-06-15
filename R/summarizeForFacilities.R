#' Wrapper for other functions doing the work
#'
#' @param sitepoints see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#' @param cutoff  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#' @param maxcutoff  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#' @param uniqueonly  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#' @param avoidorphans  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#' @param ...  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#'
#' @export

summarizeForFacilities <- function(sitepoints, cutoff, maxcutoff, uniqueonly, avoidorphans, ...) {
  
  blocks <- getrelevantCensusBlocksviaQuadTree(sitepoints, cutoff, maxcutoff, uniqueonly, avoidorphans, ...)
  cat('Aggreating results\n')
  doaggregate(sitepoints , blocks)
  # facilities
}
  