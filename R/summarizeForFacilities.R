#' Wrapper for other functions doing the work
#'
#' @param facilities see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#' @param cutoff  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#' @param maxcutoff  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#' @param uniqueonly  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#' @param avoidorphans  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#' @param ...  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#'
#' @export

summarizeForFacilities <- function(facilities, cutoff, maxcutoff, uniqueonly, avoidorphans, ...) {
  
  blocks <- getrelevantCensusBlocksviaQuadTree(facilities, cutoff, maxcutoff, uniqueonly, avoidorphans, ...)
  
  doaggregate(facilities,blocks)
  
}
  