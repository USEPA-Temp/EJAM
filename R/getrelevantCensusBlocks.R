#' Key buffering function - wrapper for other functions doing the work
#'
#' @param facilities see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#' @param cutoff  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#' @param maxcutoff  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#' @param uniqueonly  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#' @param avoidorphans  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#' @param ...  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#'
#' @export
#'
getrelevantCensusBlocks <- function(facilities, cutoff, maxcutoff, uniqueonly, avoidorphans, ...) {
  # wrapper to make it simple to switch between functions to use for this, clustered vs not, etc.

  # getrelevantCensusBlocksviaQuadTree_Clustered(facilities, cutoff, maxcutoff, uniqueonly, avoidorphans, ...)

  getrelevantCensusBlocksviaQuadTree(facilities, cutoff, maxcutoff, uniqueonly, avoidorphans, ...)
}
