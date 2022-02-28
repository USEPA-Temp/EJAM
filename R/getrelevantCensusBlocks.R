#' Key buffering function - wrapper for other functions doing the work
#' 
#' 
#'  As written it assumes that block location data file is already in global environment. 
#'  
#' @param sitepoints see \link{getrelevantCensusBlocksviaQuadTree} or other such functions
#' @param cutoff  see \link{getrelevantCensusBlocksviaQuadTree} or other such functions
#' @param maxcutoff  see \link{getrelevantCensusBlocksviaQuadTree} or other such functions
#' @param uniqueonly  see \link{getrelevantCensusBlocksviaQuadTree} or other such functions
#' @param avoidorphans  see \link{getrelevantCensusBlocksviaQuadTree} or other such functions
#' @param ...  see \link{getrelevantCensusBlocksviaQuadTree_Clustered} or other such functions
#'
#' @export
#'
getrelevantCensusBlocks <- function(sitepoints, cutoff, maxcutoff, uniqueonly, avoidorphans, ...) {
  # wrapper to make it simple to switch between functions to use for this, clustered vs not, etc.

  # getrelevantCensusBlocksviaQuadTree_Clustered(sitepoints, cutoff, maxcutoff, uniqueonly, avoidorphans, ...)

  getrelevantCensusBlocksviaQuadTree(sitepoints, cutoff, maxcutoff, uniqueonly, avoidorphans, ...)
}
