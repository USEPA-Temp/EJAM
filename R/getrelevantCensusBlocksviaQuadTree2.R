#' Find nearby blocks using Quad Tree data structure for speed, NO PARALLEL PROCESSING
#'
#' @description Given a set of points and a specified radius (cutoff), 
#'   this function quickly finds all the US Census blocks near each point. 
#'   For each point, it uses the specified cutoff distance and finds the distance to 
#'   every block within the circle defined by the radius (cutoff). 
#'   Each block is defined by its Census-provided internal point, by latitude and longitude.
#'   
#'   Each point can be the location of a regulated facility or other type of site, and 
#'   the blocks are a high-resolution source of information about where
#'   residents live. 
#'   
#'   Finding which blocks have their internal points in a circle provides 
#'   a way to quickly estimate what fraction of a block group is 
#'   inside the circular buffer more accurately and more quickly than
#'   areal apportionment of block groups would provide. 
#'   
#' @details  Relies on blockquadtree, indexgridsize and quaddata  
#'   variables that need to be available in the global environment, as data
#'   from a loaded package. 
#' 
#' @param sitepoints data.table with columns siteid, lat, lon giving point locations of sites or facilities around which are circular buffers
#' @param cutoff miles distance (check what this actually does)
#' @param maxcutoff miles distance (check what this actually does)
#' @param uniqueonly logical WILL BE REMOVED AND DONE apart from THIS FUNCTION. default FALSE (may remove this param) Whether to retain only unique blocks (unique residents) to avoid double-counting (but we want to drop duplicates later, not in here)
#' @param avoidorphans logical
#'
#' @seealso \link{getrelevantCensusBlocksviaQuadTree2}  \link{computeActualDistancefromSurfacedistance}
#' @export
#' @import data.table
#' @importFrom pdist "pdist"
#'
getrelevantCensusBlocksviaQuadTree2 <- function(sitepoints, cutoff=1, maxcutoff=31.07, 
                                                uniqueonly=FALSE, avoidorphans=TRUE) {
  
  #pass in a list of uniques and the surface cutoff distance
  #filter na values
  sitepoints <- sitepoints[!is.na(sitepoints$lat) & !is.na(sitepoints$lon), ]  
  #compute and add grid info
  earthRadius_miles <- 3959 # in case it is not already in global envt
  radians_per_degree <- pi / 180

  f2 <- data.table::copy(sitepoints) # make a copy to avoid altering sitepoints in the calling envt when modifying by reference using data.table
  rm(sitepoints) 
  f2[ , lat_RAD  := lat * radians_per_degree]   # data.table modifies it by reference
  f2[ , lon_RAD  := lon * radians_per_degree]
  cos_lat <- cos(f2[,lat_RAD])
  f2[ , FAC_X := earthRadius_miles * cos_lat * cos(f2[,lon_RAD])]
  f2[ , FAC_Y := earthRadius_miles * cos_lat * sin(f2[,lon_RAD])]
  f2[ , FAC_Z := earthRadius_miles *  sin(f2[,lat_RAD])]

  # indexgridsize was defined in initialization as say 10 miles
  # and buffer_indexdistance defined here in code but is never used anywhere...  
  buffer_indexdistance <- ceiling(cutoff / indexgridsize) 
  truedistance <- computeActualDistancefromSurfacedistance(cutoff)   # simply 7918*sin(cutoff/7918) 
  
  # main reason for using foreach::foreach() is that it supports parallel execution,
  # that is, it can execute those repeated operations on multiple processors/cores on your computer
  # (and there are other advantages as well)
  
  #---- Get ready for loop here ----
  #allocate result list
  nRowsDf <- NROW(f2)
  res <- vector('list', nRowsDf)  # list of data.frames?  cols will be blockid, distance, siteid
  result <- data.frame() # cols will be blockid, distance, siteid
  #these are now outside the loop, so they can be used in the vectorized way
  coords <- f2[ , .(FAC_X, FAC_Z)] 
  x_low <- coords[ , FAC_X] -truedistance;
  x_hi  <-  coords[ , FAC_X] +truedistance
  z_low <- coords[ , FAC_Z] -truedistance;
  z_hi  <-  coords[ , FAC_Z] +truedistance

  for (i in 1:nRowsDf) {    # LOOP OVER SITES HERE ----
    

    
    
    
    if ((i %% 100) == 0) {print(paste("Cells currently processing: ",i," of ", nRowsDf) ) }
    
    
    vec <- SearchTrees::rectLookup(blockdata::blockquadtree, unlist(c(x_low[i, ], z_low[i, ])), unlist(c(x_hi[i, ], z_hi[i, ]))) 
    # blockquadtree  here but localtree in clustered version of function
    
    tmp <- blockdata::quaddata[vec, ]
    x <- tmp[ , .(BLOCK_X, BLOCK_Y, BLOCK_Z)] # but not blockid ?? 
    y <- f2[i, c('FAC_X','FAC_Y','FAC_Z')]  # the similar clustered function uses facilities2use 
    distances <- as.matrix(pdist::pdist(x, y))
    
    #clean up fields
    tmp[ , distance := distances[ , c(1)]]
    tmp[ , siteid := f2[i , .(siteid)]]  # the similar clustered function uses facilities2use 
    
    #filter actual distance
    tmp <- tmp[distance <= truedistance, .(blockid, distance, siteid)]
    
    # hold your horses, what if there are no blocks and you are supposed to avoid that
    if ( avoidorphans && (nrow(tmp)) == 0) {
      #search neighbors, allow for multiple at equal distance
      vec  <- SearchTrees::knnLookup(blockdata::blockquadtree, unlist(c(coords[i, 'FAC_X'])), unlist(c(coords[i, 'FAC_Z'])), k=10) # blockquadtree  here but localtree in clustered version of function
      #vec <- SearchTrees::knnLookup(blockdata::blockquadtree, c(coords[i, FAC_X]),           c(coords[i, FAC_Z]), k=10)    # blockquadtree  here but localtree in clustered version of function
      tmp <- blockdata::quaddata[vec[1, ], ]
      
      x <- tmp[, .(BLOCK_X, BLOCK_Y, BLOCK_Z)]
      y <- f2[i, .(FAC_X, FAC_Y, FAC_Z)]
      distances <- as.matrix(pdist::pdist(x, y))
      
      #clean up fields
      tmp[ , distance := distances[ , c(1)]]
      tmp[ , siteid := f2[i, .(siteid)]]
      
      #filter to max distance
      truemaxdistance <- computeActualDistancefromSurfacedistance(maxcutoff)
      tmp <- tmp[distance <= truemaxdistance, .(blockid, distance, siteid)]
      res[[i]] <- tmp # saving results as a list of tables to rbind after loop; old code did rbind for each table, inside loop 
    } else {
      res[[i]] <- tmp
    }
  }
  result <- data.table::rbindlist(res) 
  
  # Actually, should only do this removal of duplicate blocks (residents) outside the getrelevant... function, so that the function gets all facility-specific results to save, 
  # and later they can be used to get site specific stats for all blocks at a site
  # and later they can also be used to get aggregated stats for all unique blocks (residents) among all the sites as a whole.
  if ( uniqueonly) {
    stop('will be recoded to allow this removal of duplicate blocks (residents) only outside this getrelevant... function')
    data.table::setkey(result, "blockid", "siteid", "distance") # not sure why that was here
    result <- data.table::unique(result, by=c("blockid"))
  }
  data.table::setkey(result, "blockid", "siteid", "distance")
  
  print(paste0(nRowsDf, ' input sites'))
  print(paste0(data.table::uniqueN(result, by = 'siteid'), ' output sites (got results)'))
  bcount <- data.table::uniqueN(result)
  print(paste0(bcount, " blocks in final row count (block-to-site pairs)" ))
  bcount_unique <- data.table::uniqueN(result, by = 'blockid')
  print(paste0(bcount_unique , ' unique blocks'))
  print(paste0(round(bcount / nRowsDf, 0), ' blocks per site, on average'))
  print(paste0(round(1 - (bcount_unique / bcount), 2), '% of blocks are duplicates because those residents are near two or more sites'))
  
  return(result)
}
