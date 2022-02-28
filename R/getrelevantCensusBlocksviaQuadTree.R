#' find nearby blocks using Quad Tree data structure for speed, NO PARALLEL PROCESSING
#'
#'
#'
#' @details  Uses indexgridsize and quaddata  variables that come from global environment (but should pass to this function rather than assume in global env?)
#'
#' @param sitepoints data.table with columns siteid, lat, lon giving point locations of sites or facilities around which are circular buffers
#' @param cutoff miles radius, defining circular buffer around site point 
#' @param maxcutoff miles distance (max distance to check? if not even 1 block point is within cutoff)
#' @param uniqueonly logical WILL BE REMOVED AND DONE OUTSIDE THIS FUNCTION. default FALSE (may remove this param) Whether to retain only unique blocks (unique residents) to avoid double-counting (but we want to drop duplicates later, not in here)
#' @param avoidorphans logical
#' @param quadtree a quadtree object created from the SearchTree package example: SearchTrees::createTree(blockdata::quaddata, treeType = "quad", dataType = "point")
#'
#' @seealso \link{getrelevantCensusBlocksviaQuadTree_Clustered}  \link{computeActualDistancefromSurfacedistance}
#' @export
#' @import data.table
#' @importFrom pdist "pdist"
#'
getrelevantCensusBlocksviaQuadTree <- function(sitepoints, cutoff, maxcutoff, uniqueonly=FALSE, avoidorphans, quadtree) {
  
  if(class(quadtree) != "QuadTree"){
    stop('quadtree must be an object created from SearchTrees package with treeType = "quad" and dataType = "point"')  
  }

  #pass in a list of uniques and the surface cutoff distance
  #filter na values
  sitepoints <- sitepoints[!is.na(sitepoints$lat) & !is.na(sitepoints$lon), ]
  #compute and add grid info
  earthRadius_miles <- 3959 # in case it is not already in global envt
  radians_per_degree <- pi / 180
  
  
  
  sitepoints[,"lat_RAD"]  <- sitepoints$lat *  radians_per_degree
  sitepoints[,"lon_RAD"]  <- sitepoints$lon * radians_per_degree
  cos_lat <- cos(sitepoints$lat_RAD)
  sitepoints[,"FAC_X"] <- earthRadius_miles * cos_lat * cos(sitepoints$lon_RAD)
  sitepoints[,"FAC_Y"] <- earthRadius_miles * cos_lat * sin(sitepoints$lon_RAD)
  sitepoints[,"FAC_Z"] <- earthRadius_miles * sin(sitepoints$lat_RAD)

  # indexgridsize was defined in initialization as say 10 miles
  # and buffer_indexdistance defined here in code but is never used anywhere...  
  buffer_indexdistance <- ceiling(cutoff/indexgridsize) 
  truedistance <- computeActualDistancefromSurfacedistance(cutoff)   # simply 7918*sin(cutoff/7918) 
  
  # main reason for using foreach::foreach() is that it supports parallel execution,
  # that is, it can execute those repeated operations on multiple processors/cores on your computer
  # (and there are other advantages as well)

  # -- Get ready for loop 
  
  # allocate result list
  nRowsDf <- NROW(sitepoints)
  res <- vector('list', nRowsDf)
  
  #### LOOP OVER THE sitepoints STARTS HERE ####


result <- data.frame()
  
for (i in 1:nRowsDf) {
  
    coords <- sitepoints[i, .(FAC_X, FAC_Z)]  # the similar clustered function uses sitepoints2use not sitepoints
    x_low <- coords[,FAC_X]-truedistance;
    x_hi  <-  coords[,FAC_X]+truedistance
    z_low <- coords[,FAC_Z]-truedistance;
    z_hi  <-  coords[,FAC_Z]+truedistance

    if ((i %% 100)==0) {print(paste("Cells currently processing: ",i," of ", nRowsDf) ) }

    vec <- SearchTrees::rectLookup(quadtree, unlist(c(x_low,z_low)), unlist(c(x_hi,z_hi)))

    tmp <- blockdata::quaddata[vec, ] 
    x <- tmp[ , .(BLOCK_X, BLOCK_Y, BLOCK_Z)] # but not blockid , blockid /  blockfips?? 
    y <- sitepoints[i, c('FAC_X','FAC_Y','FAC_Z')]  # the similar clustered function uses sitepoints2use not sitepoints
    distances <- as.matrix(pdist::pdist(x,y))

    #clean up fields
    tmp[ , distance := distances[ , c(1)]]
    tmp[ , siteid := sitepoints[i, .(siteid)]]  # the similar clustered function uses sitepoints2use not sitepoints

    #filter actual distance
    tmp <- tmp[distance <= truedistance, .(blockid, distance, siteid)]

    # hold your horses, what if there are no blocks and you are supposed to avoid that
    if ( avoidorphans && (nrow(tmp))==0 ){
      #search neighbors, allow for multiple at equal distance
      vec <- SearchTrees::knnLookup(quadtree, unlist(c(coords[ , 'FAC_X'])), unlist(c(coords[ , 'FAC_Z'])), k=10)      
      tmp <- blockdata::quaddata[vec[1,], ]

      x <- tmp[, .(BLOCK_X,BLOCK_Y,BLOCK_Z)]
      y <- sitepoints[i, .(FAC_X,FAC_Y,FAC_Z)]
      distances <- as.matrix(pdist::pdist(x,y))

      #clean up fields
      tmp[ , distance := distances[ , c(1)]]
      tmp[ , siteid := sitepoints[i, .(siteid)]]

      #filter to max distance
      truemaxdistance <- computeActualDistancefromSurfacedistance(maxcutoff)
      tmp <- tmp[distance <= truemaxdistance, .(blockid, distance, siteid)]
      result <- rbind(result, tmp)
    } else {
      result <- rbind(result, tmp)
    }

  }

  # Actually, should only do this removal of duplicate blocks (residents) outside the getrelevant... function, so that the function gets all facility-specific results to save, 
  # and later they can be used to get site specific stats for all blocks at a site
  # and later they can also be used to get aggregated stats for all unique blocks (residents) among all the sites as a whole.
  if ( uniqueonly) {
    stop('will be recoded to allow this removal of duplicate blocks (residents) only outside this getrelevant... function')
    data.table::setkey(result) #  uses all columns as keys 
    result <- unique(result, by=c("blockid")) # unique values of that specified column
  }
  
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

