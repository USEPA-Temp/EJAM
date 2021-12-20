#' find nearby blocks using Quad Tree data structure for speed, NO PARALLEL PROCESSING
#'
#'
#'
#' @details  Uses indexgridsize and quaddata  variables that come from global environment (but should pass to this function rather than assume in global env?)
#'
#' @param facilities data.table with columns LAT, LONG
#' @param cutoff miles distance (check what this actually does)
#' @param maxcutoff miles distance (check what this actually does)
#' @param uniqueonly logical
#' @param avoidorphans logical
#'
#' @seealso \link{getrelevantCensusBlocksviaQuadTree_Clustered}  \link{computeActualDistancefromSurfacedistance}
#' @export
#' @import data.table
#' @importFrom pdist "pdist"
#'
getrelevantCensusBlocksviaQuadTree <- function(facilities,cutoff,maxcutoff,uniqueonly,avoidorphans, tree) {
  #pass in a list of uniques and the surface cutoff distance
  #filter na values
  facilities <- facilities[!is.na(facilities$LAT) & !is.na(facilities$LONG), ]
  #compute and add grid info
  earthRadius_miles <- 3959 # in case it is not already in global envt
  facilities[,"LAT_RAD"] <- facilities$LAT * pi / 180
  facilities[,"LONG_RAD"] <- facilities$LONG * pi / 180
  facilities[,"FAC_X"] <- earthRadius_miles * cos(facilities$LAT_RAD) * cos(facilities$LONG_RAD)
  facilities[,"FAC_Y"] <- earthRadius_miles * cos(facilities$LAT_RAD) * sin(facilities$LONG_RAD)
  facilities[,"FAC_Z"] <- earthRadius_miles * sin(facilities$LAT_RAD)

  #now we need to buffer around the grid cell by the actual cutoff distance
  buffer_indexdistance <- ceiling(cutoff/indexgridsize) # this will be one or larger ... but where is this ever used??  indexgridsize was defined in initialization as say 10 miles

  # allocate result list
  nRowsDf <- nrow(facilities)
  res <- vector('list', nRowsDf)

  truedistance <- computeActualDistancefromSurfacedistance(cutoff)   # simply 7918*sin(cutoff/7918)

    # main reason for using foreach::foreach() is that it supports parallel execution,
  # that is, it can execute those repeated operations on multiple processors/cores on your computer
  # (and there are other advantages as well)



  #### LOOP OVER THE FACILITIES STARTS HERE ####


result <- data.frame()
for (i in 1:nRowsDf) {
  # pull these out of the loop
    coords <- facilities[i, .(FAC_X,FAC_Z)]  # the similar clustered function uses facilities2use not facilities
    x_low <- coords[,FAC_X]-truedistance;
    x_hi  <-  coords[,FAC_X]+truedistance
    z_low <- coords[,FAC_Z]-truedistance;
    z_hi  <-  coords[,FAC_Z]+truedistance

    if ((i %% 100)==0) {print(paste("Cells currently processing: ",i," of ",nRowsDf) ) }

    vec <- SearchTrees::rectLookup(tree,unlist(c(x_low,z_low)),unlist(c(x_hi,z_hi))) # blockquadtree  here but localtree in clustered version of function

    tmp <- quaddata[vec,]
    x <- tmp[, .(BLOCK_X,BLOCK_Y,BLOCK_Z)]
    y <- facilities[i, c('FAC_X','FAC_Y','FAC_Z')]  # the similar clustered function uses facilities2use not facilities
    distances <- as.matrix(pdist::pdist(x,y))

    #clean up fields
    tmp[ , Distance := distances[,c(1)]]
    tmp[ , ID := facilities[i, .(ID)]]  # the similar clustered function uses facilities2use not facilities

    #filter actual distance
    tmp <- tmp[Distance <= truedistance, .(BLOCKID,Distance,ID)]

    # hold your horses, what if there are no blocks and you are supposed to avoid that
    if ( avoidorphans && (nrow(tmp))==0 ){
      #search neighbors, allow for multiple at equal distance
      vec <- SearchTrees::knnLookup(tree,unlist(c(coords[ , 'FAC_X'])),unlist(c(coords[ , 'FAC_Z'])), k=10)   # blockquadtree  here but localtree in clustered version of function
#      vec <- SearchTrees::knnLookup(blockquadtree,c(coords[ , FAC_X]),c(coords[,FAC_Z]),k=10)   # blockquadtree  here but localtree in clustered version of function
      tmp <- quaddata[vec[1,], ]

      x <-tmp[, .(BLOCK_X,BLOCK_Y,BLOCK_Z)]
      y <-facilities[i, .(FAC_X,FAC_Y,FAC_Z)]
      distances <- as.matrix(pdist::pdist(x,y))

      #clean up fields
      tmp[,Distance := distances[,c(1)]]
      tmp[,ID := facilities[i, .(ID)]]

      #filter to max distance
      truemaxdistance <- computeActualDistancefromSurfacedistance(maxcutoff)
      tmp <- tmp[Distance<=truemaxdistance, .(BLOCKID,Distance,ID)]
      result <- rbind(result,tmp)
    } else {
      result <- rbind(result,tmp)
    }
  }


  print(paste("Total Rowcount: ", nrow(result)) )
  if ( uniqueonly) {
    data.table::setkey(result, "BLOCKID", "Distance", "ID")
    result <- unique(result, by=c("BLOCKID"))
  }
  print(paste("Final Rowcount: ", nrow(result)) )

  return(result)
}
