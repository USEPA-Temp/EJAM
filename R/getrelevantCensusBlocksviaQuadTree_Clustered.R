#' find nearby blocks using Quad Tree data structure for speed, CLUSTERED FOR PARALLEL PROCESSING
#' @description Uses packages \link[pkg]{parallel} and snow. parallel::makePSOCKcluster is an enhanced version of snow::makeSOCKcluster in package snow.
#'     It runs Rscript on the specified host(s) to set up a worker process which listens on a socket for expressions to evaluate, and returns the results (as serialized objects).
#'
#' @details  Uses indexgridsize and quaddata  variables that come from global environment (but should pass to this function rather than assume in global env?)
#'
#' @param facilities data.table with columns LAT, LONG
#' @param cutoff miles distance (check what this actually does)
#' @param maxcutoff miles distance (check what this actually does)
#' @param uniqueonly logical
#' @param avoidorphans logical
#' @param CountCPU for parallel processing via makeCluster() and doSNOW::registerDoSNOW()
#' @seealso \link{getrelevantCensusBlocksviaQuadTree}  \link{computeActualDistancefromSurfacedistance}
#' @export
#'
getrelevantCensusBlocksviaQuadTree_Clustered <-function(facilities,cutoff,maxcutoff,uniqueonly,avoidorphans,CountCPU=1) {
  #pass in a list of uniques and the surface cutoff distance

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

  #set up cluster, splitting up the facilities among the available CPUs
  cpuids <- 1:CountCPU
  facilities[,"CPUAFFINITY"] <- cpuids
  percpufacilities<- vector('list', CountCPU)
  for(i in 1:CountCPU)  ## for each CPU
  {
    percpufacilities[[i]] <- subset(facilities, CPUAFFINITY==i)
  }

  # parallel::makePSOCKcluster is an enhanced version of snow::makeSOCKcluster in package snow. It runs Rscript on the specified host(s) to set up a worker process which listens on a socket for expressions to evaluate, and returns the results (as serialized objects).
  cl <- parallel::makeCluster(CountCPU)
  doSNOW::registerDoSNOW(cl)

  # main reason for using foreach::foreach() is that it supports parallel execution,
  # that is, it can execute those repeated operations on multiple processors/cores on your computer (and there are other advantages as well)
  cpuIndex <- 1 ; FAC_X<-0; FAC_Z<-0 # this just stops the warning about undefined variable since IDE does not understand it being defined in foreach()
  #### LOOP OVER THE CPUs ##############################################################################################
  parref <- foreach::foreach(cpuIndex=1:CountCPU, .export = c("quaddata","computeActualDistancefromSurfacedistance","earthRadius_miles","crd"), .packages = c("SearchTrees","data.table","pdist")) %dopar% {

    #2 seconds overhead to create the quad tree
    localtree <- SearchTrees::createTree(quaddata, treeType = "quad", dataType = "point")
    facilities2use <- percpufacilities[[cpuIndex]]

    # allocate result list
    subnRowsDf <- nrow(facilities2use)
    partialres <- vector('list', subnRowsDf)

    #### LOOP OVER THE FACILITIES STARTS HERE, within loop over CPUs ##################################################################

    for(i in 1:subnRowsDf)  { ## for each row






      coords <- facilities2use[i, .(FAC_X,FAC_Z)]
      x_low <- coords[,FAC_X]-truedistance;
      x_hi  <-  coords[,FAC_X]+truedistance
      z_low <- coords[,FAC_Z]-truedistance;
      z_hi  <-  coords[,FAC_Z]+truedistance

      # if ((i %% 100)==0) {print(paste("Cells currently processing: ",i," of ",nRowsDf) ) }

      vec <- SearchTrees::rectLookup(localtree,c(x_low,z_low),c(x_hi,z_hi))



      tmp <- quaddata[vec,]
      x <- tmp[, .(BLOCK_X,BLOCK_Y,BLOCK_Z)]
      y <- facilities2use[i, .(FAC_X,FAC_Y,FAC_Z)]
      distances <- as.matrix(pdist(x,y))

      #clean up fields
      tmp[,Distance := distances[,c(1)]]
      tmp[,ID := facilities2use[i, .(ID)]]

      #filter actual distance
      tmp <- tmp[Distance <= truedistance, .(BLOCKID,Distance,ID)]

      #hold your horses, what if there are no blocks and you are supposed to avoid that
      if ( avoidorphans && (nrow(tmp))==0 ) {
        #search neighbors, allow for multiple at equal distance
        vec <- SearchTrees::knnLookup(localtree,c(coords[,FAC_X]),c(coords[,FAC_Z]),k=10)
        tmp <- quaddata[vec[1,],]

        x <-tmp[, .(BLOCK_X,BLOCK_Y,BLOCK_Z)]
        y <-facilities2use[i, .(FAC_X,FAC_Y,FAC_Z)]
        distances <- as.matrix(pdist(x,y))

        #clean up fields
        tmp[,Distance := distances[,c(1)]]
        tmp[,ID := facilities2use[i, .(ID)]]

        #filter to max distance
        truemaxdistance <- computeActualDistancefromSurfacedistance(maxcutoff)
        tmp <- tmp[Distance<=truemaxdistance, .(BLOCKID,Distance,ID)]
        partialres[[i]] <- tmp
      } else {
        partialres[[i]] <- tmp
      }
    }
    partial <- do.call('rbind', partialres)
    return(partial)
  }

  bound <- do.call('rbind', parref)

  print(paste("Total Rowcount: ",nrow(bound)) )
  if ( uniqueonly) {
    data.table::setkey(bound, "BLOCKID","Distance","ID")
    bound <- unique(bound, by=c("BLOCKID"))
  }
  print(paste("Final Rowcount: ",nrow(bound)) )
  # is this from parallel or snow package?
  stopCluster(cl)
  return(bound)
}
