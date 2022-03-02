# script example of running proximity analysis without Shiny app
# ****  presumes that other data are in global environment, ****
# **** like blockgroupstats, quaddata, etc. ****
if (1 ==0) {

  # See details in help for ?EJAM

  # and note that right now batch.summarizer::ejscreenapi() is in that package not here

  # and see package census2020download for 2020 census data on blocks 

  # and facilities_prep may be obsolete or should be done before save that as dataset and build a package.


  # set up parameters, functions ####
  # includes library(EJAM) which provides datasets like blockgroupstats, facilities, etc.
  library(blockdata) # for 2010 data. 
  library(EJAM)
  library(data.table)

  CountCPU <- 2
  CountCPU <- parallel::detectCores()
  indexgridsize <- 10  # This does not seem to be used ever - it is just used to create buffer_indexdistance which is not used.

  # can specify random test points (sites) ######
  #sitepoints <- points100example %>% head(1)# data in this package

  sitepoints <- data.table::copy(EJAM::points100example)
  # sitepoints <- data.table::copy(EJAM::points100example)   # NOTE the first point is far outside the continental US and returns no data using census 2010 blocks.
  sitepoints[ , siteid := .I] # .I JUST NUMBERS THE SITES
  data.table::setnames(sitepoints, 'LAT', 'lat')
  data.table::setnames(sitepoints, 'LONG', 'lon')
  data.table::setkey(sitepoints) #,  c('siteid', 'lat', 'lon'))

  # specify radius for circular buffer and other key parameters ####

  radius <- 1 # radius (miles)
  maxcutoff <- 31.07 # 50 km  # max distance to expand search to, if avoidorphans=TRUE
  avoidorphans <- TRUE  # Expand distance searched, when a facility has no census block centroid within selected buffer distance.
  
  uniqueonly <- FALSE   # The uniqueonly parameter will be removed from getrelevant... and handled in doaggregate() 
  
  # This must be done for each session - One cannot save it as .rda and just load via a pkg. 
  localtree <- SearchTrees::createTree(blockdata::quaddata, treeType = "quad", dataType = "point")
  
  # call function that finds nearby blocks  ####
  
  system.time({
    # results_by_site <- summarizeForFacilities(
    
    sites2blocks <- EJAM::getrelevantCensusBlocksviaQuadTree(
      sitepoints =  sitepoints,
      cutoff = radius,
      maxcutoff = maxcutoff,
      uniqueonly = uniqueonly,
      avoidorphans = avoidorphans,
      quadtree = localtree
    )
  })
  # > head(sites2blocks)
  #    blockid  distance siteid
  # 1:  388798 0.9879380     29
  # 2:  388799 0.8800507     29
  # 3:  388809 0.6421582     29
  
  
  
  
  ##  function that aggregates in each buffer  ####
  #
  # system.time(
  #
  # results_by_site <- EJAM::doaggregate(facilities = sitepoints, facilityblocks = results) 
  # )
  #
  # # see results ####
  #
  # head(results_by_site)

  ## compare to results from EJSCREEN API 
  #    
  # ej_api_results <- batch.summarizer::ejscreenapi(sitepoints$LONG, sitepoints$LAT, radius = 1)

}
