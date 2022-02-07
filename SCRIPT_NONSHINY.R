# script example of running proximity analysis without Shiny app
# **** as written currently, presumes that other data are in global environment, ****
# **** like blockgroupstats, blockdata, quaddata, etc. ****
if (1 ==0) {

  # See details in help for ?EJAM

  # and note that right now batch.summarizer::ejscreenapi() is in that package not here

  # and see  census2020download::blocks2020  for newer census data on blocks but may move to EJAM-Blockdata?
  
  # and facilities_prep may be obsolete or should be done before save that as dataset and build a package.


  # setup parameters, functions ####
  # - get data by loading package and some constants etc.
  # includes library(EJAM) which provides datasets like blockgroupstats, facilities, etc.
  library(blockdata) # may move it to EJAM-Census2020download
  library(EJAM)
  library(data.table)

  CountCPU <- 2
  indexgridsize <- 10  # This does not seem to be used ever - it is just used to create buffer_indexdistance which is not used.

  # can specify random test points (sites) ######
  #sitepoints <- points100example %>% head(1)# data in this package

  sitepoints <- data.table::copy(EJAM::points1000example)
  # sitepoints <- data.table::copy(EJAM::points100example)   # NOTE the first point is far outside the continental US and returns no data using census 2010 blocks.
  sitepoints[ , siteid := .I]
  data.table::setnames(sitepoints, 'LAT', 'lat')
  data.table::setnames(sitepoints, 'LONG', 'lon')
  data.table::setkey(sitepoints) #,  c('siteid', 'lat', 'lon'))

  # specify radius for circular buffer and other key parameters ####

  radius <- 1 # radius (miles)
  maxcutoff <- 31.07 # 50 km  # max distance to expand search to (miles?)
  avoidorphans <- TRUE  # Expand distance searched, when a facility has no census block centroid within selected buffer distance
  uniqueonly <- FALSE    # TRUE = stats are for dissolved single buffer to avoid double-counting. FALSE = we want to count each person once for each site they are near.

  ### IS THIS NEEDED? seems to be used only in the clustered version of getrelevant??
  # localtree <- blockquadtree
  #  localtree <- SearchTrees::createTree(blockdata::quaddata, treeType = "quad", dataType = "point")

  # save.image('tempjunk.RData')

  # blockdata[ , sum( , na.rm = TRUE), by = BLOCKGROUPFIPS]
  # blockwts

  # call function that finds nearby blocks  ####

  system.time({
    # results_by_site <- summarizeForFacilities(

    # **** as written currently, presumes that other data are in global environment, ****
    # **** especially it uses quaddata ****
    sites2blocks <- getrelevantCensusBlocksviaQuadTree(
      sitepoints =  sitepoints,
      cutoff = radius,
      maxcutoff = maxcutoff,
      uniqueonly = uniqueonly,
      avoidorphans = avoidorphans
    )
  }
  )
  # 1000 * 3600/13

  # # call function that aggregates in each buffer  ####
  #
  # system.time(
  #
  #   # **** as written currently, presumes that other data are in global environment, ****
  #   # *** this uses blockdata and blockgroupstats ***
  #
  #   results_by_site <- doaggregate(sitepoints, results)
  # )
  #
  # # see results ####
  #
  # head(results_by_site)

    # ej_api_results <- ejscreenapi(sitepoints$LONG[1:10], sitepoints$LAT[1:10], radius = 1)
  #   #ej_api_results <- batch.summarizer::ejscreenapi(sitepoints$LONG, sitepoints$LAT, radius = 1)

  #
  # ej_api_results <- ej_api_results %>%
  #   dplyr::relocate(
  #     c(lon, lat),
  #     .before = RAW_E_PM25
  #   )
  #
  # ej_pop <- sum(as.numeric(ej_api_results$totalPop))
  # ej_pop
  #
  # quadtree_pop <- sum(results$POP100)
  # quadtree_pop

}
