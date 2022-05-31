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

  
  # CountCPU <- 2
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

  radius <- 3 # radius (miles)
  maxcutoff <- 31.07 # 50 km  # max distance to expand search to, if avoidorphans=TRUE
  avoidorphans <- TRUE  # Expand distance searched, when a facility has no census block centroid within selected buffer distance.
  
  uniqueonly <- FALSE   # The uniqueonly parameter will be removed from getrelevant... and handled in doaggregate() 
  
  
if (FALSE) {
  # RESULTS FROM EJScreen 2.0 API for comparison
  outapi_3mile_100sites <- ejscreenapi_plus(sitepoints$lon, sitepoints$lat, radius = radius)
  head(outapi_3mile_100sites)
}  
  
  
  
  
  # This must be done for each session?? - One cannot save it as .rda and just load via a pkg. 
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
  }) # end of timed function
  
  
  save.image(file = 'saved image so far in testing.rda')

  
  # > head(sites2blocks)
  #    blockid  distance siteid
  # 1:  388798 0.9879380     29
  # 2:  388799 0.8800507     29
  # 3:  388809 0.6421582     29
  
  
  # from doaggregate2, as a script to test:
  blockgroupstats <- ejscreen::bg21plus; blockgroupstats$bgfips <- blockgroupstats$FIPS
  setDT(blockgroupstats, key = c('bgfips'))
  
  countcols <- c(
    "pop", 'nonmins', "mins", 
    "lowinc",   "povknownratio",   
    "lths",     "age25up", 
    "lingiso",  "hhlds", 
    "under5", "over64",
    "unemployed",   "unemployedbase", # new in 2022
    'pre1960',  'builtunits',
    "nhwa", "hisp", "nhba", "nhaa", "nhaiana", "nhnhpia", "nhotheralone", "nhmulti" # not in EJScreen 2.0 but will use here
  )
  calculatedcols <- c(
    "VSI.eo", "pctmin", "pctlowinc", "pctlths", "pctlingiso", "pctunder5", "pctover64", 'pctunemployed',
    "pctnhwa", "pcthisp", "pctnhba", "pctnhaa", "pctnhaiana", "pctnhnhpia", "pctnhotheralone", "pctnhmulti", 
    "flagged"
    )
popmeancols <- c(
    "pm", "o3", "cancer", "resp", "dpm", 
    "pctpre1960", "traffic.score", 
    "proximity.npl", "proximity.rmp", "proximity.tsdf", "proximity.npdes", 
    "ust", 
    "EJ.DISPARITY.pm.eo", "EJ.DISPARITY.o3.eo", "EJ.DISPARITY.cancer.eo", "EJ.DISPARITY.resp.eo", "EJ.DISPARITY.dpm.eo", 
    "EJ.DISPARITY.pctpre1960.eo", "EJ.DISPARITY.traffic.score.eo", 
    "EJ.DISPARITY.proximity.npl.eo", "EJ.DISPARITY.proximity.rmp.eo", "EJ.DISPARITY.proximity.tsdf.eo", "EJ.DISPARITY.proximity.npdes.eo", 
    "EJ.DISPARITY.ust.eo"
  )
library(data.table); library(blockdata); data("blockwts")
sites2blocks <- blockwts[sites2blocks, .(siteid,blockid,distance,blockwt,bgfips), on='blockid']
sites2blocks[, sitecount_near_block := .N, by=blockid] # (must use the table with duplicate blocks, not unique only)
sites2blocks_overall <- unique(sites2blocks, by="blockid") 
sites2blocks_overall[, bg_fraction_in_buffer_overall := sum(blockwt), by=bgfips]  
sites2blocks[, bg_fraction_in_buffer_bysite := sum(blockwt), by=c('bgfips', 'siteid')]
rm(blockwts) ; gc()  # drop 6m row block table to save RAM # does not seem to be loaded to do that??
sites2bgs_overall <- sites2blocks_overall[ , .(bgwt = sum(blockwt)), by=bgfips ]
sites2bgs_bysite  <- sites2blocks[ , .(bgwt = sum(blockwt, na.rm = TRUE)), by=.(siteid, bgfips)]
sites2bgs_bysite[ , sitecount_near_bg := length(unique(siteid)), by=bgfips] 
# join
sites2bgs_plusblockgroupdate_bysite  <- merge(sites2bgs_bysite,  
                                              blockgroupstats[ , c('bgfips', ..countcols, ..popmeancols, ..calculatedcols)], 
                                              all.x = TRUE, all.y=FALSE, by='bgfips')  
sites2bgs_plusblockgroupdate_overall <- merge(sites2bgs_overall, 
                                              blockgroupstats[ , c('bgfips', ..countcols, ..popmeancols, ..calculatedcols)], 
                                              all.x = TRUE, all.y=FALSE, by='bgfips') 
results_overall <- sites2bgs_plusblockgroupdate_overall[ ,  lapply(.SD, FUN = function(x) round(sum(x * bgwt, na.rm=TRUE), 1) ), .SDcols = countcols ]
results_bysite <- sites2bgs_plusblockgroupdate_bysite[ ,  lapply(.SD, FUN = function(x) round(sum(x * bgwt, na.rm=TRUE), 1) ), by = .(siteid), .SDcols = countcols ]
results_overall_popmeans <- sites2bgs_plusblockgroupdate_overall[ ,  lapply(.SD, FUN = function(x) weighted.mean(x, w = bgwt * pop, na.rm = TRUE)), .SDcols = popmeancols ]
results_overall <- cbind(results_overall, results_overall_popmeans)
results_bysite_popmeans <- sites2bgs_plusblockgroupdate_bysite[ ,  lapply(.SD, FUN = function(x) weighted.mean(x, w = bgwt * pop, na.rm = TRUE)), by = .(siteid), .SDcols = popmeancols ]
results_bysite <- merge(results_bysite, results_bysite_popmeans)



  
  
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
