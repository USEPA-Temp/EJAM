# script example of running proximity analysis without Shiny app
# ****  presumes that other data are in global environment, ****
# **** like blockgroupstats, quaddata, etc. ****
if (FALSE) {
  
  
  
  # See details in help for ?EJAM
  
  # and note that right now batch.summarizer::ejscreenapi() is in that package not here
  
  # and see package census2020download for 2020 census data on blocks 
  
  # and facilities_prep may be obsolete or should be done before save that as dataset and build a package.
  
  
  # set up parameters, functions ####
  # includes library(EJAM) which provides datasets like blockgroupstats, facilities, etc.
  library(blockdata) # for 2010 data. 
  library(EJAM)
  library(data.table)
  data("blockwts")
  
  # SLOW ------------
  blockgroupstats <- ejscreen::bg21plus; blockgroupstats$bgfips <- blockgroupstats$FIPS
  setDT(blockgroupstats, key = c('bgfips'))
  
  # SLOW ------------
  # This must be done for each session?? - One cannot save it as .rda and just load via a pkg. 
  localtree <- SearchTrees::createTree(blockdata::quaddata, treeType = "quad", dataType = "point")
  
  
  # CountCPU <- 2
  CountCPU <- parallel::detectCores()
  indexgridsize <- 10  # This does not seem to be used ever - it is just used to create buffer_indexdistance which is not used.
  
  # can specify random test points (sites) ######
  #sitepoints <- points100example %>% head(1)# data in this package
  
  sitepoints <- data.table::copy(EJAM::points100example) # [1:5, ])
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
  
  
  
  # call function that finds nearby blocks  ####
  
  system.time({
    sites2blocks <- EJAM::getrelevantCensusBlocksviaQuadTree(
      sitepoints =  sitepoints,
      cutoff = radius,
      maxcutoff = maxcutoff,
      uniqueonly = uniqueonly,
      avoidorphans = avoidorphans,
      quadtree = localtree
    )
  }) # end of timed function
  
  
  
  # head(sites2blocks)
  
  # > head(sites2blocks)
  #    blockid  distance siteid
  # 1:  388798 0.9879380     29
  # 2:  388799 0.8800507     29
  # 3:  388809 0.6421582     29
  
  
  # save.image(file = 'saved image so far in testing.rda')
  
  out <- doaggregate(sites2blocks = sites2blocks)
  
  # DONE - can look at out$results_overall  and out$results_bysite
  
  
  
  #################################################################################
  
  # RESULTS FROM EJScreen 2.0 API for comparison
  for (fname in list.files('~/R/mypackages/ejscreenapi/R', pattern = '\\.R$',full.names = TRUE)) source(fname )
  outapi <- ejscreenapi_plus(sitepoints$lon, sitepoints$lat, radius = radius)
  
  weblinks <- outapi[ , c("EJScreenPDF", "EJScreenMAP" )]
  outapi <- outapi[ , which(!(names(outapi) %in% c("EJScreenPDF", "EJScreenMAP" )))]
  t(outapi[1:5,])
  ##  function that aggregates in each buffer  ####
  # system.time(
  # results_by_site <- EJAM::doaggregate(facilities = sitepoints, facilityblocks = results) 
  # )
  
  
  
  #################################################################################
  
}

#################################################################################
#################################################################################
#################################################################################
#################################################################################




stop('stopped here') 

# from doaggregate , as a script to test:



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
  # "VSI.eo", 
  "pctmin", "pctlowinc", "pctlths", "pctlingiso", "pctunder5", "pctover64", 'pctunemployed',
  "pctnhwa", "pcthisp", "pctnhba", "pctnhaa", "pctnhaiana", "pctnhnhpia", "pctnhotheralone", "pctnhmulti", 
  "flagged"
)
popmeancols <- c(
  #    *** should EJ Index percentiles be here too? do we use the popwtd mean of the state percentiles, but the lookedup US percentile of the popwtd mean raw EJ score??? ****** e.g.,  state.pctile.EJ.DISPARITY.dpm.eo
  'VSI.eo',   # Demog.Index <- weighted.mean(Demog.Index, w = pop)
  "pm", "o3", "cancer", "resp", "dpm", 
  "pctpre1960", "traffic.score", 
  "proximity.npl", "proximity.rmp", "proximity.tsdf", "proximity.npdes", 
  "ust", 
  "EJ.DISPARITY.pm.eo", "EJ.DISPARITY.o3.eo", "EJ.DISPARITY.cancer.eo", "EJ.DISPARITY.resp.eo", "EJ.DISPARITY.dpm.eo", 
  "EJ.DISPARITY.pctpre1960.eo", "EJ.DISPARITY.traffic.score.eo", 
  "EJ.DISPARITY.proximity.npl.eo", "EJ.DISPARITY.proximity.rmp.eo", "EJ.DISPARITY.proximity.tsdf.eo", "EJ.DISPARITY.proximity.npdes.eo", 
  "EJ.DISPARITY.ust.eo"
)

sites2blocks <- blockwts[sites2blocks, .(siteid,blockid,distance,blockwt,bgfips), on='blockid']
sites2blocks[, sitecount_near_block := .N, by=blockid] # (must use the table with duplicate blocks, not unique only) 

data.table::setorder(sites2blocks, siteid, bgfips, blockid) # new

#table(sites2blocks$sitecount_near_block) 
sites2blocks[, bg_fraction_in_buffer_bysite := sum(blockwt), by=c('bgfips', 'siteid')]

sites2blocks_overall <- unique(sites2blocks, by="blockid") 
sites2blocks_overall[, bg_fraction_in_buffer_overall := sum(blockwt), by=bgfips]  
rm(blockwts) ; gc()  # drop 6m row block table to save RAM # does not seem to be loaded to do that??

blockcount_by_site <- sites2blocks[, .(blockcount_near_site = .N), by=siteid] # new
bgcount_by_site <- sites2blocks[, .(bgcount_near_site = length(unique(bgfips))), by=siteid] # new
count_of_blocks_near_multiple_sites <- (NROW(sites2blocks) - NROW(sites2blocks_overall)) # NEW fraction is over /NROW(sites2blocks_overall)

sites2bgs_overall <- sites2blocks_overall[ , .(siteid, bgwt = sum(blockwt)), by=bgfips ]

sites2bgs_bysite  <- sites2blocks[ , .(bgwt = sum(blockwt, na.rm = TRUE)), by=.(siteid, bgfips)]
sites2bgs_bysite[ , sitecount_near_bg := length(unique(siteid)), by=bgfips] # do we need/want this for overall summary??
# join
sites2bgs_plusblockgroupdate_bysite  <- merge(sites2bgs_bysite,  
                                              blockgroupstats[ , c('bgfips', 'ST', ..countcols, ..popmeancols, ..calculatedcols)], 
                                              all.x = TRUE, all.y=FALSE, by='bgfips')  
sites2bgs_plusblockgroupdate_overall <- merge(sites2bgs_overall, 
                                              blockgroupstats[ , c('bgfips', 'ST', ..countcols, ..popmeancols, ..calculatedcols)], 
                                              all.x = TRUE, all.y=FALSE, by='bgfips') 

results_overall <- sites2bgs_plusblockgroupdate_overall[ ,  lapply(.SD, FUN = function(x) round(sum(x * bgwt, na.rm=TRUE), 1) ), .SDcols = countcols ]
results_bysite <- sites2bgs_plusblockgroupdate_bysite[ ,    lapply(.SD, FUN = function(x) round(sum(x * bgwt, na.rm=TRUE), 1) ), .SDcols = countcols, by = .(siteid) ]
results_overall_popmeans <- sites2bgs_plusblockgroupdate_overall[ ,  lapply(.SD, FUN = function(x) weighted.mean(x, w = bgwt * pop, na.rm = TRUE)), .SDcols = popmeancols ]
results_overall <- cbind(results_overall, results_overall_popmeans)
results_bysite_popmeans <- sites2bgs_plusblockgroupdate_bysite[ ,  lapply(.SD, FUN = function(x) weighted.mean(x, w = bgwt * pop, na.rm = TRUE)), by = .(siteid), .SDcols = popmeancols ]
results_bysite <- merge(results_bysite, results_bysite_popmeans)

results_bysite <- merge(results_bysite, blockcount_by_site) # new
results_bysite <- merge(results_bysite, bgcount_by_site) # new

# hardcoded formulas for now
results_overall[ , `:=`(
  pctover64 = ifelse(pop==0, 0, over64 / pop),
  pctunder5 = ifelse(pop==0, 0, under5 / pop),
  pcthisp = ifelse(pop==0, 0, as.numeric(hisp ) / pop),
  pctnhwa = ifelse(pop==0, 0, as.numeric(nhwa ) / pop),
  pctnhba = ifelse(pop==0, 0, as.numeric(nhba ) / pop) ,
  pctnhaiana = ifelse(pop==0, 0, as.numeric(nhaiana ) / pop),
  pctnhaa = ifelse(pop==0, 0, as.numeric(nhaa ) / pop), 
  pctnhnhpia = ifelse(pop==0, 0, as.numeric(nhnhpia ) / pop),
  pctnhotheralone = ifelse(pop==0, 0, as.numeric(nhotheralone ) / pop), 
  pctnhmulti = ifelse(pop==0, 0, as.numeric(nhmulti ) / pop),
  pctmin = ifelse(pop==0, 0, as.numeric(mins ) / pop), 
  pctlowinc = ifelse( povknownratio==0, 0, lowinc / povknownratio),                                                                                                                      
  pctlths = ifelse(age25up==0, 0, as.numeric(lths ) / age25up), 
  pctlingiso = ifelse( hhlds==0, 0, lingiso / hhlds), 
  pctpre1960 = ifelse( builtunits==0, 0, pre1960 / builtunits),
  pctunemployed = ifelse(unemployedbase==0, 0, as.numeric(unemployed) / unemployedbase)
) ]
# cbind(sum = prettyNum(results_overall, big.mark = ','))

results_bysite[ , `:=`(
  pctover64 = ifelse(pop==0, 0, over64 / pop),
  pctunder5 = ifelse(pop==0, 0, under5 / pop),
  pcthisp = ifelse(pop==0, 0, as.numeric(hisp ) / pop),
  pctnhwa = ifelse(pop==0, 0, as.numeric(nhwa ) / pop),
  pctnhba = ifelse(pop==0, 0, as.numeric(nhba ) / pop) ,
  pctnhaiana = ifelse(pop==0, 0, as.numeric(nhaiana ) / pop),
  pctnhaa = ifelse(pop==0, 0, as.numeric(nhaa ) / pop), 
  pctnhnhpia = ifelse(pop==0, 0, as.numeric(nhnhpia ) / pop),
  pctnhotheralone = ifelse(pop==0, 0, as.numeric(nhotheralone ) / pop), 
  pctnhmulti = ifelse(pop==0, 0, as.numeric(nhmulti ) / pop),
  pctmin = ifelse(pop==0, 0, as.numeric(mins ) / pop), 
  pctlowinc = ifelse( povknownratio==0, 0, lowinc / povknownratio),                                                                                                                      
  pctlths = ifelse(age25up==0, 0, as.numeric(lths ) / age25up), 
  pctlingiso = ifelse( hhlds==0, 0, lingiso / hhlds), 
  pctpre1960 = ifelse( builtunits==0, 0, pre1960 / builtunits),
  pctunemployed = ifelse(unemployedbase==0, 0, as.numeric(unemployed) / unemployedbase)
) ]



# missing:  id, lat, lon, Demog.Index which is VSI.eo, state.avg., state.pctile., us.avg., pctile., 
#  ST, Statename, REGION, 
#  NUM_NPL, NUM_TSDF, 
#  StatLayerCount, StatLayerZeroPopCount, 
#  weightLayerCount which might be the count of blocks nearby???
# "timeSeconds", "radius.miles", "unit", "statlevel", "inputAreaMiles"

t(results_bysite[1:5,])
sum(results_bysite$pop)            ###############   PROBLEM IF THOSE DO NOT LOOK FAIRLY SIMILAR    *************
results_overall$pop

##################################################### #
# FIND PERCENTILES THOSE RAW SCORES REPRESENT  ####
#  VIA  lookup tables of US/State  percentiles
##################################################### #

# results_bysite
# results_overall

# Use the dataset called EJAM::usastats as the lookup table for USA percentiles and mean. 
# but update/ fix it so it uses right variable names, etc., or replace with the one from ejscreen pkg

# hard coded for now:
varsneedpctiles <- c(ejscreen::names.e, union(ejscreen::names.d, 'pctunemployed'), ejscreen::names.d.subgroups, ejscreen::names.ej)
varnames.us.pctile <- paste0('pctile.', varsneedpctiles)
varnames.state.pctile <- paste0('state.pctile.', varsneedpctiles)

us.pctile.cols_bysite <- data.frame(matrix(nrow = NROW(results_bysite), ncol = length(varsneedpctiles))); colnames(us.pctile.cols_bysite) <- varnames.us.pctile
state.pctile.cols_bysite <- data.frame(matrix(nrow = NROW(results_bysite), ncol = length(varsneedpctiles))); colnames(state.pctile.cols_bysite) <- varnames.state.pctile
us.pctile.cols_overall <- data.frame(matrix(nrow = NROW(results_overall), ncol = length(varsneedpctiles))); colnames(us.pctile.cols_overall) <- varnames.us.pctile
state.pctile.cols_overall <- data.frame(matrix(nrow = NROW(results_overall), ncol = length(varsneedpctiles))); colnames(state.pctile.cols_overall) <- varnames.state.pctile

for (i in seq_along(varsneedpctiles)) {
  # older code 2015 had used  EJAM::lookup.pctile.US(results_bysite[ , myvar], .....  i=1
  myvar <- varsneedpctiles[i]
  if (myvar %in% names(ejscreen::lookupUSA)) {
    us.pctile.cols_bysite[ , varnames.us.pctile[[i]]] <- ejanalysis::lookup.pctile(unlist(results_bysite[  , ..myvar]), varname.in.lookup.table = myvar, lookup = ejscreen::lookupUSA) 
    us.pctile.cols_overall[, varnames.us.pctile[[i]]] <- ejanalysis::lookup.pctile(unlist(results_overall[ , ..myvar]), varname.in.lookup.table = myvar, lookup = ejscreen::lookupUSA) 
    # state.pctile.cols_bysite[ , varnames.state.pctile[[i]]] <- ejanalysis::lookup.pctile(unlist(results_bysite[  , ..myvar]), varname.in.lookup.table = myvar, lookup = ejscreen::lookupStates, zone =  results_bysite$ST) 
    # It may not make sense to do state percentiles except for EJ Indexes, in the "overall" summary:
    # state.pctile.cols_overall[, varnames.state.pctile[[i]]] <- ejanalysis::lookup.pctile(results_overall[ , varsneedpctiles[i]], varname.in.lookup.table = varsneedpctiles[i], lookup = ejscreen::lookupStates, zone =  results_overall$ST)
  } else {
    us.pctile.cols_bysite[ , varnames.us.pctile[[i]]] <- NA
    us.pctile.cols_overall[, varnames.us.pctile[[i]]] <- NA
  }
}

##################
# possibly add the us.avg.  and the state.avg.  for each key variable repeated in each row(site), since the ejscreenAPI does that. 
##################



# shiny::runApp('~/R/mypackages/ejscreenapi')

