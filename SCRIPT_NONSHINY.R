# script example of running proximity analysis without Shiny app
# **** as written currently, presumes that other data are in global environment, ****
# **** like blockgroupstats, blockdata, quaddata, etc. ****
if (1 ==0) {

# See details in help for ?bufferfast

# and note that right now proxistat::bufferapi() is in that package not here

# and see  census2020download::blocks2020  for newer census data on blocks

# and facilities_prep may be obsolete or should be done before save that as dataset and build a package.


# setup parameters, functions ####
# - get data by loading package and some constants etc.
# includes library(bufferfast) which provides datasets like blockdata, blockgroupstats, facilities, etc.

source('global.R')
source("R/getRelevantCensusBlocksviaQuadTree.R")
source("R/getRelevantCensusBlocksviaQuadTree_Clustered.R")
# specify random test points (sites) ######

#dataLocDT <- points100example %>% head(1)# data in this package
dataLocDT <- points1000example
dataLocDT[, ID := .I]

facilities <- dataLocDT
# or alternatively just create a data.table with columns LONG, LAT
#### or, Using ejscreen and proxistat packages from github (MC's):
# mysites <- proxistat::testpoints_bg20(100)
# names(mysites) <- gsub('lat', 'LAT', names(mysites))
# names(mysites) <- gsub('lon', 'LONG', names(mysites))
# dataLocDT <- data.table::as.data.table( mysites )


# specify radius for circular buffer and other key parameters ####

cutoff <- 1 # radius (miles?)
maxcuttoff <- 50  # max distance to expand search to (miles?)
avoidorphans <- TRUE  # Expand distance searched, when a facility has no census block centroid within selected buffer distance
uniqueonly <- TRUE    # TRUE = stats are for dissolved single buffer to avoid double-counting. FALSE = we want to count each person once for each site they are near.


# call function that finds nearby blocks  ####

system.time(

  # ON MAC THIS CRASHES R ENTIRELY - due to lack of support for multithreaded something or other??:

  results <- getrelevantCensusBlocksviaQuadTree(

    # **** as written currently, presumes that other data are in global environment, ****
    # **** especially it uses quaddata ****

    facilities =  dataLocDT,
    cutoff = cutoff,
    maxcutoff = maxcuttoff,
    uniqueonly = uniqueonly,
    avoidorphans = avoidorphans)

)


system.time(

  ej_api_results <- bufferapi(dataLocDT$LONG, dataLocDT$LAT, radius = 1)


)

all_ej_api_results <- ej_api_results %>%
  map_dfr(~ .)


results <- results %>%
  left_join(blockdata::blockdata, by = "BLOCKID")


ej_pop <- sum(as.numeric(all_ej_api_results$totalPop))
ej_pop

quadtree_pop <- sum(results$POP100)
quadtree_pop


# call function that aggregates in each buffer  ####

system.time(

  # **** as written currently, presumes that other data are in global environment, ****
  # *** this uses blockdata and blockgroupstats ***

  dat <- doaggregate(dataLocDT, results)
)

# see results ####

head(dat)


}
