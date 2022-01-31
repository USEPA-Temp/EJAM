create_bgstats2020 <- function() {
  ## script to create bgstats2020.rda
  
  b2 <- ejscreen::bg20 # EJAM::blockgroupstats) # work with it as a data.frame not data.table until a later step
  # drop the bin number for percentiles, which just tells what decile except 10 is 90-95th pctile and bin 11 is 95-100, like ejscreen orange and red map colors in choropleths 
  dropping <- grep('^bin\\.', names(b2), value = TRUE )
  # drop the text labels describing the percentile values as in popups
  dropping <- c(dropping, grep('^pctile\\.text', names(b2), value = TRUE ) ) # certainly do not need pctile.text... cols
  dropping <- c(dropping, grep('^pctile\\.', names(b2), value = TRUE)) # probably do not need these - pctiles for buffer scores are looked up, not calculated as popwtd means, right?
  dropping <- c(dropping, "VNI.eo", "VDI.eo") #obsolete, they were basis for alt1 and alt2 EJ Indexes. VNI.eo is just mean of mins and lowinc counts. VSI.eo is mean of pctlowinc and pctmin, simple avg of those 2, treating as if denominator is pop for both.
  dropping <- c(dropping,  "FIPS.TRACT", "FIPS.COUNTY", 'countyname', "FIPS.ST", "ST", "statename", "REGION") # none stay useful if just using blockgroupstats for buffer summary since buffer can span multiple states, etc.
  dropping <- c(dropping, "AREALAND", "AREAWATER", 'area') # could be analyzed as a count variable to get total area, but if circular buffer pi * radius^2 is easier
  dropping <- c(dropping,  "OBJECTID") # maybe keep ??
  dropping <- c(dropping, 'Shape_Length') # to avoid issue when merge since both files have these cols
  dropping <- c(dropping,  c('lat', 'lon')) #  this was just lat lon of a point in the blockgroup.
  dropping <- c(dropping,  c('flagged')) # if any EJ index is at 80+ pctile US for this bg?
  
  b2 <- b2[ , !names(b2) %in% dropping]
  
  names(b2) <- gsub('FIPS', 'bgfips', names(b2))
  names(b2) <- gsub('NPL_CNT', 'count.NPL', names(b2))
  names(b2) <- gsub('TSDF_CNT', 'count.TSDF', names(b2))
  
  # drop fields already in b2
  subgroups <- ejscreen::bg20DemographicSubgroups2014to2018
  subgroups$mins <- NULL
  subgroups$pctmin <- NULL
  subgroups$pop <- NULL
  
  b2 <- merge(b2, subgroups, by.x = 'bgfips', by.y = 'FIPS', all = TRUE)
  
  # I removed ST column but had seen PR is an issue, differs;
  # all.equal(b2$pop.x[b2$ST != 'PR'], b2$pop.y[b2$ST != 'PR'])
  # (all.equal(bg20$FIPS,b2$BLOCKGROUPFIPS))
  # 
  # names(b2) <- gsub('BLOCKGROUPFIPS', 'bgfips', names(b2))
  
  
  # setdiff(names(ejscreen::bg20), names(b2) )
  # setdiff(names(b2) ,names(ejscreen::bg20))
  bgstats2020 <- b2
  data.table::setDT(bgstats2020, key = 'bgfips') # by reference only
  # bgstats2020 <- data.table::data.table(bgstats2020, key = 'bgfips') # makes a copy
  rm(b2); rm(subgroups);rm(dropping)
  
  invisible(bgstats2020)
  # bgstats2020 uses 116 MB according to tables()
  # without the race eth subgroups it was just 87 MB !
  # saveRDS(bgstats2020, file = './data/bgstats2020.rda')
}
