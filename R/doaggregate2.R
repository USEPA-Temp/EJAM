#' Summarize indicators in each buffer (given the blocks in each buffer and indicators for each block)
#'
#' @description This updated 2022 code takes a set of facilities and the set of blocks that are near each,
#'   (as identified previously, in other code that has identified which blocks are nearby)
#'   and combines those with indicator scores for block groups.
#'
#'   It aggregates the blockgroup scores to create a summary of each indicator,
#'    in each buffer (i.e., near each facility):
#'      -Sums of counts, such as for population or number of households or Hispanics
#'      -Calculated variables for the buffer via formulas based on aggregated counts, 
#'        such as percent low income.
#'      -Population-Weighted means for EJ Indexes or Environmental indicators.
#'
#' @details
#'  \preformatted{
#'
#'   requires the following as data lazy loaded for example from blockdata package
#'
#'    blockdata  data.table (was 335 MB as .rdata) with these columns: blockfips, bgfips, STUSAB, blockpop2010, bgpop2010, blockwt, 
#'    # INTPTLAT, INTPTLON, BLOCK_LAT_RAD, BLOCK_LONG_RAD  BLOCK_X   BLOCK_Y  BLOCK_Z ID GRID_X GRID_Y GRID_Z 
#'    # old names: blockid, BLOCKGROUPFIPS, STUSAB, STATE, COUNTY, TRACT, BLKGRP, BLOCK, POP100, HU100, Census2010Totalpop
#'    
#'    blockgroupstats - A data.table (such as EJSCREEN demographic and environmental data by blockgroup?)
#'    statesshp   (a shapefile of state boundries to determine what state a point is in)
#'    stateregions  data.table lookup of EPA REGION given the ST (state code like AK)
#'
#'    only these are passed to the function rather than being already in the global envt:
#'      facilityblocks - passed to the function
#'      facilities
#'
#'    Check which table has which column names, including columns with
#'       distance, statename?,  REGION etc.
#'
#'    Check if it includes Puerto Rico ( that was dropped?? - but it is in blocks, and in blockgroupstats for most fields though not race/ethnic subgroups)
#'  }
#'
#' @param sites2blocks data.table of distances in miles between all sites (facilities) and 
#'   nearby Census block internal points, with columns siteid, blockid, distance,
#'   created by getrelevant... function. 
#' @param ... more to pass
#' @import data.table
#' @import blockdata
#' @export
#'
doaggregate2 <- function(sites2blocks, countcols=NULL,popmeancols=NULL,calculatedcols=NULL,  ...) {
  
  
    # HARDCODED FOR NOW:
  # from EJSCREEN dataset, names as in ejscreen package:
  # **** but we probably treat pctpre1960 as pop wtd mean like other Evars?
  
  if (is.null(countcols)) {
    countcols <- c(
      "pop", "mins", 'nonmins',
      "lowinc", "povknownratio",   
      "lths", "age25up", 
      "lingiso", "hhlds", 
      "under5", "over64",
      "unemployed","unemployedbase",
      'pre1960','builtunits',
      "nhwa", "hisp", "nhba", "nhaa", "nhaiana", "nhnhpia", "nhotheralone", "nhmulti"
    )
  }  
  if (is.null(calculatedcols)) {
    # calculatedcols <- c(ejscreen::names.d, 'flagged') 
    # or to avoid depending on ejscreen package, 
    # dput(c(ejscreen::names.d, 'flagged') )
    calculatedcols <- c("VSI.eo", "pctmin", "pctlowinc", "pctlths", "pctlingiso", "pctunder5", 
                        "pctover64", "flagged")
    # These must be calculated after aggregating count variables and using those at siteid level. 
    # Use ejscreen::ejscreenformulas$formula to calculate these.
  }
  if (is.null(popmeancols)) {
    # popmeancols <- c(ejscreen::names.ej, ejscreen::names.e) 
    # or to avoid depending on ejscreen package, 
    # dput(c(ejscreen::names.ej, ejscreen::names.e) )
    popmeancols <- c("EJ.DISPARITY.pm.eo", "EJ.DISPARITY.o3.eo", "EJ.DISPARITY.cancer.eo", 
                     "EJ.DISPARITY.resp.eo", "EJ.DISPARITY.dpm.eo", "EJ.DISPARITY.pctpre1960.eo", 
                     "EJ.DISPARITY.traffic.score.eo", "EJ.DISPARITY.proximity.npl.eo", 
                     "EJ.DISPARITY.proximity.rmp.eo", "EJ.DISPARITY.proximity.tsdf.eo", 
                     "EJ.DISPARITY.proximity.npdes.eo", "pm", "o3", "cancer", "resp", 
                     "dpm", "pctpre1960", "traffic.score", "proximity.npl", "proximity.rmp", 
                     "proximity.tsdf", "proximity.npdes")
    # *** we treat pctpre1960 as if can do popwtd avg, right? Technically pctpre1960 should use ejscreenformulas... ratio of sums of counts pre1960 and denom builtunits  
    # only 3 of names.d are exactly popmeans,  ("pctmin", "pctunder5", "pctover64") since denominators are pop. 
    #   May as well just calculate all of the names.d.pct exactly not some as popwtd mean and others not.
    # flagged is a variable that maybe has an obvious single way to be aggregated for a buffer? 
    # It could signal if any EJ>80 for avg person as avg of each EJ index for all residents in buffer, 
    # (or alternatively could perhaps tell us if there is any flagged bg at all in buffer?).
  }  
  
    
  # HANDLING DOUBLE COUNTING
  # Some steps are the same for overall and site-by-site so it is more efficient to do both together if want both. 
  # The uniqueonly parameter got removed from getrelevant... to be handled in doaggregate() 
  # uniqueonly <- FALSE  meant that it returned overall unique blocks summary. 
  # FALSE = we want to count each person once for each site they are near.
  # TRUE = stats are for dissolved single buffer to avoid double-counting. 
  
  # SEE NOTES ON HOW TO MAKE AGGREGATE...
  
  # Get the block weights only for the blocks found near these analyzed sites
  # via merge, or possibly via a data.table-specific join that avoids making a copy
  # like blockwts[sites2blocks, ]
  
  
  # CODE #############################################################################################
  
  if (testing) {
    
    # FOR TESTING 
    library(data.table); library(blockdata); data("blockwts")
    data('sites2blocks_example') # it is called  sites2blocks_example
    sites2blocks <- sites2blocks_example 
  }
  # data.table::setkey(result, "blockid", "siteid", "distance") #  has been done by getrelevant.. now
  
  # but need blockid, not fips.   *********************  THIS IS SLOW:
  # sites2blocks***** <- merge(blockdata::blockid2fips, sites2blocks, by='blockfips', all.x=FALSE, all.y=TRUE)
  # sites2blocks$blockid <- blockdata::blockid2fips[sites2blocks, .(blockid), on='blockfips']
  # sites2blocks[,blockfips := NULL]
  # data.table::setkey(sites2blocks, 'blockid', 'siteid')
  
  
  # blocks #######################################
  
  # get weights for nearby blocks
  # sites2blocks <- merge(sites2blocks, blockwts, by='blockid', all.x	=TRUE, all.y=FALSE) # incomparables=NA
  sites2blocks <- blockwts[sites2blocks, .(siteid,blockid,distance,blockwt,bgfips), on='blockid']
  # rm(blockwts) #; gc()  # drop 6m row block table to save RAM # does not seem to be loaded to do that??
  
  # optional: Calc # of sites nearby each block: How many of these sites are near this resident? this bg? avg resident overall? 
  sites2blocks[, sitecount_near_block := .N, by=blockid] # (must use the table with duplicate blocks, not unique only)
  
  # drop duplicate blocks, which are residents near 2 or more sites, to avoid double-counting them
  sites2blocks_overall <- unique(sites2blocks, by="blockid")
  
  # Calc avg person's proximity (block-level), by bg: censuspop-wtd mean of block-specific distances, for each bg 
  sites2blocks_overall[, bg_fraction_in_buffer_overall := sum(blockwt), by=bgfips]  
  sites2blocks[, bg_fraction_in_buffer_bysite := sum(blockwt), by=c('bgfips', 'siteid')]
  
  # was going to try to do join of weights and the aggregation by blockid all in this one step? but not tested 
  # and may need to do intermed step 1st, where 
  # sites2bg <- blockwts[sites2blocks, .(siteid, bgfips, distance, bgwt = sum(blockwt, na.rm=TRUE)), on = 'blockid', by =.(siteid, bgfips)] 

  ## why do sum(blockwt) by bgfips  here AGAIN, if already did it above?
  
    # rollup as blockgroups - Aggregate blocks into blockgroups, per siteid ***  #######################################
  # Calc bgwt, the fraction of each (parent)blockgroup's censuspop that is in buffer
  sites2bgs_overall <- sites2blocks_overall[ , .(bgwt = sum(blockwt)), by=bgfips ]
  sites2bgs_bysite  <- sites2blocks[ , .(bgwt = sum(blockwt, na.rm = TRUE)), by=.(siteid, bgfips)]
  
  # optional: Calc maybe # of unique sites nearby each blockgroup
  sites2bgs_bysite[ , sitecount_near_bg := length(unique(siteid)), by=bgfips] 
  #slow!:
  if (!testing) {rm(sites2blocks); gc()} # unless need save that to analyze distance distribution 

  
  #  Maybe want some extra summary stats across people and sites (about the distribution), one column per indicator. 
  #  BUT MOST OF THE INTERESTING STATS LIKE MEDIAN PERSON'S SCORE, OR WORST BLOCKGROUP, 
  #  HAVE TO BE CALCULATED FROM BG DATA BEFORE WE AGGREGATE WITHIN EACH SITE (BUFFER)... 
  #  Same for sites: worst site as measured by highest nearby blockgroup-level %poor needs raw bg data before summarized by siteid.
  
  
  
  # 2) *** JOIN *** the midsized intermed table to blockgroupstats   ################################
  
   
  
  #    NEEED TO DO JOIN HERE OF **blockgroupstats**   200 columns, on bgid 
  
  
  
  countcols <- c('pop', 'mins', ejscreen::names.d.subgroups.count) # examples 
  popmeancols <- c(names.e, names.ej) # we want the raw scores only for EJ and E, or pct only for demog.
  calculatedcols <- names.d # use formulas for these
  
  sites2bgs_overall <- sites2bgs_overall[blockgroupstats, , on=]
  
  
  
  # COUNTS OVERALL
  results_overall <- sites2bgs_overall[ , .(countcols = lapply(.SD, FUN = function(x) sum(x * bgwt, na.rm=TRUE))), .SDcols = countcols] 
  
  # COUNTS BY SITE
  results_bysite <- sites2bgs_overall[ , .(countcols = lapply(.SD, FUN = function(x) sum(x * bgwt, na.rm=TRUE))), by = siteid, .SDcols = countcols] 
  # or is it like this...? ***************************
  # sites2bg[, lapply(.SD, countcols   :=                                           sum(blockwt * blockgroupstats[,  countcols], na.rm=T)), by = .(siteid), .SDcols = countcols]  

    
  # POPMEAN OVERALL
  results_overall[ , .(popmeancols := lapply(.SD, FUN = function(x) weighted.mean(x, w = bgwt * pop, na.rm = TRUE))), .SDcols = popmeancols  ]
  
  # POPMEAN BY SITE
  results_bysite[  , .(popmeancols := lapply(.SD, FUN = function(x) weighted.mean(x, w = bgwt * pop, na.rm = TRUE))), by = siteid, .SDcols = popmeancols]
  # or is it like this...?? ***************************
  # sites2bg[,           lapply(.SD, popmeancols := sum(pop * blockwt * blockgroupstats[,popmeancols], na.rm=T) / sum(pop * blockwt, na.rm=T)), by = .(siteid), .SDcols = popmeancols]
  
  
  # next it needs 
  
  warning('work in progress stops here')  # 3############   code below is older 
  
  results <- list(results_overall = results_overall, results_bysite = results_bysite)
  
  
  # 
  # # filter out any rows with missing values
  # facilities <- facilities[!is.na(facilities$LONG) & !is.na(facilities$LAT),]
  
  ########### Create locations lookup   ###########
  # 
  # #through nearest block
  # nearestlocationdata <- data.table::as.data.table(extendedfacilityblocks_ext[,.(blockid, distance, siteid)])
  # data.table::setkey(nearestlocationdata, "siteid", "distance", "blockid")
  # uniquelocations <- data.table::as.data.table(unique(nearestlocationdata, by = c("ID")))
  # 
  # uniquelocationdata <- merge(uniquelocations, extendedfacilityblocks_ext, by = c("ID", "blockid"))
  # aux_locations <- data.table::as.data.table(uniquelocationdata[,.(ID, blockid, distance.x, BLOCKGROUPFIPS, STUSAB = STUSAB, STATE = statename, COUNTY, TRACT, BLKGRP, BLOCK, REGION)])
  
  ############ through shapefile
  # prime_locations <- merge_state_shapefiles(facilities, statesshp)
  # prime_locations <- data.table::as.data.table(prime_locations@data)
  
  #  get state abbreviations from stateregions (but that could be replaced by a function that returns )
  # prime_locations <- merge(prime_locations, stateregions, by.x = "STATE", by.y = "STATENAME")
  # prime_locations <- prime_locations[,.(ID, STATE, STUSAB = ST)]
  # data.table::setkey(prime_locations, "STUSAB")
  # data.table::setkey(stateregions, "ST")
  # prime_locations <- merge(prime_locations, stateregions, all.x = TRUE, all.y = FALSE, by.x = "STUSAB", by.y = "ST")
  # 
  # prime_locations <- data.table::as.data.table(prime_locations[,.(ID, blockid = NA, distance.x = NA, BLOCKGROUPFIPS = NA, STUSAB, STATE, COUNTY = NA, TRACT = NA, BLKGRP = NA, BLOCK = NA, REGION)] )
  # 
  # incompletes <- data.table::as.data.table(prime_locations[is.na(STUSAB),.(ID)])
  # ################just take those complete
  # prime_locations <- prime_locations[!is.na(STUSAB),]
  # 
  # data.table::setkey(incompletes, "ID")
  # data.table::setkey(aux_locations, "ID")
  # aux_locations <- merge(incompletes, aux_locations, all.x = TRUE, all.y = FALSE)
  # 
  # locations <- rbind(prime_locations, aux_locations)
  
  
  ###################################################
  ###########   CALCULATE INDICATORS USING FORMULAS, BASED ON THE ROLLED UP COUNTS 
  # 
  # ######### create demographic index
  # result[, "VSI.eo"] <- (result$pctmin + result$pctlowinc ) /2

  # #create EJ indexes
  # result[, "inedx_EJ_Traffic"] <- result$traffic.score * result$POP100 * (result$VSI.eo - National_Demographic_Index)
  # etc
  
  ###################################################
  
  ########### FIND PERCENTILES THOSE RAW SCORES REPRESENT 
  #  VIA  lookup tables of US/State/Regional percentiles
  # lres <- data.table::as.data.table(lookup.pctile.US(result)) # ************
  # result <- cbind(result, lres)
  # 
  # ###### old code used a loop through states 
  
  # states <- unique(result$STUSAB) #recycled in the data
  # state_result <- list()
  # stateindex <- 1
  # for (state in states) {
  #   partial_state <- result[result$STUSAB == state,]
  #   partial_states_processed <- data.table::as.data.table(lookup.pctile.State(partial_state, state))
  #   state_result[[stateindex]] <- data.table::as.data.table(cbind(partial_state, partial_states_processed))
  #   stateindex <- stateindex + 1
  # }
  # result <- data.table::rbindlist(state_result)
  
  
  return(result)
}
