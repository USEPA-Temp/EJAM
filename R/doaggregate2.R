#' Summarize indicators in each buffer (given the blocks in each buffer and indicators for each block)
#'
#' @description This updated 2022 code takes a set of facilities and the set of blocks that are near each,
#'   (as identified previously, in other code that has identified which blocks are nearby)
#'   and combines those with indicator scores for block groups.
#'
#'   It aggregates the scores as population weighted means, (check that - all of them?)
#'   creating a summary of each indicator, in each buffer (i.e., near each facility).
#'
#' @details
#'  \preformatted{
#'
#'   As currently written, requires the following already be in the global environment,
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
doaggregate2 <- function(sites2blocks, ...) {
  
  # @param popmeancols   # **********************************************
  # @param countcols vector of column names   # **********************************************
  #
  #  popmeancols=popmeancols_default, countcols=countcols_default, indicator_formulas=indicator_formulas_default
  
  
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
  
  # testing
  
  library(data.table); library(blockdata); data("blockwts")
  data('sites2blocks_example') # it is called  sites2blocks 
  data.table::setnames(sites2blocks, 'blockid', 'blockfips')
  data.table::setnames(sites2blocks, 'BLOCKID', 'blockfips')
  data.table::setkey(sites2blocks, 'blockfips', 'siteid')
  
  # but need blockid, not fips.   *********************  THIS IS SLOW:
  # sites2blocks***** <- merge(blockdata::blockid2fips, sites2blocks, by='blockfips', all.x=FALSE, all.y=TRUE)
  sites2blocks$blockid <- blockdata::blockid2fips[sites2blocks, .(blockid), on='blockfips']
  sites2blocks[,blockfips := NULL]
  data.table::setkey(sites2blocks, 'blockid', 'siteid')
  
  
  # blocks #######################################
  
  # get weights for nearby blocks
  # sites2blocks <- merge(sites2blocks, blockwts, by='blockid', all.x	=TRUE, all.y=FALSE) # incomparables=NA
  sites2blocks <- blockwts[sites2blocks, .(siteid,blockid,distance,blockwt,bgfips), on='blockid']
  rm(blockwts) #; gc()  # drop 6m row block table to save RAM
  
  # drop duplicate blocks, which are residents near 2 or more sites, to avoid double-counting them
  sites2blocks_overall <- unique(sites2blocks, by="blockid")
  
  # Calc avg person's proximity (block-level), by bg: censuspop-wtd mean of block-specific distances, for each bg 
  sites2blocks_overall[,bg_fraction_in_buffer_overall := sum(blockwt), by=bgfips]  
  sites2blocks[, bg_fraction_in_buffer_bysite := sum(blockwt), by=c('bgfips', 'siteid')]
  
  # optional: Calc # of sites nearby each block: How many of these sites are near this resident? this bg? avg resident overall? 
  sites2blocks[, sitecount_near_block := .N, by=blockid] # (must use the table with duplicate blocks, not unique only)
  
  # rollup as blockgroups - Aggregate blocks into blockgroups, per siteid ***  #######################################
  # Calc bgwt, the fraction of each (parent)blockgroup's censuspop that is in buffer
  sites2bgs_overall <- sites2blocks_overall[ , bgwt := sum(blockwt), by=bgfips ]
  sites2bgs_bysite  <- sites2blocks[ , bgwt := sum(blockwt, na.rm = TRUE), by=.(siteid, bgfips)]
  
  # optional: Calc maybe # of unique sites nearby each blockgroup
  sites2bgs_overall[ , sitecount_near_bg := length(unique(siteid)), by=bgfips] 
  #slow!:
  rm(sites2blocks); gc() # unless need save that to analyze distance distribution 
  
  
  #  Maybe want some extra summary stats across people and sites (about the distribution), one column per indicator. 
  #  BUT MOST OF THE INTERESTING STATS LIKE MEDIAN PERSON'S SCORE, OR WORST BLOCKGROUP, 
  #  HAVE TO BE CALCULATED FROM BG DATA BEFORE WE AGGREGATE WITHIN EACH SITE (BUFFER)... 
  #  Same for sites: worst site as measured by highest nearby blockgroup-level %poor needs raw bg data before summarized by siteid.
  
  
  
  # 2) *** JOIN *** the midsized intermed table to blockgroupstats   ################################
  
  # Notes on using variable with list of colnames, to 
  # apply function to specified columns and 
  # assign results with specified variable names to the original data.
  #  but I don't need to rename the indicators actually.
  # 
  # in_cols  = c("dep_delay", "arr_delay")
  # out_cols = c("max_dep_delay", "max_arr_delay")
  # flights[, c(out_cols) := lapply(.SD, max), by = month, .SDcols = in_cols]
  
  
  
  countcols <- c('pop', 'mins', ejscreen::names.d.subgroups.count) # examples 
  popmeancols <- c(names.e, names.ej) # we want the raw scores only for EJ and E, or pct only for demog.
  calculatedcols <- names.d # use formulas for these
  
  # COUNTS OVERALL
  results_overall <- sites2bgs_overall[ , .(countcols = lapply(.SD, FUN = function(x) sum(x * bgwt, na.rm=TRUE))), .SDcols = countcols] 
  
  # COUNTS BY SITE
  results_bysite <- sites2bgs_overall[ , .(countcols = lapply(.SD, FUN = function(x) sum(x * bgwt, na.rm=TRUE))), by = siteid, .SDcols = countcols] 
  
  
  # POPMEAN OVERALL
  results_overall[ ,     .(popmeancols := lapply(.SD, FUN = function(x) FUN = weighted.mean(x, w = bgwt * pop, na.rm = TRUE))), .SDcols = popmeancols  ]
  
  # POPMEAN BY SITE
  results_bysite[  ,     .(popmeancols := lapply(.SD, FUN = function(x) weighted.mean(x, w = bgwt * pop, na.rm = TRUE))), by = siteid, .SDcols = popmeancols]
  
  

  warning('work in progress stops here')  # 3############   code below is older 
  
  results <- list(results_overall = results_overall, results_bysite = results_bysite)
  
  
  # 
  # # filter out any rows with missing values
  # facilities <- facilities[!is.na(facilities$LONG) & !is.na(facilities$LAT),]
  # 
  
  ########### Get blockdata for the nearby blocks as listed in facilityblocks ###########
  # 
  # bdata <- data.table::as.data.table(blockdata[POP100 != 0 & Census2010Totalpop != 0, .(blockid, BLOCKGROUPFIPS, STUSAB, STATE, COUNTY, TRACT, BLKGRP, BLOCK, POP100, HU100, Census2010Totalpop)])
  # data.table::setkey(bdata, "blockid")
  # data.table::setkey(facilityblocks, "blockid")
  # extendedfacilityblocks <- merge(facilityblocks, bdata)
  # 
  # ########### Get blockgroupstats (envt or demog indicators) for the nearby blocks  ###########
  # blockgroupstats <- as.data.table(blockgroupstats)
  # data.table::setkey(extendedfacilityblocks, "BLOCKGROUPFIPS")
  # data.table::setkey(blockgroupstats, "BLOCKGROUPFIPS")
  # extendedfacilityblocks_ext <- merge(extendedfacilityblocks, blockgroupstats)
  
  
  ########### Create locations lookup   ###########
  # 
  # #through nearest block
  # nearestlocationdata <- data.table::as.data.table(extendedfacilityblocks_ext[,.(blockid, distance, siteid)])
  # data.table::setkey(nearestlocationdata, "siteid", "distance", "blockid")
  # uniquelocations <- data.table::as.data.table(unique(nearestlocationdata, by = c("ID")))
  # 
  # uniquelocationdata <- merge(uniquelocations, extendedfacilityblocks_ext, by = c("ID", "blockid"))
  # aux_locations <- data.table::as.data.table(uniquelocationdata[,.(ID, blockid, distance.x, BLOCKGROUPFIPS, STUSAB = STUSAB, STATE = statename, COUNTY, TRACT, BLKGRP, BLOCK, REGION)])
  
  # through shapefile
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
  # #just take those complete
  # prime_locations <- prime_locations[!is.na(STUSAB),]
  # 
  # data.table::setkey(incompletes, "ID")
  # data.table::setkey(aux_locations, "ID")
  # aux_locations <- merge(incompletes, aux_locations, all.x = TRUE, all.y = FALSE)
  # 
  # locations <- rbind(prime_locations, aux_locations)
  
  ########### Create scoringweight = fraction of whole Blockgroup pop that is in a given block ###########
  
  # We want the fraction of total blockgroup pop to know how much weight to give each block in aggregating scores in a buffer
  # and this scoringweight is correctly calculated here as comparing apples to apples, namely it is the
  # block pop from Decennial census divided by total blockgroup pop according to that same Decennial census.
  # (i.e., It is NOT the block's Decennial Census pop as fraction of ACS blockgroup pop used in EJSCREEN)
  # 
  # extendedfacilityblocks_ext[ , "scoringweight"] <- extendedfacilityblocks_ext$pop / extendedfacilityblocks_ext$Census2010Totalpop
  # data.table::setkey(extendedfacilityblocks_ext, "ID")
  
  ########### Prepare to add outputs to global env  ###########
  # 
  #  THIS FUNCTION USES env HERE AS A WAY TO PUT VARIOUS VARIABLES INTO THE GLOBAL ENVIRONMENT SO THAT
  #  THEY ARE AVAILABLE LATER FROM WITHIN ALL FUNCTIONS WITHOUT HAVING TO RETURN THAT DATA AS OUTPUT OF THIS FUNCTION, or having to later pass it to EACH FUNCTION THAT NEEDS IT:
  #
  # env <- globalenv() # The global environment .GlobalEnv, more often known as the user's workspace, is the first item on the search path. It can also be accessed by globalenv(). On the search path, each item's enclosure is the next item.
  # 
  #env$debug <- extendedfacilityblocks_ext
  
  ###################################################
  
  
  
  
  
  
  
  
  ###################################################
  #do post processing here REMEMBER NAMES HAVE POSSIBLY CHANGED
  # 
  # #create demographic index
  # result[, "VSI.eo"] <- (result$pctmin + result$pctlowinc ) /2
  # # #create supplemental demographic index
  # # result[, "VSI.svi6"] <- (result$pctmin + result$pctlowinc + result$pctlths + result$pctlingiso + result$pctunder5 + result$pctover64 ) / 6
  # #create EJ index traffic
  # result[, "inedx_EJ_Traffic"] <- result$traffic.score * result$POP100 * (result$VSI.eo - National_Demographic_Index)
  # #create EJ index pm
  # result[, "inedx_EJ_Lead"] <- result$pctpre1960 * result$POP100 * (result$VSI.eo - National_Demographic_Index)
  # #create EJ index PM
  # result[, "inedx_EJ_PM"] <- result$pm * result$POP100 * (result$VSI.eo - National_Demographic_Index)
  # #create EJ index Ozone
  # result[, "inedx_EJ_Ozone"] <- result$o3 * result$POP100 * (result$VSI.eo - National_Demographic_Index)
  # #create EJ index Cancer
  # result[, "inedx_EJ_Cancer"] <- result$cancer * result$POP100 * (result$VSI.eo - National_Demographic_Index)
  # #create EJ index dpm
  # result[, "inedx_EJ_DPM"] <- result$dpm * result$POP100 * (result$VSI.eo - National_Demographic_Index)
  # #create EJ index resp
  # result[, "inedx_EJ_Resp"] <- result$resp * result$POP100 * (result$VSI.eo - National_Demographic_Index)
  # # ***
  # #create EJ index neuro
  # result[, "inedx_EJ_Neuro"] <- result$neuro * result$POP100 * (result$VSI.eo - National_Demographic_Index)
  # #create EJ index proximity.tsdf
  # result[, "inedx_EJ_proximity.tsdf"] <- result$proximity.tsdf * result$POP100 * (result$VSI.eo - National_Demographic_Index)
  # #create EJ index proximity.rmp
  # result[, "inedx_EJ_proximity.rmp"] <- result$proximity.rmp * result$POP100 * (result$VSI.eo - National_Demographic_Index)
  # #create EJ index proximity.npl
  # result[, "inedx_EJ_proximity.npl"] <- result$proximity.npl * result$POP100 * (result$VSI.eo - National_Demographic_Index)
  # #create EJ index proximity.npdes
  # result[, "inedx_EJ_proximity.npdes"] <- result$proximity.npdes * result$POP100 * (result$VSI.eo - National_Demographic_Index)
  
  
  # 
  # #merge geographic info
  # data.table::setkey(result, "ID")
  # data.table::setkey(locations, "ID")
  # 
  # result <- merge(result, locations)
  
  ###################################################
  
  # #do lookups
  # lres <- data.table::as.data.table(lookup.pctile.US(result)) # ************
  # result <- cbind(result, lres)
  # 
  # #now the same with states
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
  
  
  # 
  # #merge facility lat long *************
  # data.table::setkey(facilities, "ID")
  # data.table::setkey(result, "ID")
  # result <- merge(facilities, result, by="ID", all.x = TRUE)
  
  return(result)
}
