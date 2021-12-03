#' Summarize indicators in each buffer (given the blocks in each buffer and indicators for each block)
#'
#' @description This takes a set of facilities and the set of blocks that are near each,
#'   (as identified previously, in other code that has identified which blocks are nearby)
#'   and combines those with indicator scores for block groups.
#'
#'   It aggregates the scores as population weighted means, (check that - all of them?)
#'   creating a summary of each indicator, in each buffer (i.e., near each facility).
#'
#' @details
#'  \preformatted{
#'
#'  **** As currently written, this is hardcoded for specific indicators as the columns, indicators like pctlowinc.
#'  **** It should be made generic.
#'
#'  As currently written, the code USES SOME VARIABLES THAT WERE ALREADY PUT INTO THE GLOBAL ENVIRONMENT
#'    SO THAT THEY ARE AVAILABLE FROM WITHIN ALL FUNCTIONS WITHOUT HAVING TO PASS THE DATA TO EACH FUNCTION THAT NEEDS IT.
#'   It sets,  env <- globalenv()
#'   ### The global environment .GlobalEnv, more often known as the user's workspace, is the first item on the search path.
#'   It can also be accessed by globalenv().
#'
#'   So, as currently written, this would require the following already be in the global environment,
#'  (until it may be recoded to have those passed to this function):
#'
#'    blockdata  data.table (335 MB as .rdata) with these columns: BLOCKID, BLOCKGROUPFIPS, STUSAB, STATE, COUNTY, TRACT, BLKGRP, BLOCK, POP100, HU100, Census2010Totalpop
#'    blockgroupstats or blockgroupstats2020 - A data.table (such as EJSCREEN demographic and environmental data by blockgroup?)
#'    statesshp   (a shapefile of state boundries to determine what state a point is in)
#'    stateregions  data.table lookup of EPA REGION given the ST (state code like AK)
#'
#'    So far, only these are passed to the function rather than being already in the global envt:
#'
#'      facilityblocks - passed to the function
#'      facilities
#'
#'    Check which table has which column names, including columns with
#'       Distance, statename?,  REGION etc.
#'
#'    Check if it includes Puerto Rico (no, that was dropped)
#'  }
#'
#' @param facilities data.table of facility locations
#' @param facilityblocks data.table of blocks inside buffer around any given facility
#'
#' @export
#'
doaggregate <- function(facilities, facilityblocks){

  # ******* this should be rewritten to not be hardcoded for specific columns, indicators like pctlowinc   ****************

  # Also, as written, it assumes blockdata is in global environment already.
  # blockdata for Census 2010 was 335 MB as a .rdata file.

  # filter out any rows with missing values
  facilities <- facilities[!is.na(facilities$LONG) & !is.na(facilities$LAT),]

  ########### Get blockdata for the nearby blocks as listed in facilityblocks ###########

  bdata <- data.table::as.data.table(blockdata[POP100 != 0 & Census2010Totalpop != 0, .(BLOCKID, BLOCKGROUPFIPS, STUSAB, STATE, COUNTY, TRACT, BLKGRP, BLOCK, POP100, HU100, Census2010Totalpop)])
  data.table::setkey(bdata, "BLOCKID")
  data.table::setkey(facilityblocks, "BLOCKID")
  extendedfacilityblocks <- merge(facilityblocks, bdata)

  ########### Get blockgroupstats (envt or demog indicators) for the nearby locks  ###########
  blockgroupstats <- as.data.table(blockgroupstats)
  data.table::setkey(extendedfacilityblocks, "BLOCKGROUPFIPS")
  data.table::setkey(blockgroupstats, "BLOCKGROUPFIPS")
  extendedfacilityblocks_ext <- merge(extendedfacilityblocks, blockgroupstats)


  ########### Create locations lookup   ###########

  #through nearest block
  nearestlocationdata <- data.table::as.data.table(extendedfacilityblocks_ext[,.(BLOCKID, Distance, ID)])
  data.table::setkey(nearestlocationdata, "ID", "Distance", "BLOCKID")
  uniquelocations <- data.table::as.data.table(unique(nearestlocationdata, by = c("ID")))

  uniquelocationdata <- merge(uniquelocations, extendedfacilityblocks_ext, by = c("ID", "BLOCKID"))
  aux_locations <- data.table::as.data.table(uniquelocationdata[,.(ID, BLOCKID, Distance.x, BLOCKGROUPFIPS, STUSAB = STUSAB, STATE = statename, COUNTY, TRACT, BLKGRP, BLOCK, REGION)])

  # through shapefile
  prime_locations <- merge_state_shapefiles(facilities, statesshp)
  prime_locations <- data.table::as.data.table(prime_locations@data)

  #  get state abbreviations from stateregions (but that could be replaced by a function that returns )
  prime_locations <- merge(prime_locations, stateregions, by.x = "STATE", by.y = "STATENAME")
  prime_locations <- prime_locations[,.(ID, STATE, STUSAB = ST)]
  data.table::setkey(prime_locations, "STUSAB")
  data.table::setkey(stateregions, "ST")
  prime_locations <- merge(prime_locations, stateregions, all.x = TRUE, all.y = FALSE, by.x = "STUSAB", by.y = "ST")

  prime_locations <- data.table::as.data.table(prime_locations[,.(ID, BLOCKID = NA, Distance.x = NA, BLOCKGROUPFIPS = NA, STUSAB, STATE, COUNTY = NA, TRACT = NA, BLKGRP = NA, BLOCK = NA, REGION)] )

  incompletes <- data.table::as.data.table(prime_locations[is.na(STUSAB),.(ID)])
  #just take those complete
  prime_locations <- prime_locations[!is.na(STUSAB),]

  data.table::setkey(incompletes, "ID")
  data.table::setkey(aux_locations, "ID")
  aux_locations <- merge(incompletes, aux_locations, all.x = TRUE, all.y = FALSE)

  locations <- rbind(prime_locations, aux_locations)

  ########### Create scoringweight = fraction of whole Blockgroup pop that is in a given block ###########

  # We want the fraction of total blockgroup pop to know how much weight to give each block in aggregating scores in a buffer
  # and this scoringweight is correctly calculated here as comparing apples to apples, namely it is the
  # block pop from Decennial census divided by total blockgroup pop according to that same Decennial census.
  # (i.e., It is NOT the block's Decennial Census pop as fraction of ACS blockgroup pop used in EJSCREEN)
  extendedfacilityblocks_ext[ , "scoringweight"] <- extendedfacilityblocks_ext$pop / extendedfacilityblocks_ext$Census2010Totalpop
  data.table::setkey(extendedfacilityblocks_ext, "ID")

  ########### Prepare to add outputs to global env  ###########

  #  THIS FUNCTION USES env HERE AS A WAY TO PUT VARIOUS VARIABLES INTO THE GLOBAL ENVIRONMENT SO THAT
  #  THEY ARE AVAILABLE LATER FROM WITHIN ALL FUNCTIONS WITHOUT HAVING TO RETURN THAT DATA AS OUTPUT OF THIS FUNCTION, or having to later pass it to EACH FUNCTION THAT NEEDS IT:
  #
  env <- globalenv() # The global environment .GlobalEnv, more often known as the user's workspace, is the first item on the search path. It can also be accessed by globalenv(). On the search path, each item's enclosure is the next item.

  #env$debug <- extendedfacilityblocks_ext

  ###################################################

  #do preprocessing here, field ratios, etc, these may be needed in later steps
  # ******* this should not be hardcoded for specific columns, indicators:   ****************
  pctlowinc <- sum_pctlowinc(extendedfacilityblocks_ext)
  pctlangugage <- sum_pctlanguage(extendedfacilityblocks_ext)

  #do direct sums
  env$direct_sums <- directsums(extendedfacilityblocks_ext)
  data.table::setkey(env$direct_sums, "ID")

  #do pop weighted sums
  env$popweight_sums <- popweightedsums(extendedfacilityblocks_ext)
  data.table::setkey(env$popweight_sums, "ID")
  ###################################################

  #merge
  result <- merge(env$direct_sums, env$popweight_sums)
  rm(direct_sums, envir = env)
  rm(popweight_sums, envir = env)
  gc()
  data.table::setkey(result, "ID")

  data.table::setkey(pctlowinc, "ID")
  result$pctlowinc <- pctlowinc$pctlowinc

   data.table::setkey(pctlangugage, "ID")
  result$pctlingiso <- NA
  tmp <- merge(result, pctlangugage, all.x = TRUE)[,.(ID, pctlingiso.y)]

  data.table::setkey(tmp, "ID")
  result <- merge(result, tmp, all.x = TRUE)
  result$pctlingiso <- result$pctlingiso.y
  result[, pctlingiso.y := NULL]
  # ***********************


  ###################################################

  #do post processing here REMEMBER NAMES HAVE POSSIBLY CHANGED

  #create demographic index
  result[, "VSI.eo"] <- (result$pctmin + result$pctlowinc ) /2

  #create supplemental demographic index
  result[, "VSI.svi6"] <- (result$pctmin + result$pctlowinc + result$pctlths + result$pctlingiso + result$pctunder5 + result$pctover64 ) / 6

  #create EJ index traffic
  result[, "inedx_EJ_Traffic"] <- result$traffic.score * result$POP100 * (result$VSI.eo - National_Demographic_Index)

  #create EJ index pm
  result[, "inedx_EJ_Lead"] <- result$pctpre1960 * result$POP100 * (result$VSI.eo - National_Demographic_Index)

  #create EJ index PM
  result[, "inedx_EJ_PM"] <- result$pm * result$POP100 * (result$VSI.eo - National_Demographic_Index)

  #create EJ index Ozone
  result[, "inedx_EJ_Ozone"] <- result$o3 * result$POP100 * (result$VSI.eo - National_Demographic_Index)

  #create EJ index Cancer
  result[, "inedx_EJ_Cancer"] <- result$cancer * result$POP100 * (result$VSI.eo - National_Demographic_Index)

  #create EJ index dpm
  result[, "inedx_EJ_DPM"] <- result$dpm * result$POP100 * (result$VSI.eo - National_Demographic_Index)

  #create EJ index resp
  result[, "inedx_EJ_Resp"] <- result$resp * result$POP100 * (result$VSI.eo - National_Demographic_Index)

  # ***
  #create EJ index neuro
  result[, "inedx_EJ_Neuro"] <- result$neuro * result$POP100 * (result$VSI.eo - National_Demographic_Index)

  #create EJ index proximity.tsdf
  result[, "inedx_EJ_proximity.tsdf"] <- result$proximity.tsdf * result$POP100 * (result$VSI.eo - National_Demographic_Index)

  #create EJ index proximity.rmp
  result[, "inedx_EJ_proximity.rmp"] <- result$proximity.rmp * result$POP100 * (result$VSI.eo - National_Demographic_Index)

  #create EJ index proximity.npl
  result[, "inedx_EJ_proximity.npl"] <- result$proximity.npl * result$POP100 * (result$VSI.eo - National_Demographic_Index)

  #create EJ index proximity.npdes
  result[, "inedx_EJ_proximity.npdes"] <- result$proximity.npdes * result$POP100 * (result$VSI.eo - National_Demographic_Index)

  #merge geographic info
  data.table::setkey(result, "ID")
  data.table::setkey(locations, "ID")

  locations <- locations[, ID := as.character(ID)]
  result <- merge(result, locations)

  ###################################################

  #do lookups
  lres <- data.table::as.data.table(lookup.pctile.US(result))
  result <- cbind(result, lres)

  # now for each region
  regions <- unique(result$REGION)
  region_result <- list()
  regionindex <- 1
  for (region in regions) {
    partial_region <- result[result$REGION == region,]
    partial_regions_processed <- data.table::as.data.table(lookup.pctile.Region(partial_region, region))
    region_result[[regionindex]] <- data.table::as.data.table(cbind(partial_region, partial_regions_processed))
    regionindex <- regionindex + 1
  }
  result <- data.table::rbindlist(region_result)

  #now the same with states
  states <- unique(result$STUSAB) #recycled in the data
  state_result <- list()
  stateindex <- 1
  for (state in states) {
    partial_state <- result[result$STUSAB == state,]
    partial_states_processed <- data.table::as.data.table(lookup.pctile.State(partial_state, state))
    state_result[[stateindex]] <- data.table::as.data.table(cbind(partial_state, partial_states_processed))
    stateindex <- stateindex + 1
  }
  result <- data.table::rbindlist(state_result)

  #merge facility lat long
  data.table::setkey(facilities, "ID")
  data.table::setkey(result, "ID")
  facilities <- facilities[, ID := as.character(ID)]
  result <- merge(facilities, result, by="ID", all.x = TRUE)

  return(result)
}
