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
  
  
  # HARDCODED blockgroup dataset and variable names, FOR NOW:  ####
  
  blockgroupstats <- ejscreen::bg21plus; blockgroupstats$bgfips <- blockgroupstats$FIPS
  setDT(blockgroupstats, key = c('bgfips'))
  
  # from EJSCREEN dataset, names as in ejscreen package:
  # **** but we probably treat pctpre1960 as pop wtd mean like other Evars?
  
  if (is.null(countcols)) {
    # # note that names.d.count was not yet defined in ejscreen pkg, and would lack denominators if only based on pctxyz
    # names.d.count <- union( gsub('pct','', grep(pattern = 'pct', ejscreen::names.d, value=TRUE)),
    #  c('unemployed', 'unemployedbase'))
    # names.d.count <- gsub('^min$', 'mins', names.d.count) # since singular was used in pctmin but plural for count is mins
    # countcols <- unique(c('pop', 'nonmins', names.d.count,
    #                "povknownratio",  "age25up", "hhlds",  'pre1960', 'builtunits',
    #                ejscreen::names.d.subgroups.count)) 
    
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
    
  }  
  if (is.null(calculatedcols)) {
    # calculatedcols <- c(ejscreen::names.d, ejscreen::names.d.subgroups, 'flagged') # use formulas for these
    #  or to avoid depending on ejscreen package, 
    #  dput(c(ejscreen::names.d, ejscreen::names.d.subgroups, 'flagged')) # but make sure pctunemployed got added
    
    calculatedcols <- c(
      "VSI.eo", "pctmin", "pctlowinc", "pctlths", "pctlingiso", "pctunder5", "pctover64", 'pctunemployed',
      "pctnhwa", "pcthisp", "pctnhba", "pctnhaa", "pctnhaiana", "pctnhnhpia", "pctnhotheralone", "pctnhmulti", 
      "flagged"
      )
    
    # These must be calculated after aggregating count variables and using those at siteid level. 
    # e.g. Use ejscreen::ejscreenformulas$formula to calculate these.
  }
  if (is.null(popmeancols)) {
    # popmeancols <- c(ejscreen::names.e, ejscreen::names.ej)
    # or to avoid depending on ejscreen package, 
    # dput(c(ejscreen::names.e, ejscreen::names.ej) )
    
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
    
    # ** CHECK THIS:  EJScreen treats pctpre1960 as if can do popwtd avg, right? Technically pctpre1960 should use ejscreenformulas... ratio of sums of counts pre1960 and denom builtunits  
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
  
  # get weights for nearby blocks ####
  # sites2blocks <- merge(sites2blocks, blockwts, by='blockid', all.x	=TRUE, all.y=FALSE) # incomparables=NA
  sites2blocks <- blockwts[sites2blocks, .(siteid,blockid,distance,blockwt,bgfips), on='blockid']
  
  # optional: Calc # of sites nearby each block: #### 
  # How many of these sites are near this resident? this bg? avg resident overall? 
  sites2blocks[, sitecount_near_block := .N, by=blockid] # (must use the table with duplicate blocks, not unique only)
  
  # *** Get overall stats counting each block once #### 
  # and thus each person only once even if near 2+ sites, 
  # For each overall resident (blockid) near 1 or more sites, 
  # find and save the info for just the closest single siteid.
  # ***    Just for now, The simplistic way to drop duplicate blocks (even if other columns are not duplicates), 
  #    which are residents near 2 or more sites, to avoid double-counting them, is this:
  sites2blocks_overall <- unique(sites2blocks, by="blockid") 
  # But note that would just pick the first instance found of a given blockid, regardless of which siteid that was for,
  # so it retains whatever distance happened to be the one found first, and same for all site characteristics.
  # *** We actually instead want to save the shortest distance for that blockid, as the worst case proximity
  # to be able to have summary stats by group of distance to the closest site, not to a random site among those nearby. 
  # What is the efficient way to find that?
  
  
  
  # Calc avg person's proximity (block-level), by bg: #### 
  #  censuspop-wtd mean of block-specific distances, for each bg 
  sites2blocks_overall[, bg_fraction_in_buffer_overall := sum(blockwt), by=bgfips]  
  sites2blocks[, bg_fraction_in_buffer_bysite := sum(blockwt), by=c('bgfips', 'siteid')]
  
  # was going to try to do join of weights and the aggregation by blockid all in this one step? but not tested 
  # and may need to do intermed step 1st, where 
  # sites2bg <- blockwts[sites2blocks, .(siteid, bgfips, distance, bgwt = sum(blockwt, na.rm=TRUE)), on = 'blockid', by =.(siteid, bgfips)] 
  
  ## why do sum(blockwt) by bgfips  here AGAIN, if already did it above?
  rm(blockwts) ; gc()  # drop 6m row block table to save RAM # does not seem to be loaded to do that??
  
  
  # rollup as blockgroups - Aggregate blocks into blockgroups, per siteid ***  #######################################
  
  # Calc bgwt, the fraction of each (parent)blockgroup's censuspop that is in buffer #### 
  sites2bgs_overall <- sites2blocks_overall[ , .(bgwt = sum(blockwt)), by=bgfips ]
  sites2bgs_bysite  <- sites2blocks[ , .(bgwt = sum(blockwt, na.rm = TRUE)), by=.(siteid, bgfips)]
  
  # optional: Calc maybe # of unique sites nearby each blockgroup #### 
  sites2bgs_bysite[ , sitecount_near_bg := length(unique(siteid)), by=bgfips] 
  
  # how many blockgroups here were found near 1, 2, or 3 sites? e.g., 6k bg were near only 1/100 sites tested, 619 near 2, 76 bg had 3 of the 100 sites nearby.
  # table(table(sites2bgs_bysite$bgfips))
  
  # COULD TRY TO REMOVE sites2blocks and sites2blocks_overall NOW TO FREE UP RAM, BUT THAT IS slow!:
  # if (!testing) {rm(sites2blocks); gc()} # unless need save that to analyze distance distribution 
  
  
  #  Maybe want some extra summary stats across people and sites (about the distribution), one column per indicator. 
  # *****  BUT MOST OF THE INTERESTING STATS LIKE MEDIAN PERSON'S SCORE, OR WORST BLOCKGROUP, 
  #  HAVE TO BE CALCULATED FROM BG DATA BEFORE WE AGGREGATE WITHIN EACH SITE (BUFFER)... 
  #  Same for sites: worst site as measured by highest nearby blockgroup-level %poor needs raw bg data before summarized by siteid.
  
  
  
  ##################################################### #
  # 2) *** JOIN *** the midsized intermed table to blockgroupstats ... sites2bgs_overall ??  ################################
  ##################################################### #
  
  # countcols  # like population count, add up within a buffer
  # popmeancols  # we want average persons raw score,  for Environmental and EJ indexes
  # calculatedcols  # use formulas for these, like  sum of counts of lowincome divided by sum of counts of those with known poverty ratio (universe)
  
  # DO JOIN  OF **blockgroupstats**   200 columns, on bgid , 
  # and not sure I can calculate results at same time, since this kind of join is getting a subset of blockgroupstats but grouping by sites2bgs_bysite$siteid  and 
  # maybe cannot use blockgroupstats[sites2bgs_bysite,    by=.(siteid)    since siteid is in sites2bgs_bysite not in blockgroupstats table. 
  # So, first join blockgroupstats necessary variables to the shorter sites2bgs_bysite:   
  # about 29 bg did not match on bgfips due to changes in codes as used by bg21 vs blockpoints dataset used in EJAM as of May 2022.
  
  sites2bgs_plusblockgroupdate_bysite  <- merge(sites2bgs_bysite,  
                                                blockgroupstats[ , c('bgfips', ..countcols, ..popmeancols, ..calculatedcols)], 
                                                all.x = TRUE, all.y=FALSE, by='bgfips')  
  sites2bgs_plusblockgroupdate_overall <- merge(sites2bgs_overall, 
                                                blockgroupstats[ , c('bgfips', ..countcols, ..popmeancols, ..calculatedcols)], 
                                                all.x = TRUE, all.y=FALSE, by='bgfips') 
  
  
  ##################################################### #
  # CALCULATE TOTALS FOR COUNT VARIABLES WITHIN EACH SITE OR BUFFER & OVERALL (POPULATION, 
  # AND ALSO SUBGROUPS IF WANT TO 
  #  USE FORMULAS TO GET EXACT %D AT EACH SITE AS SUM OF NUMERATORS / SUM OF DENOMINATORS)
  ##################################################### #
  
  # COUNTS OVERALL  ####
  results_overall <- sites2bgs_plusblockgroupdate_overall[ ,  lapply(.SD, FUN = function(x) round(sum(x * bgwt, na.rm=TRUE), 1) ), .SDcols = countcols ]
  
  # cbind(sum = prettyNum(results_overall,big.mark = ','))
  # versus if you did not remove duplicate places/people:
  # sites2bgs_plusblockgroupdate_bysite[ ,  .(sums = lapply(.SD, FUN = function(x) sum(x * bgwt, na.rm=TRUE))), .SDcols = countcols][1,]
  # 1: 9,978,123
  # but sum(outapi_3mile_100sites$pop[outapi_3mile_100sites$statename != 'PUERTO RICO' ])
  # [1] 10,022,946
  
  
  # COUNTS BY SITE  ####
  results_bysite <- sites2bgs_plusblockgroupdate_bysite[ ,  lapply(.SD, FUN = function(x) round(sum(x * bgwt, na.rm=TRUE), 1) ), by = .(siteid), .SDcols = countcols ]
  # results_bysite[1:100,1:8]
  
  ##################################################### #
  # CALCULATE POPULATION WEIGHTED MEAN FOR SOME VARIABLES ( ENVT, EJ index. ####  
  # ... AND MAYBE ALL THE DEMOG TOO???)
  ##################################################### #
  
  # POP wtd MEAN OVERALL ####
  results_overall_popmeans <- sites2bgs_plusblockgroupdate_overall[ ,  lapply(.SD, FUN = function(x) weighted.mean(x, w = bgwt * pop, na.rm = TRUE)), .SDcols = popmeancols ]
  results_overall <- cbind(results_overall, results_overall_popmeans)
  # cbind(sum = prettyNum(results_overall, big.mark = ','))
  
  # POP wtd MEAN BY SITE ####
  results_bysite_popmeans <- sites2bgs_plusblockgroupdate_bysite[ ,  lapply(.SD, FUN = function(x) weighted.mean(x, w = bgwt * pop, na.rm = TRUE)), by = .(siteid), .SDcols = popmeancols ]
  results_bysite <- merge(results_bysite, results_bysite_popmeans)
  # (results_bysite)
  
  
  save.image('~/R/mypackages/EJAM/inst/doagg2 so far just before calculated vars made.rda')
  
  
  ##################################################### #
  # and/or if some variables have to be calculated using formulas, 
  # can do that using the list of formulas and this function: ... 
  ##################################################### #
  
  #   CALCULATE INDICATORS USING FORMULAS, BASED ON THE ROLLED UP COUNTS #### 
  # this was meant to handle multiple columns (formula for each new one) for many rows (and here in buffer results, one site is a row, not one blockgroup) 
  
  # "nonmins <- nhwa"
  # "mins <- pop - nhwa" 
  results_overall[ , `:=`(
    pctover64 = ifelse( pop==0, 0, over64 / pop) ,
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
  pctover64 = ifelse( pop==0, 0, over64 / pop) ,
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
  # VSI.eo.US = sum(mins) / sum(pop)  +  sum(lowinc) / sum(povknownratio) ) / 2, 
  # VNI.eo = VSI.eo * pop, 
  # VSI.eo = (pctlowinc + pctmin) / 2, 
  #  or is it treated as envt var and just pop mean?
  
  # to be replaced with data available to this package
  # myformulas = ejscreen::ejscreenformulas
  
  # one row per buffer/site?  
  # results_bysite_formulas_done <- ejscreen::ejscreen.acs.calc(bg = results_bysite, keep.old = 'all', keep.new = 'all', formulas = myformulas)
  
  # just one row?
  
  # results_overall_formulas_done  <- ejscreen::ejscreen.acs.calc(bg = results_overall, keep.old = 'all', keep.new = 'all', formulas = myformulas)


  # cbind(prettyNum( (results_bysite[1,]),big.mark = ','))
  # t(usastats[usastats$PCTILE == 'mean', ])
   
  
  save.image('~/R/mypackages/EJAM/inst/doagg2 so far just AFTER calculated vars made.rda')
  
  
  ##################################################### #
  # FIND PERCENTILES THOSE RAW SCORES REPRESENT  ####
  #  VIA  lookup tables of US/State  percentiles
  ##################################################### #
  

  
  # Use the dataset called EJAM::usastats as the lookup table for USA percentiles and mean. 
  # but update/ fix it so it uses right variable names, etc., or replace with the one from ejscreen pkg
  
  
  
  # ejanalysis::lookup.pctile(results_bysite[, varsneedpctiles[i]], varname.in.lookup.table = varsneedpctiles[i], lookup = lookup.pctile.US) 
  # ejanalysis::lookup.pctile(c(1000, 3000), varname.in.lookup.table = 'traffic.score',
  #   lookup = lookupStates19, zone = 'NY')
  
  # lookup.pctile.US

  
  
  # pctilevars <- c(ejscreen::names.e, ejscreen::names.d, ejscreen::names.d.subgroups, ejscreen::names.ej)
  # pctile.cols <- data.frame(matrix(nrow = 2, ncol = length(pctilevars))); colnames(pctile.cols) <- pctilevars
  # # for (myvar in pctilevars) {
    # pctile.cols[ , myvar] <- EJAM::lookup.pctile.US(results_bysite[ , myvar], ..... 
  # }
  #  need to do state and region here too *****

  
  
  warning('work in progress stops here')  # ############   code below is older 
  
  # RETURN THE RESULTS ####
  
  results <- list(results_overall = results_overall, results_bysite = results_bysite)
  
  
  return(results)
  
  
  ##################################################### #
  # EJSCREENbatch code for comparison #### 
  # took weighted mean of data within each buffer (shape_ID) for these indicators:
  ##################################################### #
  # 
  # Extract key variables, take ***pop-weighted*** average
  # df.var.wm <-list_data %>%
  #   as.data.frame() %>%
  #   dplyr::select(shape_ID, ACSTOTPOP, PM25, OZONE, DSLPM, CANCER,
  #                 RESP, PTRAF, PNPL, PRMP, PRE1960PCT, PTSDF, PWDIS,
  #                 VULEOPCT, MINORPCT, LOWINCPCT, UNDER5PCT,LESSHSPCT,
  #                 OVER64PCT, LINGISOPCT, med_inc, frac_white, frac_black,
  #                 frac_amerind, frac_asian, frac_pacisl, frac_hisp, 
  #                 frac_pov50, frac_pov99) %>%
  #   dplyr::group_by(shape_ID) %>%
  #   dplyr::summarize(across(PM25:frac_pov99, ~weighted.mean(., w = ACSTOTPOP, na.rm = T)))
  # # 
  # 
  # # ...
  # 
  #   EJSCREENbatch code uses ecdf function here on the entire ejscreen dataset each time??, 
  #  to estimate US AND THEN STATE percentiles
  # 
  # #Rejoin, then calculate nat'l percentiles
  # df.var.wm <- df.var.wm %>%
  #   dplyr::left_join(df.var.state, by = 'shape_ID') %>%
  #   dplyr::mutate(across(PM25:LINGISOPCT,
  #                        list(~round(ecdf(ejscreen_data %>%
  #                                           as.data.frame() %>%
  #                                           dplyr::select(cur_column()) %>%
  #                                           unlist() %>%
  #                                           as.numeric())(.)*100
  #                                    ,0)),
  #                        # list(~ntile(., 100)),
  #                        .names="P_{.col}_US")) %>%
  #   dplyr::mutate(across(med_inc:frac_pov99,
  #                        list(~round(ecdf(acs_data %>%
  #                                           as.data.frame() %>%
  #                                           dplyr::select(cur_column()) %>%
  #                                           unlist() %>%
  #                                           as.numeric())(.)*100
  #                                    ,0)),
  #                        .names="P_{.col}_US"))
  # 
  # #Calculate state percentiles
  # states <- na.omit(unique(df.var.wm$ST_ABBREV))
  # temp_state <- lapply(states, function(x){
  #   ti2 <- df.var.wm %>%
  #     dplyr::filter(ST_ABBREV==x) %>%
  #     dplyr::filter(!is.na(shape_ID))  %>%
  #     dplyr::mutate(across(PM25:LINGISOPCT,
  #                          list(~round(ecdf(na.omit(ejscreen_data %>%
  #                                                     as.data.frame() %>%
  #                                                     dplyr::filter(ST_ABBREV==x) %>%
  #                                                     dplyr::select(cur_column()) %>%
  #                                                     unlist() %>%
  #                                                     as.numeric()))(.)*100
  #                                      ,0)),
  #                          .names="P_{.col}_state")) %>%
  #     dplyr::mutate(across(med_inc:frac_pov99,
  #                          list(~round(ecdf(na.omit(acs_data %>%
  #                                                     as.data.frame() %>%
  #                                                     dplyr::filter(state==x) %>%
  #                                                     dplyr::select(cur_column()) %>%
  #                                                     unlist() %>%
  #                                                     as.numeric()))(.)*100
  #                                      ,0)),
  #                          .names="P_{.col}_state"))
  # })
  
  # ow code to name all the percentile columns
  #
  # df.var.wm <- data.table::rbindlist(temp_state) %>%
  #   dplyr::rename_at(vars(rename_cols), ~paste0('P_',all_of(rename_cols),'_raw')) %>%
  #   tidyr::pivot_longer(cols=starts_with("P_"),
  #                       names_to="variable",
  #                       values_to = "value") %>%
  #   dplyr::mutate(variable=stringi::stri_replace_last_fixed(variable,'_','.')) %>%
  #   tidyr::separate(variable, into=c("variable","geography"), sep="\\.",extra="merge", fill = "left")   %>%
  #   tidyr::pivot_wider(names_from = c(variable)) %>%
  #   dplyr::rename(Lead         = P_PRE1960PCT,
  #                 'Diesel PM'         = P_DSLPM,
  #                 'Air, Cancer'       = P_CANCER,
  #                 'Resp. Hazard'      = P_RESP,
  #                 'Traffic'           = P_PTRAF,
  #                 'WW Discharge'      = P_PWDIS,
  #                 'NPL'               = P_PNPL,
  #                 'RMP Facility'      = P_PRMP,
  #                 'TSD Facility'      = P_PTSDF,
  #                 'Ozone'             = P_OZONE,
  #                 'PM'                = P_PM25,
  #                 'Demo. Index'       = P_VULEOPCT,
  #                 Minority            = P_MINORPCT,
  #                 'Low Income'        = P_LOWINCPCT,
  #                 'Less HS Educ'      = P_LESSHSPCT,
  #                 'Ling. Isol.'       = P_LINGISOPCT,
  #                 'Age Under 5'       = P_UNDER5PCT,
  #                 'Age Over 64'       = P_OVER64PCT,
  #                 'Median Income'     = P_med_inc,
  #                 'Caucasian (%)'     = P_frac_white,
  #                 'Black (%)'         = P_frac_black,
  #                 'Amer. Ind. (%)'    = P_frac_amerind,
  #                 'Asian (%)'         = P_frac_asian,
  #                 'Pac. Isl (%)'      = P_frac_pacisl,
  #                 'Hispanic (%)'      = P_frac_hisp,
  #                 '<50% P.L. (%)'     = P_frac_pov50,
  #                 '<100% P.L. (%)'    = P_frac_pov99) %>%
  #   dplyr::select(-ST_ABBREV)
  # 
  # OW code to sum count of population at each site?
  # 
  # # Sum of population w/in 5miles
  # df.pop.sum <- list_data %>%
  #   dplyr::select(ACSTOTPOP, shape_ID) %>%
  #   dplyr::rename(`Pop. Count` = ACSTOTPOP) %>%
  #   dplyr::group_by(shape_ID) %>%
  #   dplyr::summarize_at(vars(`Pop. Count`),funs(sum))
  # 
  # Need lat/lon, (previously: URL to the facility's DFR)
  # df.latlon <- facil_data %>%
  #   dplyr::select(shape_ID, geometry) %>%
  #   sf::st_transform(crs = 4326)
  # 
  # Merge all together
  # together.sf <- dplyr::inner_join(df.var.wm, df.pop.sum, by = "shape_ID") %>%
  #   dplyr::inner_join(df.latlon, by = 'shape_ID') %>%
  #   dplyr::relocate(shape_ID, `Pop. Count`,
  #                   `Low Income`, `Minority`, `Less HS Educ`, `Ling. Isol.`,
  #                   `Age Under 5`, `Age Over 64`, `Air, Cancer`, `Diesel PM`,
  #                   Lead, Ozone, PM, NPL, `RMP Facility`, Traffic, `TSD Facility`,
  #                   `WW Discharge`, `Resp. Hazard` )
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
  
}
