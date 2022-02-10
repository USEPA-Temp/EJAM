#' EJAM package
#'
#' @md
#'
#' @description
#'
#' This package provides functions and data for very fast proximity analysis
#' for a large number of locations, or "buffers", summarizing conditions at each location.
#' It summarizes conditions as the conditions for the average resident in the buffer at that location.
#' A buffer or location here is defined as the area within a specified distance of a specified site.
#' A site of facility in this package is defined as a single geographic point (by latitude and longitude).
#'
#' The functions here are somewhat generalized, but the specific datasets included here enable an
#' environmental justice (EJ) proximity analysis of US EPA's EJSCREEN data, including
#' environmental indicators (e.g., local traffic score, or estimated PM2.5 concentration)
#' and demographic indicators (e.g., percent low-income),
#' for the populations estimated to live within a specified distance (e.g., 3 miles) from
#' one or more sites, typically EPA-regulated facilities.
#' This means the tools can provide the same information that an EJSCREEN standard report provides,
#' but for a large number of reports (one for each site).
#'
#' @details
#' # Specifying sites like EPA-regulated facilities:
#'
#' The sites or facilities can be specified by uploaded lat/lon point data,
#' or by selecting (from a list) one or more types of facilities, as defined by NAICS,
#' or by uploading EPA Facility Registry System (FRS) ID numbers, for example.
#' The NAICS are 2-digit to 6-digit codes that specify industrial sectors or types of facilities, such as
#' 325 - Chemical Manufacturing, or 325211 - Plastics Material and Resin Manufacturing.
#'
#' @details
#' 
#' ---------------------------------------------------------------------------------------------------------------------------------------------------
#' New versions of data.tables (user data or loaded data files) will be as follows:-------------------------------------------------
#' ---------------------------------------------------------------------------------------------------------------------------------------------------
#' 
#' Shiny app module(s) at front end lets user specify places/facilities/buffers by NAICS code, Facility IDs, or locations. 
#'   returns data.table sitepoints with fields siteid, lat, lon:
#' 
#'  1- interface so user can select NAICS or enter naics 
#'   returns a list of naics_selected
#' 
#'  naics2latlon <- function(naics_selected) 
#'   uses frs_naics_2016.rdata or  frs_naics_2020.rdata
#'   returns data.table sitepoints[ , .(siteid, lat, lon)]  (here, siteid is just 1:n)
#'   the 2016 version was facdata.rdata renamed  frs_naics_2016.rdata with PROGRAM, PROGRAM_ID, REGISTRY_ID, NAICS, LAT, LONG
#'   
#'  2- interface so user can upload FRS REGISTRY_ID csv file, 
#'    returns list of frsids values (the REGISTRY_ID in FRS) 
#' 
#'  frsid2latlon <- function(frsids)
#'    uses frsdata::frsid2latlon.rdata
#'    returns data.table sitepoints[ , .(siteid, lat, lon)]
#' 
#'  3- interface so user can upload latitude longitude siteid (and optionally other columns like sitename),
#'    returns data.table sitepoints[ , .(siteid, lat, lon)]  (here, siteid is just 1:n)
#' 
#' 
#' 
#' 
#' census2020download::blocks2020_id2fips[ , .(blockid, blockfips)]     (old one was blockdata::blocks2010) 
#' census2020download::blocks2020points[ , .(blockid, lat, lon, ???, ???)]
#' quadtree files for 2020 are not yet created?  (they were in blockdata::quaddata, blockdata::blockquadtree)
#' 
#'   calculated intermediate result:
#' blocks2sites[ , .(siteid, blockid, distance)]
#' 
#' INPUT FROM USER 
#' 
#' sitepoints[ , .(siteid, lat, lon)]
#' 
#' ** sitepoints   A user-specified data table with maybe 100 to 1k to 10k+ points, with cols called
#' 
#'   - siteid
#'   - lat
#'   - lon
#'   
#' BUILT-IN DATABASES FOR DISTANCE ANALYSIS ------------------
#' 
#' ** blocks2020_id2fips.rda   A loaded data.table with 6-8m rows, 2 columns: (There are 8.2 million blocks, but only 5.8 million have population > 0.)
#' 
#'   - blockid
#'   - blockfips (just in case need get back to full FIPS, and to find parent bg of block when precreating blockwts)
#' 
#' 
#' ** blocks2020points.rda    A loaded data.table with with 6-8m rows, 3 to 10 cols called
#' 
#'  - blockid
#'  - lat
#'  - lon
#'  - any quadtree cols needed? so it can be used in getrelevant...
#'  
#' **  other quadtree files?? need any separate quadtree files made from that? tried to make localtree and save as data() but did not work?
#'   
#'   
#'   -------then those are used in getrelevant with user's sitepoints to create user's blocks2sites:
#'   
#' CREATED INTERMEDIATE RESULTS FOR USER
#' 
#' **  blocks2sites   Created by getrelevant... and passed to  doaggregate()  
#' 
#'      This is a data table with maybe 100k to 1m rows (assume 1k blocks within 3 miles of each site, or 100 blocks within 1 mile),
#'      
#'   -  siteid    (site with circular buffer to group by)
#'   -  blockid   (and blockfips?)  for join to blockwts
#'   -  distance  (in miles, from block to site) (0 or irrelevant for noncircular buffers, 
#'          since a block is only in this table if in one or more buffers, 
#'          unless analysis is for residents within x miles of the edges of some shapes, like facility boundaries)
#'   
#'   
#' ** blocks_unique  ?!!! Not sure this has to be created at all - mostly wastes space to make a copy of nearly every row in blocks2sites
#'        To avoid doublecounting, when getting overall stats for avg person near any 1 or more sites.
#'        Created by some trivial function like dissolveblocks() or as part of the doaggregate() function? 
#'        
#'    - blockid  Just the unique blockid values from blocks2sites
#'    - sitecount  Number of sites near this block, which is just how many duplicates there were of this blockid
#'    - distance  The minimum (worst-case) distance among all the duplicates for this blockid (if any dupes)
#'         Probably do not need to save additional details about range of distances or which site
#'   
#'   
#' **  bg2sites   Create (but not make a copy?) an aggregated version of blocks2sites, 
#'          that has  bg2sites <- blocks2sites[ , bgwt = sum(blockwt), by=bgid ]
#'            then possibly do rm(blocks2sites); gc() 
#'            unless we want to preserve the full bg2sites info for other detailed analysis of distribution of distances in each demog group.
#'   
#'   
#'  BUILT-IN DATABASES FOR WEIGHTED MEANS OF INDICATORS  ------- other precalculated datasets:
#'   
#' ** blockwts2020.rda   Passed to doaggregate()  a data.table with 6-8m rows, 3 cols called 
#' 
#'   - blockid? (integer key  for join to blocks2sites)
#'   - bgfips   integer key, used to aggregate  sum(blockwt), by=bgid  and also for join to bgstats
#'   - bgid     integer key   
#'   - blockwt  (fraction of parent blockgroup decennial pop that is in this one block)
#' 
#' 
#' ** bgstats2020.rda   a data.table with 220k rows, and about 200 cols: 
#' 
#'   - bgid    integer key for join to 
#'   - bgfips  for convenience? not sure it is needed
#'   - pop   
#'   - many columns of blockgroup indicator scores (approx 200 columns) 
#'        e.g., counts like count of lowincome people, percents like pct lowincome, etc.
#' 
#' 
#' OUTPUT RESULTS CREATED FOR USER:
#' 
#' results_by_site   results for individual sites (buffers) - a data.table of results, one row per siteid, one column per indicator 
#' results_overall   one row data.table, like results_by_site, but just one row with aggregated results for all unique residents. 
#' 
#' + maybe some extra columns with summary stats across indicators, as separate summary stats beyond what EJSCREEN report does?, one row per site and for overall. 
#' + maybe some extra rows with summary stats about the distribution across people and facilities, one column per indicator. 
#'                                                
#' ---------------------------------------------------------------------------------------------------------------------------------------------------
#' #--------------------- Key Datasets:   --- older versions of names etc. -----------------------------------
#' 
#'   ## NOTE: THESE NEED TO BE UPDATED TO 2020/2021 AND MAYBE RENAMED to make clear the vintage
#'   and RECODE FUNCTIONS TO USE LATEST DATASETS. --
#'   work in progress...
#'   bgstats or blockgroupstats.rdata is now the 2020 version, but
#'   usastats and other lookups ARE NOT UPDATED TO 2020V YET,
#'   etc.
#'
#'   * [blockgroupstats]  data.table (such as EJSCREEN demographic and environmental data by blockgroup - EJSCREEN 2020 version was about 100MB as .rdata)
#'   * bgstats2020  will be the new version...
#'
#'   * blockdata::blockdata        data.table (Census 2010 version was 335 MB as .rdata, 6.2 million rows; Census 2020 would be 8.2 million)
#'         with these and other columns: BLOCKID, BLOCKGROUPFIPS, STUSAB, STATE, COUNTY, TRACT, BLKGRP, BLOCK, POP100, HU100, Census2010Totalpop
#'   * blockdata::blockquadtree
#'   * blockdata::quaddata         data.table (Census 2010 version was 140 MB as .rdata, 6.2 million rows; Census 2020 would be 8.2 million)
#'
#'   * Will clarify facilities vs sitepoints. 
#'   * frsdata::frs  not really needed for EJAM... data.table (The 2021 Facility Registry System version was 98 MB as .rdata, >1 million rows, EPA-regulated facilities) with lat/lon location and other information)
#'   * frsdata::frs_naics_2016  data.table needed to get lat lon by naics, from the 2016 version, one row per naics per registry id
#'   * frsdata::facilities  data.table (The 2016 Facility Registry System version was 98 MB as .rdata, >1 million rows, EPA-regulated facilities) with lat/lon location and other information)
#'   * sitepoints  data.table input by user: siteid, lat, lon (centers of circular buffers)
#'   * [points100example] Random test datapoints
#'   * [points1000example] Random test datapoints
#'
#'   * [NAICS]   data.table of industrial codes and names of those sectors or industries, for selecting sites by NAICS
#'   * [usastats] and [statestats]  data.table lookup of 100 percentiles and means (for each indicator in blockgroupstats) in each zone (us, region, or state)
#'   * [stateregions]  data.table lookup of EPA REGION given the ST (state code like AK)
#'   * [statesshp]     shapefile of state boundries to determine what state a point is in
#' ---------------------------------------------------------------------------------------------------------------------------------------------------
#' @details
#' # Key Functions:
#'
#' * [doaggregate()] Summarize the indicators from blockgroupstats within each buffer weighted using blockdata
#'     (for average resident within specified distance of site or facility)
#'    NOTE: THAT FUNCTION NEEDS TO BE MADE GENERIC TO USE ANY INDICATORS
#'
#' * [getrelevantCensusBlocksviaQuadTree_Clustered()]  Very fast method to buffer, identifying which blocks are within specified distance of site or facility
#'
#' @details
#' # Buffering method:
#'
#' The buffering is currently done in a way that includes all Census blocks (2010 blocks, as of 8/2021)
#' whose "internal point" (a lat/lon provided by Census) is within the specified distance of the facility point.
#'
#' The summary or aggregation or "rollup" within the buffer is done by calculating the
#' population-weighted average block group score among all the people residing in the buffer.
#'
#' Since the blockgroup population counts are from American Community Survey (ACS) estimates,
#' but the block population counts are from a decennial census, the totals for a blockgroup differ.
#' The amount each partial blockgroup contributes to the buffer's overall score is based on
#' the estimated number of residents from that blockgroup who are in the buffer.
#' This is based on the fraction of the blockgroup population that is estimated to be in the buffer,
#' and that fraction is calculated as the fraction of the blockgroup's decennial census block population
#' that is in the census blocks inside the buffer.
#'
#' A given block is considered entirely inside or entirely outside the buffer,
#' and those are used to more accurately estimate what fraction of a given block group's
#' population is inside the buffer. This is more accurate and faster than areal apportionment of block groups.
#' Census blocks are generally so small relative to typical buffers that this is very accurate -
#' it is least accurate if a very small buffer distance is specified
#' in an extremely low density rural area where a block can be geographically large.
#' Although it is rarely if ever a significant issue (for reasonable, useful buffer sizes),
#' an even more accurate approach in those cases might be either areal apportionment of blocks,
#' which is very slow and assumes residents are evenly spread out across the full block's area,
#' or else an approach that uses higher resolution estimates of residential locations than even
#' the Decennial census blocks can provide, such as a dasymetric map approach.
#'
#'
#' @keywords internal
"_PACKAGE"
# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
