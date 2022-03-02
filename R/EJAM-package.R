#' @docType package
#' @title Environmental Justice (EJ) Analysis Multisite tool
#' @name EJAM
#' @aliases EJAM-package
#'
#' @md
#' 
#' @description
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
#' @details  ## **Key Functions:** ####################################################################
#'
#' * [doaggregate()]* Summarize the indicators from blockgroupstats 
#'     within each buffer weighted using blockwts
#'     (for average resident within specified distance of site or facility)
#'      NOTE: FUNCTION IS BEING REWRITTEN to be generic to use any indicators,
#'      and to use smaller data files, and be faster, etc.
#'      doaggregate2() is that work in progress.
#'
#' * [getrelevantCensusBlocksviaQuadTree()]*  
#'     Very fast method to buffer, identifying which blocks are 
#'     within specified distance of site or facility
#'
#' @details # **SPECIFYING BUFFER SITES/FACILITIES** ####################################################################
#' 
#'   A user can specify locations, via an interface, and that shiny app returns  
#'  *`sitepoints`*, a data.table with fields siteid, lat, lon.
#'   A user-specified table with maybe 100, 1k, 10k+ points (centers of circular buffers)
#'   
#'  One can specify sitepoints, the places to be analyzed (sites or facilities), 
#'  in one of three ways:
#' 
#'    1. NAICS code (selecting from a list, one or more types of facilities, as defined by NAICS). 
#'         The NAICS are 2-digit to 6-digit codes that specify sectors or types of facilities, such as
#'         325 - Chemical Manufacturing, or 325211 - Plastics Material and Resin Manufacturing.
#'    2. Facility IDs - EPA Facility Registry System (FRS) ID numbers
#'    3. uploaded locations as lat/lon points.
#'    
#'  **1. BY INDUSTRIAL SECTOR/ NAICS:  **
#'         Interface lets user select NAICS from pulldown, or type in NAICS 
#'         Interface returns `naics_selected`, a vector of one or more naics codes,
#'         converted to `sitepoints` by naics2latlon().
#' 
#'    **[naics2latlon()](naics_selected)** returns `sitepoints` data.table (note here, siteid is just 1:n)
#'  
#'      Required **frsdata::frs_naics_2016.rdata** **to be updated** 
#'      Need to update FRS data used here regularly, ideally frequently.
#'      The 2016 version was facdata.rdata renamed  frs_naics_2016.rdata 
#'      with columns PROGRAM, PROGRAM_ID, REGISTRY_ID, NAICS, LAT, LONG
#'      data.table needed to get lat lon by naics
#'      Initially from the 2016 version, one row per naics per registry id
#'    * frsdata::frs* is not really needed for EJAM... A data.table with lat/lon location and other information
#'      (The 2021 Facility Registry System version was 98 MB as .rdata, >1 million rows, EPA-regulated facilities)
#'       
#'  **2. BY FACILITY ID:**  
#'         Interface so user can upload FRS REGISTRY_ID csv file, 
#'         Interface returns `frsids`, list of REGISTRY_ID values from FRS
#'         converted to `sitepoints` by frsid2latlon().
#' 
#'    **[frsid2latlon()](frsids)** returns `sitepoints` data.table
#'  
#'         Requires **[frsdata::frsid2latlon].rdata** data.table with cols frsid, lat, lon 
#' 
#'  **3- BY LAT/LON POINT: **
#'         Interface so user can upload latitude longitude siteid (and optionally others like sitename),
#'         Returns `sitepoints[ , .(siteid, lat, lon)]`  data.table (here, siteid is just 1:n)
#' 
#' @details # **BUFFERING, TO FIND site-block DISTANCES**  ####################################################################
#' 
#'   Input: **`sitepoints`** data.table from user picking points
#'   Columns are siteid, lat, lon; maybe 100 to 10k points
#'      
#'   **[getrelevantCensusBlocksviaQuadTree](sitepoints)**
#'        Returns `sites2blocks` 
#'        Requires datasets [quaddata] and [blockquadtree] (may rename)
#'        
#'   **sites2blocks**   Created by [getrelevantCensusBlocksviaQuadTree()] and passed to  [doaggregate()]  
#'      This is a data table with maybe 100k to 1m rows (assume 1k blocks within 3 miles of each site, or 100 blocks within 1 mile),
#'      `sites2blocks[ , .(siteid, blockid, distance or dist)]`
#'    -  siteid    (site with circular buffer to group by)
#'    -  blockid   (and blockfips?)  for join to blockwts
#'    -  distance or dist  (in miles, from block to site) (0 or irrelevant for noncircular buffers, 
#'          since a block is only in this table if in one or more buffers, 
#'          unless analysis is for residents within x miles of the edges of some shapes, like facility boundaries)
#'          
#' @details ## **DATA FILES FOR DISTANCE ANALYSIS** ####################################################################
#' 
#' **[blockdata::quaddata].rda** (may rename as blockpoints)  dataset data.table 
#'    <5.8m rows (>8m if nonpopulated blocks were kept).  120MB file for 2020 Census.
#'     (Census 2010 version was 140 MB as .rdata, 6.2 million rows populated blocks)
#'    - blockid 
#'    - BLOCK_X, BLOCK_Y, BLOCK_Z  (not lat, lon)
#'  
#' **[blockdata::blockquadtree].rda**  (may rename as blocktree) 
#'   Index to quaddata (QuadTree class, via SearchTrees pkg), not a data.table 
#'   
#'  (An older file was called blockdata.rda ... pre-2022 had been used to hold many columns, 
#'       like variables used in doaggregate and nonessential ones too,
#'       BLOCKID, BLOCKGROUPFIPS, STUSAB, STATE, COUNTY, TRACT, BLKGRP, BLOCK, POP100, HU100, Census2010Totalpop)
#'       (Census 2010 version was 335 MB as .rdata, 6.2 million rows; Census 2020 would be 8.2 million)
#' 
#'   -------then those are used in getrelevant with user's sitepoints to create user's sites2blocks:
#'   
#' @details # **SUMMARIZING INDICATORS IN BUFFERS** ####################################################################
#' 
#'       INPUT IS  `sites2blocks`, 
#'       OUTPUT IS results_overall, results_bysite, and maybe other summary stats  ####
#'   
#'   *[doaggregate2()]* <- function(sites2blocks) **
#'       #(in buffer and overall)
#'     
#'   **[blockdata::blockwts].rda**
#'    Required by [doaggregate()]. A data.table of 6-8m rows 
#'   - blockwt  The fraction of parent blockgroup decennial pop that is in this one block
#'   - blockid (integer key  for join to sites2blocks)
#'   - bgfips integer key, for sum(blockwt), by=bgfips, and for join to blockgroupstats
#'   - bgid ?  integer key might add at some point. More efficient than bgfips but bgfips is easier
#' 
#'   **[blockgroupstats].rda** a data.table with 220k rows (blockgroups), and about 200 cols.
#'      Needs to be updated each time EJSCREEN is updated. 
#'      (such as EJSCREEN demographic and environmental data
#'       EJSCREEN 2020 version was about 100MB as .rdata)
#'
#'   * [usastats]* and *[statestats]*  data.table lookup of 100 percentiles and means 
#'       (for each indicator in blockgroupstats) in each zone (us, region, or state).
#'       Need to update each time blockgroupstats is updated.
#'       
#'   * [stateregions]*  data.table lookup of EPA REGION given the ST (state code like AK)
#'   * [statesshp]*   possibly obsolete  shapefile of state boundries to determine what state a point is in
#' 
#'   ** blocks_unique?**  Intermediate result, not saved
#'    - blockid  Just the unique blockid values from sites2blocks
#'    - sitecount  Number of sites near this block, which is just how many duplicates there were of this blockid
#'    - distance  The minimum (worst-case) distance among all the duplicates for this blockid (if any dupes)
#'         Probably do not need to save additional details about range of distances or which site
#'   
#'  ** bg2sites?**  Intermediate result, not saved.  aggregated version of sites2blocks
#'        via  `bg2sites <- sites2blocks[ , bgwt = sum(blockwt), by=bgid ]`
#'            then possibly do rm(sites2blocks); gc() 
#'            unless we want to preserve the full bg2sites info for other detailed 
#'            analysis of distribution of distances in each demog group.
#'            
#' @details ## **OUTPUT RESULTS CREATED FOR USER** ####################################################################
#' 
#' `results_overall`   one row data.table, like results_by_site, but just one row with 
#'     aggregated results for all unique residents. 
#' 
#' `results_by_site`   results for individual sites (buffers) - a data.table of results, 
#'     one row per siteid, one column per indicator
#' 
#'  + maybe want some extra rows with summary stats across people and sites 
#'      (about the distribution), one column per indicator. 
#'      BUT MOST OF THE INTERESTING STATS LIKE MEDIAN PERSON'S SCORE, OR WORST BLOCKGROUP,
#'      HAVE TO BE CALCULATED BEFORE AGGREGATING/ SUMMARIZING BY SITE (BUFFER), FROM RAW BG DATA!
#'      Same for sites: worst site as measured by highest nearby blockgroup-level
#'      %poor needs raw bg data before summarized by siteid.
#' 
#'  + maybe some extra columns with summary stats across indicators, as 
#'      separate summary stats beyond what EJSCREEN report does?, one row per site and for overall. 
#'      
#' @details # OTHER DATA FILES ####################################################################
#'   
#'   * [sites2blocks_example]* sample output of getrelevant...
#'   * [points100example]* Random test datapoints
#'   * [points1000example]* Random test datapoints
#'
#' @details # Buffering method:  ####################################################################
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
