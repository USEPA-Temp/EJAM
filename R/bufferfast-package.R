#' bufferfast package
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
#' # Key Datasets:
#'
#'   ## NOTE: THESE NEED TO BE UPDATED TO 2020/2021 AND MAYBE RENAMED to make clear the vintage
#'   and RECODE FUNCTIONS TO USE LATEST DATASETS. --
#'   work in progess...
#'   blockgroupstats.rdata is now the 2020 version, but
#'   usastats and other lookups ARE NOT UPDATED TO 2020V YET,
#'   etc.
#'
#'   * [blockgroupstats]  data.table (such as EJSCREEN demographic and environmental data by blockgroup - EJSCREEN 2020 version was about 100MB as .rdata)
#'
#'   * blockdata::blockdata        data.table (Census 2010 version was 335 MB as .rdata, 6.2 million rows; Census 2020 would be 8.2 million) with these and other columns: BLOCKID, BLOCKGROUPFIPS, STUSAB, STATE, COUNTY, TRACT, BLKGRP, BLOCK, POP100, HU100, Census2010Totalpop
#'   * blockdata::blockquadtree
#'   * blockdata::quaddata         data.table (Census 2010 version was 140 MB as .rdata, 6.2 million rows; Census 2020 would be 8.2 million)
#'
#'   * frsdata::frs  data.table (The 2021 Facility Registry System version was 98 MB as .rdata, >1 million rows, EPA-regulated facilities) with lat/lon location and other information)
#'   * frsdata::facilities  data.table (The 2016 Facility Registry System version was 98 MB as .rdata, >1 million rows, EPA-regulated facilities) with lat/lon location and other information)
#'   * [points100example] Random test datapoints
#'   * [points1000example] Random test datapoints
#'
#'   * [NAICS]   data.table of industrial codes and names of those sectors or industries, for selecting sites by NAICS
#'   * [usastats] and [statestats]  data.table lookup of 100 percentiles and means (for each indicator in blockgroupstats) in each zone (us, region, or state)
#'   * [stateregions]  data.table lookup of EPA REGION given the ST (state code like AK)
#'   * [statesshp]     shapefile of state boundries to determine what state a point is in
#'
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
