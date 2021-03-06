% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doaggregate.R
\name{doaggregate}
\alias{doaggregate}
\title{Summarize indicators in each buffer (given the blocks in each buffer and indicators for each block)}
\usage{
doaggregate(
  sites2blocks,
  countcols = NULL,
  popmeancols = NULL,
  calculatedcols = NULL,
  testing = FALSE,
  ...
)
}
\arguments{
\item{sites2blocks}{data.table of distances in miles between all sites (facilities) and 
nearby Census block internal points, with columns siteid, blockid, distance,
created by getrelevant... function.}

\item{...}{more to pass}
}
\description{
This updated 2022 code takes a set of facilities and the set of blocks that are near each,
  (as identified previously, in other code that has identified which blocks are nearby)
  and combines those with indicator scores for block groups.

  It aggregates the blockgroup scores to create a summary of each indicator,
   in each buffer (i.e., near each facility):
     -Sums of counts, such as for population or number of households or Hispanics
     -Calculated variables for the buffer via formulas based on aggregated counts, 
       such as percent low income.
     -Population-Weighted means for EJ Indexes or Environmental indicators.
}
\details{
\preformatted{

  requires the following as data lazy loaded for example from blockdata package

   blockdata  data.table (was 335 MB as .rdata) with these columns: 
   blockfips, bgfips, STUSAB, blockpop2010, bgpop2010, blockwt, 
   # INTPTLAT, INTPTLON, BLOCK_LAT_RAD, BLOCK_LONG_RAD  BLOCK_X   BLOCK_Y  BLOCK_Z ID GRID_X GRID_Y GRID_Z 
   # old names: blockid, BLOCKGROUPFIPS, STUSAB, STATE, COUNTY, TRACT, BLKGRP, BLOCK, POP100, HU100, Census2010Totalpop
   
   NOTE EJScreen APIs provide (slow limited number per query) access to the block weights table they use, here: 
   https://ejscreen.epa.gov/arcgis/rest/services/ejscreen/ejquery/MapServer/73 
   
   blockgroupstats - A data.table (such as EJSCREEN demographic and environmental data by blockgroup?)
   statesshp   (a shapefile of state boundries to determine what state a point is in)
   stateregions  data.table lookup of EPA REGION given the ST (state code like AK)

   only these are passed to the function rather than being already in the global envt:
     facilityblocks - passed to the function
     facilities

   Check which table has which column names, including columns with
      distance, statename?,  REGION etc.

   Check if it includes Puerto Rico ( that was dropped?? - but it is in blocks, and in blockgroupstats for most fields though not race/ethnic subgroups)
 }
}
