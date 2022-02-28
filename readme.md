Package: bufferfast
Type: Package
Title: Very Fast Buffering - Aggregates Stats for All Blocks Within X
        Distance of Each Site
Version: 0.1.0
Author: Original version of key code was by Andreas Maier, Abt Associates March 2016, extensively modified by Mark Corrales 2021
Maintainer: The package maintainer <corrales.mark@epa.gov>
Description: Uses quad trees, data.table, parallel processing - for very fast distance calculations
    Uses Census blocks and finds which ones are within specified distance of a facility or site at given lat/lon, 
    for each of a number of facilities or sites.
Requires: data.table, foreach, blockdata, frsdata
Imports: DBI, RMySQL, SearchTrees, doSNOW, funprog, sp, readr
Depends: R (>= 3.5.0)
License: MIT
Encoding: UTF-8
LazyData: true
RoxygenNote: 7.1.2
NeedsCompilation: no
Packaged: 2021-10-04 15:04:17 UTC; markcorrales
