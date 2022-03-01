#' @name blockgroupstats
#' @docType data
#' @title EJSCREEN demographic and enviromental indicators for Census block groups
#'
#' @description 
#' This is essentially the EJSCREEN dataset, but 
#'   could be replaced with something using different indicators.
#'   It has demographic and environmental data from EJSCREEN.
#'   
#' @details 
#'   NOTE: It also has the race/ethnic subgroups that add up to minority or people of color, 
#'   while EJSCREEN as of Feb. 2022 version 2.0 did not have those variables for standard reports.
#'   However those variables are not filled in for Puerto Rico, so 
#'   US totals will include PR for pop but not for hisp, for example.
#'   
#'   EJSCREEN 2.0 was released 2/18/2022 (raw data download avail 2/22/2022).
#'   
#'   The 2020 version of EJSCREEN was released in 2020/2021 and used through Jan 2022. 
#'   It is based on American Community Survey (ACS) data for 2014-2018.
#'   Each year this could be created as for the latest version.
#'   It is also available in a similar form via the ejscreen package on github, ejscreen::bg20 for the 2020 version, 
#'   but there are differences in which columns are kept.
#'   
#'   It is a data.table of US Census blockgroups (not blocks). Approx 220,333 rows, approx 100 columns.
#'   See \url{https:\\www.epa.gov\ejscreen}
#'   
#'   column names include bgfips, pop, pctlowinc, pcthisp, etc.
#'   
#'   See notes on cleaning up and changing the dataset starting from ejscreen::bg20 or bg21
#'   
#' @seealso \link{create_blockgroupstats}
#'   
NULL
