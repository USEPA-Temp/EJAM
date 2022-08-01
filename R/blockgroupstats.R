#' @name blockgroupstats
#' @docType data
#' @title EJSCREEN demographic and enviromental indicators for Census block groups
#'
#' @description 
#' This is essentially the EJSCREEN dataset, plus more demographic subgroups.  
#'   It has demographic and environmental data from EJSCREEN.
#'   
#' @details 
#'   As of 4/2022 it is the EJScreen 2.0 version of data, which used ACS 2015-2019.
#'   EJScreen 2.0 was released 2/18/2022 (raw data download avail 2/22/2022).
#'   
#'   NOTE: It also has the race/ethnic subgroups that add up to minority or people of color, 
#'   while EJSCREEN as of July 2022 (version 2.0) did not have those variables for standard reports.
#'   
#'   **** PUERTO RICO IS NOT INCLUDED IN THE VERSION HERE AS OF JULY 2022.
#'   
#'   Those demographic variables were not yet calculated for Puerto Rico, so 
#'   US totals would include PR for pop but not for hisp, for example.
#'   
#'   Each year this could be created as for the latest version.
#'   It is also available in a similar form via the ejscreen package on github, 
#'   but there are differences in which columns are kept.
#'   
#'   It is a data.table of US Census blockgroups (not blocks). Approx 220,333 rows, approx 100 columns.
#'   See \url{https:\\www.epa.gov\ejscreen}
#'   
#'   column names include bgfips, pop, pctlowinc, pcthisp, etc.
#'   
#'   See notes on cleaning up and changing the dataset starting from ejscreen::bg21 or bg21plus
#'   
#'   see source code in EJAM::create_blockgroupstats
#'   
NULL
