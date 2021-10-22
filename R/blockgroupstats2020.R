#' @name blockgroupstats2020
#' @docType data
#' @title 2020 version of EJSCREEN indicators for Census block groups
#'
#' This is essentially the EJSCREEN dataset, but could be replaced with something using different indicators.
#'   The 2020 version of EJSCREEN was released mid 2020 and uses
#'   American Community Survey (ACS) data for 2014-2018.
#'   It has demographic and environmental data.
#'   Each year this could be created as for the latest version.
#'   It is also available in a similar form via the ejscreen package on github, ejscreen::bg20 for the 2020 version, but that file does not have
#'   all the percentile text fields and others.
#'
#'   It is a data.table of US Census blockgroups (not blocks). Approx 220,333 rows, 141 columns.
#'   See \url{https:\\www.epa.gov\ejscreen}
#'   names(blockgroupstats2020) include OBJECTID, BLOCKGROUPFIPS, pop, etc.
NULL
