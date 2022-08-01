#' old code to create a look up table to find percentile that a raw score is at  within EPA Region
#'
#' @details CURRENTLY HARD CODED TO USE THESE SPECIFIC FIELDS - NEEDS TO BE MADE GENERIC.
#'   This should be about the same as ejanalysis::lookup.pctile()
#'   using lookup tables from something like ejscreen::ejscreen.lookuptables()
#'   See user ejanalysis, ejscreen and ejanalysis packages on GitHub
#'
#' @param data passed to \link{lookup.pctile.US}
#' @param thisregion passed to \link{lookup.pctile.US}
#' @param ... passed to \link{lookup.pctile.US}
#'
#' @return data.table of 100 percentiles and mean (in rows) for each indicator (in columns)
#' @export
#'
lookup.pctile.Region <- function(data, thisregion, ...) {
  lookup.pctile.US(data = data, thisregion = thisregion, geolevel = 'region', ...)
}

# OLD VERSION DUPLICATED CODE FROM lookup.pctile.US()
#
# lookup.pctile.Region <- function(data, thisregion) {
#
#   fields2process <- c("VSI.eo","proximity.npdes","proximity.tsdf","proximity.rmp",
#                       "proximity.npl","pctpre1960","traffic.score","resp",
#                       "neuro","cancer","dpm","o3",
#                       "pm","pctmin","pctlowinc","pctlths",
#                       "pctlingiso","pctunder5","pctover64")
#
#   lookupfields <- c("VULEOPCT","PWDIS","PTSDF","PRMP",
#                     "PNPL","PRE1960PCT","PTRAF","RESP",
#                     "NEURO","CANCER","DSLPM","OZONE",
#                     "PM25","MINORPCT","LOWINCPCT","LESSHSPCT",
#                     "LINGISOPCT","UNDER5PCT","OVER64PCT")
#
#
#   factors <- c(0.01,1,1,1,
#                1,1,1,1
#                ,1,1,1,1,
#                1,0.01,0.01,0.01,
#                0.01,0.01,0.01)
#
#   fields_outputnames <- c("R_D_INDEX_PER","R_E_NPDES_PER","R_E_TSDF_PER","R_E_RMP_PER",
#                           "R_E_NPL_PER","R_E_LEAD_PER","R_E_TRAFFIC_PER","R_E_RESP_PER",
#                           "R_E_NEURO_PER","R_E_CANCER_PER","R_E_DIESEL_PER","R_E_O3_PER",
#                           "R_E_PM25_PER","R_D_MINOR_PER","R_D_INCOME_PER","R_D_LESSHS_PER",
#                           "R_D_LING_PER","R_D_UNDER5_PER","R_D_OVER64_PER")
#
#   fields_outputnames_means <- c("R_D_INDEX","R_E_NPDES","R_E_TSDF","R_E_RMP",
#                                 "R_E_NPL","R_E_LEAD","R_E_TRAFFIC","R_E_RESP",
#                                 "R_E_NEURO","R_E_CANCER","R_E_DIESEL","R_E_O3",
#                                 "R_E_PM25","R_D_MINOR","R_D_INCOME","R_D_LESSHS",
#                                 "R_D_LING","R_D_UNDER5","R_D_OVER64")
#
#   factors_means <- c(100,1,1,1,
#                      1,1,1,1,
#                      1,1,1,1,
#                      1,100,100,100,
#                      100,100,100)
#
#   #prepare data
#   lookup <- as.data.frame( regionstats[(regionstats$PCTILE!="mean") & (regionstats$PCTILE!="std.dev") & (regionstats$REGION==thisregion), ])
#   lookup$PCTILE <- as.numeric(as.character(lookup$PCTILE))
#   lookup <- lookup[order(lookup$PCTILE,lookup$MINORPCT),]
#   lookup_mean <- as.data.frame( regionstats[(regionstats$PCTILE=="mean" & (regionstats$REGION==thisregion)),])
#
#   #lookup percentiles
#   region_subres <- list()
#   listindex <- 1
#   for(field in fields2process){
#     colindex <- which(names(data)==field)
#     myvector <- round(factors[[listindex]] * data[[colindex]],6)
#     region_subres[[listindex]] <-  as.numeric(lookup$PCTILE[ findInterval(myvector, lookup[ , lookupfields[[listindex]]]) ])
#     listindex <- listindex + 1
#   }
#
#   #merge together
#   result_pct <-data.table::as.data.table(region_subres)
#
#   #results are v1 v2 v3...
#   names(result_pct) <- fields_outputnames
#
#
#   #lookup means
#   nRowsDf <- nrow(result_pct) # number of records
#   means_subres <- list()
#   listindex <- 1
#   for(field in lookupfields){
#     colindex <- which(names(lookup_mean)==field)
#     val <-  factors_means[[listindex]] * as.numeric(lookup_mean[ 1, colindex ])
#     means_subres[[listindex]] <- rep(c(val),each=nRowsDf)
#     listindex <- listindex + 1
#   }
#   result_means <-data.table::as.data.table(means_subres)
#   names(result_means) <- fields_outputnames_means
#
#   result <- cbind(result_pct,result_means)
#
#   return(result)
# }


