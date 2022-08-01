#'  old code to create a look up table to find percentile that a raw score is at within given US State
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
lookup.pctile.State <- function(data, thisregion, ...) {
  lookup.pctile.US(data = data, thisregion = thisregion, geolevel = 'state', ...)
}

# old version copied most of US code
#
# lookup.pctile.State <- function(data, thisstate) {
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
#                1,1,1,1,
#                1,1,1,1,
#                1,0.01,0.01,0.01,
#                0.01,0.01,0.01)
#
#   fields_outputnames <- c("S_D_INDEX_PER","S_E_NPDES_PER","S_E_TSDF_PER","S_E_RMP_PER",
#                           "S_E_NPL_PER","S_E_LEAD_PER","S_E_TRAFFIC_PER","S_E_RESP_PER",
#                           "S_E_NEURO_PER","S_E_CANCER_PER","S_E_DIESEL_PER","S_E_O3_PER",
#                           "S_E_PM25_PER","S_D_MINOR_PER","S_D_INCOME_PER","S_D_LESSHS_PER",
#                           "S_D_LING_PER","S_D_UNDER5_PER","S_D_OVER64_PER")
#
#   fields_outputnames_means <- c("S_D_INDEX","S_E_NPDES","S_E_TSDF","S_E_RMP",
#                                 "S_E_NPL","S_E_LEAD","S_E_TRAFFIC","S_E_RESP",
#                                 "S_E_NEURO","S_E_CANCER","S_E_DIESEL","S_E_O3",
#                                 "S_E_PM25","S_D_MINOR","S_D_INCOME","S_D_LESSHS",
#                                 "S_D_LING","S_D_UNDER5","S_D_OVER64")
#
#   factors_means <- c(100,1,1,1,
#                      1,1,1,1,
#                      1,1,1,1,
#                      1,100,100,100,
#                      100,100,100)
#
#   #prepare data
#   # REGION is recycled to hold STATE!
#   lookup <- as.data.frame( statestats[(statestats$PCTILE!="mean") & (statestats$PCTILE!="std.dev") & (statestats$REGION==thisstate), ])
#   lookup$PCTILE <- as.numeric(as.character(lookup$PCTILE))
#   lookup <- lookup[order(lookup$PCTILE,lookup$MINORPCT),]
#   lookup_mean <- as.data.frame( statestats[(statestats$PCTILE=="mean" & (statestats$REGION==thisstate)),])
#
#   #lookup percentiles
#   state_subres <- list()
#   listindex <- 1
#   for(field in fields2process){
#     colindex <- which(names(data)==field)
#     myvector <- round(factors[[listindex]] * data[[colindex]],6)
#     percentile_indices <- findInterval(myvector, lookup[ , lookupfields[[listindex]]])
#     percentile_indices[percentile_indices==0]  <- 1
#     state_subres[[listindex]] <-  as.numeric(lookup$PCTILE[ percentile_indices ])
#     #if ((field=="o3") & (thisstate=="KS")){
#     #  stop("does not compute")
#     #}
#
#     listindex <- listindex + 1
#   }
#
#   #merge together
#   #if ( length(subres[[1]])!=length(state_subres[[2]])){
#   #  stop("WOW")
#   #}
#   result_pct <-data.table::as.data.table(state_subres)
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
