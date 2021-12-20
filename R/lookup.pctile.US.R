#' look up table to find percentile that a raw score is at within USA
#'
#'   Still needs to have in global envt statestats, regionstats, or usastats depending on geolevel -
#'   Can recode to make those data() in this package.
#'
#' @param data indicators data.table?
#' @param thisregion default is USA
#' @param geolevel default is usa, can be used to get state or region percentiles
#' @param fields2process names of columns to use in data
#' @param lookupfields corresponding version of those colnames as used in the lookup table
#' @param fields_outputnames colnames for percentiles in outputs
#' @param fields_outputnames_means colnames for means in outputs
#' @param factors vector of scaling values to convert to correct units like 1, 0.01, etc. (see source code)
#'
#' @export
#'
#' @details CURRENTLY HARD CODED TO USE THESE SPECIFIC FIELDS - NEEDS TO BE MADE GENERIC.
#'   This should be about the same as ejanalysis::lookup.pctile()
#'   using lookup tables from something like ejscreen::ejscreen.lookuptables()
#'   See user ejanalysis, ejscreen and ejanalysis packages on GitHub
#'
#' @return data.table of 100 percentiles and mean (in rows) for each indicator (in columns)
lookup.pctile.US <- function(data, thisregion = 'USA', geolevel = 'usa', fields2process = NULL, lookupfields = NULL, fields_outputnames = NULL, fields_outputnames_means = NULL, factors = NULL) {

  # geolevel = 'state'; fields2process = NULL; lookupfields = NULL; fields_outputnames = NULL; fields_outputnames_means = NULL; factors = NULL
  # CURRENTLY HARD CODED TO USE THESE SPECIFIC FIELDS - NEEDS TO BE MADE GENERIC
  # COULD work from

  fields2process <- c("VSI.eo",
                      "proximity.npdes","proximity.tsdf","proximity.rmp","proximity.npl",
                      "pctpre1960","traffic.score","resp",
                      #"neuro",
                      "cancer","dpm","o3","pm",
                      "pctmin","pctlowinc",
                      "pctlths",
                      "pctlingiso","pctunder5","pctover64")
  fields2process <- c(
    fields2process,
    c('inedx_EJ_Traffic',
      'inedx_EJ_Lead',
      'inedx_EJ_PM', 'inedx_EJ_Ozone',
      'inedx_EJ_Cancer', 'inedx_EJ_DPM', 'inedx_EJ_Resp',
      #'inedx_EJ_Neuro',
      'inedx_EJ_proximity.tsdf', 'inedx_EJ_proximity.rmp', 'inedx_EJ_proximity.npl', 'inedx_EJ_proximity.npdes')
  )

  lookupfields <- c("VULEOPCT",
                    "PWDIS","PTSDF","PRMP","PNPL",
                    "PRE1960PCT","PTRAF","RESP",
                    #"NEURO",
                    "CANCER","DSLPM","OZONE","PM25",
                    "MINORPCT","LOWINCPCT","LESSHSPCT",
                    "LINGISOPCT","UNDER5PCT","OVER64PCT")
  lookupfields <- c(
    lookupfields,
    c(
      "D_PTRAF_2",
      "D_LDPNT_2",
      "D_PM25_2", "D_OZONE_2",
      "D_CANCR_2", "D_DSLPM_2", "D_RESP_2",
      #"D_NEURO_2",
      "D_PTSDF_2", "D_PRMP_2", "D_PNPL_2", "D_PWDIS_2"
    )
    #aka, 'inedx_EJ_Traffic', 'inedx_EJ_Lead', 'inedx_EJ_PM', 'inedx_EJ_Ozone', 'inedx_EJ_Cancer', 'inedx_EJ_DPM', 'inedx_EJ_Resp', 'inedx_EJ_Neuro', 'inedx_EJ_proximity.tsdf', 'inedx_EJ_proximity.rmp', 'inedx_EJ_proximity.npl', 'inedx_EJ_proximity.npdes'
  )

  factors <- c(0.01,1,1,1,
               1,1,1,1,
               #1,
               1,1,1,
               1,0.01,0.01,0.01,
               0.01,0.01,0.01)
  factors <- c(
    factors,
    c(rep(1, times = 11)) # leaves out neuro
  )

  fields_outputnames <- c("N_D_INDEX_PER","N_E_NPDES_PER","N_E_TSDF_PER","N_E_RMP_PER",
                          "N_E_NPL_PER","N_E_LEAD_PER","N_E_TRAFFIC_PER","N_E_RESP_PER",
                          #"N_E_NEURO_PER",
                          "N_E_CANCER_PER","N_E_DIESEL_PER","N_E_O3_PER",
                          "N_E_PM25_PER","N_D_MINOR_PER","N_D_INCOME_PER","N_D_LESSHS_PER",
                          "N_D_LING_PER","N_D_UNDER5_PER","N_D_OVER64_PER")
  fields_outputnames <- c(
    fields_outputnames,
    c(
      "N_P_TRAFFIC",
      "N_P_LEAD",
      "N_P_PM25", "N_P_O3",
      "N_P_CANCER", "N_P_DIESEL", "N_P_RESP",
      #"N_P_NEURO",
      "N_P_TSDF", "N_P_RMP", "N_P_NPL", "N_P_NPDES"
    )
  )

  fields_outputnames_means <- c("N_D_INDEX","N_E_NPDES","N_E_TSDF","N_E_RMP",
                                "N_E_NPL","N_E_LEAD","N_E_TRAFFIC","N_E_RESP",
                                #"N_E_NEURO",
                                "N_E_CANCER","N_E_DIESEL","N_E_O3",
                                "N_E_PM25","N_D_MINOR","N_D_INCOME","N_D_LESSHS",
                                "N_D_LING","N_D_UNDER5","N_D_OVER64")
  # *** MEAN VALUE FOR RAW EJ INDEX IS NEVER SHOWN, SO DO NOT NEED IT
  # HOWEVER, the code assumes same length to
  #  factors_means (was 18) and lookup_mean? and lookupfields (29)
  fields_outputnames_means <- c(
    fields_outputnames_means,
    paste(geolevel, 'noejmean', 1:11, sep = '')
  )

  if (geolevel == 'region') {
    fields_outputnames <- gsub('N_', 'R_', fields_outputnames)
    fields_outputnames_means <- gsub('N_', 'R_', fields_outputnames_means)
  }
  if (geolevel == 'state') {
    fields_outputnames <- gsub('N_', 'S_', fields_outputnames)
    fields_outputnames_means <- gsub('N_', 'S_', fields_outputnames_means)
  }

  factors_means <- c(100,1,1,1,
                     1,1,1,1,
                     #1,
                     1,1,1,
                     1,100,100,100,
                     100,100,100)
  # *** MEAN VALUE FOR RAW EJ INDEX IS NEVER SHOWN, SO DO NOT NEED IT
  # HOWEVER, the code assumes same length to
  #  factors_means (was 18) and lookup_mean? and lookupfields (29)
  factors_means <- c(
    factors_means,
    rep(1, 11)
  )

  #prepare data
  geostats <- switch(geolevel,
                     state = statestats,
                     region = regionstats,
                     usa = usastats)
  lookup_mean <- as.data.frame( geostats[(geostats$PCTILE == "mean" & (geostats$REGION == thisregion)), ])
  lookup <- as.data.frame( geostats[(geostats$PCTILE != "mean") & (geostats$PCTILE != "std.dev") & (geostats$REGION == thisregion), ])

  lookup$PCTILE <- as.numeric(as.character(lookup$PCTILE))
  lookup <- lookup[order(lookup$PCTILE, lookup$MINORPCT), ]

  #lookup percentiles
  us_subres <- list()
  listindex <- 1
  for (field in fields2process) {
    colindex <- which(names(data) == field)
    myvector <- round(factors[[listindex]] * data[[colindex]], 6)
    if(!is.na(myvector)) {
          us_subres[[listindex]] <-  as.numeric(lookup$PCTILE[ findInterval(myvector, lookup[ , lookupfields[[listindex]]]) ])
    } else
    {
      us_subres[[listindex]] <- NA
    }

    listindex <- listindex + 1
    if ((field == "o3")) {
      #stop("does not compute") # ???
    }
  }

  #merge together
  if ( length(unique(length(us_subres))) != 1) {
    stop("WOW")
  }
  result_pct <-data.table::as.data.table(us_subres)

  #results are v1 v2 v3...
  names(result_pct) <- fields_outputnames

  #lookup means
  nRowsDf <- nrow(result_pct) # number of records
  means_subres <- list()
  listindex <- 1
  for (field in lookupfields) {
    colindex <- which(names(lookup_mean) == field)
    val <-  factors_means[[listindex]] * as.numeric(lookup_mean[ 1, colindex ])
    means_subres[[listindex]] <- rep(c(val), each = nRowsDf)
    listindex <- listindex + 1
  }

  # get rid of those extra fields that were filler just to make lengths of lists of fields match even though US/Reg/State mean EJ index value is not needed
  means_subres <- means_subres[ !grepl('noejmean', fields_outputnames_means) ]

  result_means <-data.table::as.data.table(means_subres)
  fields_outputnames_means <- fields_outputnames_means[ !grepl('noejmean', fields_outputnames_means) ]
  names(result_means) <- fields_outputnames_means

  result <- cbind(result_pct, result_means)

  return(result)
}

# lookup.pctile.Region <- function(data, thisregion) {
#
#   fields2process <- c("VSI.eo","proximity.npdes","proximity.tsdf","proximity.rmp",
#                       "proximity.npl","pctpre1960","traffic.score","resp",
#                     #  "neuro",
#                     "cancer","dpm","o3",
#                       "pm","pctmin","pctlowinc","pctlths",
#                       "pctlingiso","pctunder5","pctover64")
#
#   lookupfields <- c("VULEOPCT","PWDIS","PTSDF","PRMP",
#                     "PNPL","PRE1960PCT","PTRAF","RESP",
#                     #"NEURO",
#                   "CANCER","DSLPM","OZONE",
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
#   lookup <- as.data.frame( regionstats[(regionstats$PCTILE != "mean") & (regionstats$PCTILE != "std.dev") & (regionstats$REGION == thisregion), ])
#   lookup$PCTILE <- as.numeric(as.character(lookup$PCTILE))
#   lookup <- lookup[order(lookup$PCTILE,lookup$MINORPCT),]
#   lookup_mean <- as.data.frame( regionstats[(regionstats$PCTILE == "mean" & (regionstats$REGION == thisregion)),])
#
#   #lookup percentiles
#   region_subres <- list()
#   listindex <- 1
#   for (field in fields2process) {
#     colindex <- which(names(data) == field)
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
#     # REGION is recycled to hold STATE!
#   lookup <- as.data.frame( statestats[(statestats$PCTILE != "mean") & (statestats$PCTILE != "std.dev") & (statestats$REGION == thisstate), ])
#   lookup$PCTILE <- as.numeric(as.character(lookup$PCTILE))
#   lookup <- lookup[order(lookup$PCTILE,lookup$MINORPCT),]
#   lookup_mean <- as.data.frame( statestats[(statestats$PCTILE == "mean" & (statestats$REGION == thisstate)),])
#
#   #lookup percentiles
#   state_subres <- list()
#   listindex <- 1
#   for (field in fields2process) {
#     colindex <- which(names(data) == field)
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
#   for (field in lookupfields) {
#     colindex <- which(names(lookup_mean) == field)
#     val <-  factors_means[[listindex]] * as.numeric(lookup_mean[ 1, colindex ])
#     means_subres[[listindex]] <- rep(c(val), each = nRowsDf)
#     listindex <- listindex + 1
#   }
#   result_means <-data.table::as.data.table(means_subres)
#   names(result_means) <- fields_outputnames_means
#
#   result <- cbind(result_pct,result_means)
#
#   return(result)
# }
