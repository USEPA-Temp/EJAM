#' clean up output of fast batch tool? not used currently
#'
#'  # this function is not currently used (yet?) -
#'   it would just make the outputs of a batch tool into a data.table, which it already should be I think,
#'   renames fields (colnames) to friendlier names,
#'   rounds values to specific numbers of significant digits,
#'   writes the output as a csv file locally
#'
#' @param finaldata output of batch tool
#' @param filename what to save output as
#'
#' @export
#'
cleanoutput <- function(finaldata,filename) {

  # this function is not currently used -
  # it would just make the outputs of a batch tool into a data.table, which it already should be I think,
  # renames fields (colnames) to friendlier names,
  # rounds values to specific numbers of significant digits,
  # writes the output as a csv file locally

  ########################################################################################################################### #

  # Make it a data.table ########### #
  result <- data.table::data.table(finaldata)

  ########################################################################################################################### #

  # Rename columns ##############

  newnames <- c(

    # THESE HAVE TO BE IN THE SAME SEQUENCE AS EXISTING COLNAMES IN result FOR THIS APPROACH TO WORK, THOUGH.
    # COULD BE BETTER TO USE ejscreen::change.fieldnames.ejscreen.csv() or a similar approach.

    "FACID","LAT","LON","totpop",
    "mins","RAW_D_MINOR","lowinc","RAW_D_INCOME","lths","RAW_D_LESSHS","lingiso","RAW_D_LING","under5","RAW_D_UNDER5","over64","RAW_D_OVER64",
    "RAW_E_TRAFFIC","RAW_E_LEAD","RAW_E_PM25","RAW_E_O3","RAW_E_CANCER","RAW_E_DIESEL","RAW_E_RESP","RAW_E_TSDF","RAW_E_RMP","RAW_E_NPL","RAW_E_NPDES",
    "RAW_D_INDEX","VSI.svi6",

    # THERE ARE TYPOS IN THESE index names BUT GDB NOW MAY HAVE CORRECTED THOSE, SO NEED TO UPDATE/ CHECK:
    "inedx_EJ_Traffic","inedx_EJ_Lead","inedx_EJ_PM","inedx_EJ_Ozone","inedx_EJ_Cancer","inedx_EJ_DPM","inedx_EJ_Resp",
     "inedx_EJ_proximity.tsdf","inedx_EJ_proximity.rmp","inedx_EJ_proximity.npl","inedx_EJ_proximity.npdes",
    #    could change those but the batch sum lookup name changer can handle these now for 2016

    "blockid","buff","BLOCKGROUPFIPS",
    "stabbr","statename","COUNTY","TRACT","BLKGRP","BLOCK","region",

    # NATIONAL demog and env percentiles etc

    "N_D_INDEX_PER",
    "N_E_NPDES_PER", "N_E_TSDF_PER",  "N_E_RMP_PER","N_E_NPL_PER","N_E_LEAD_PER","N_E_TRAFFIC_PER","N_E_RESP_PER", "N_E_CANCER_PER","N_E_DIESEL_PER","N_E_O3_PER","N_E_PM25_PER",
    "N_D_MINOR_PER", "N_D_INCOME_PER","N_D_LESSHS_PER","N_D_LING_PER","N_D_UNDER5_PER","N_D_OVER64_PER",    "N_D_INDEX",
    "N_E_NPDES",     "N_E_TSDF",      "N_E_RMP", "N_E_NPL", "N_E_LEAD", "N_E_TRAFFIC", "N_E_RESP", "N_E_CANCER","N_E_DIESEL","N_E_O3","N_E_PM25",
    "N_D_MINOR",     "N_D_INCOME",    "N_D_LESSHS", "N_D_LING", "N_D_UNDER5", "N_D_OVER64",

    # REGIONAL demog and env percentiles etc

    "R_D_INDEX_PER",
    "R_E_NPDES_PER", "R_E_TSDF_PER",  "R_E_RMP_PER","R_E_NPL_PER","R_E_LEAD_PER","R_E_TRAFFIC_PER","R_E_RESP_PER","R_E_CANCER_PER","R_E_DIESEL_PER","R_E_O3_PER","R_E_PM25_PER",
    "R_D_MINOR_PER", "R_D_INCOME_PER","R_D_LESSHS_PER","R_D_LING_PER","R_D_UNDER5_PER","R_D_OVER64_PER",     "R_D_INDEX",
    "R_E_NPDES",     "R_E_TSDF",      "R_E_RMP","R_E_NPL","R_E_LEAD","R_E_TRAFFIC","R_E_RESP", "R_E_CANCER","R_E_DIESEL","R_E_O3","R_E_PM25",
    "R_D_MINOR",     "R_D_INCOME",    "R_D_LESSHS","R_D_LING","R_D_UNDER5","R_D_OVER64",

    # STATE demog and env percentiles etc

    "S_D_INDEX_PER",
    "S_E_NPDES_PER", "S_E_TSDF_PER",  "S_E_RMP_PER","S_E_NPL_PER","S_E_LEAD_PER","S_E_TRAFFIC_PER","S_E_RESP_PER", "S_E_CANCER_PER","S_E_DIESEL_PER","S_E_O3_PER","S_E_PM25_PER",
    "S_D_MINOR_PER", "S_D_INCOME_PER","S_D_LESSHS_PER","S_D_LING_PER","S_D_UNDER5_PER","S_D_OVER64_PER",    "S_D_INDEX",
    "S_E_NPDES",     "S_E_TSDF",      "S_E_RMP","S_E_NPL","S_E_LEAD","S_E_TRAFFIC","S_E_RESP", "S_E_CANCER","S_E_DIESEL","S_E_O3","S_E_PM25",
    "S_D_MINOR",     "S_D_INCOME",    "S_D_LESSHS","S_D_LING","S_D_UNDER5","S_D_OVER64",

    # **** to add what had been missing EJ percentiles fields:

    "N_P_NPDES",     "N_P_TSDF",      "N_P_RMP","N_P_NPL","N_P_LEAD","N_P_TRAFFIC","N_P_RESP", "N_P_CANCER","N_P_DIESEL","N_P_O3","N_P_PM25",
    "R_P_NPDES",     "R_P_TSDF",      "R_P_RMP","R_P_NPL","R_P_LEAD","R_P_TRAFFIC","R_P_RESP", "R_P_CANCER","R_P_DIESEL","R_P_O3","R_P_PM25",
    "S_P_NPDES",     "S_P_TSDF",      "S_P_RMP","S_P_NPL","S_P_LEAD","S_P_TRAFFIC","S_P_RESP", "S_P_CANCER","S_P_DIESEL","S_P_O3","S_P_PM25"
  )

  ############################################################# #
  # # To fix typos in gdb fieldnames to my preferred versions
  #
  #   wrongnames <- c("inedx_EJ_Traffic","inedx_EJ_Lead","inedx_EJ_PM","inedx_EJ_Ozone","inedx_EJ_Cancer","inedx_EJ_DPM","inedx_EJ_Resp","inedx_EJ_Neuro","inedx_EJ_proximity.tsdf","inedx_EJ_proximity.rmp","inedx_EJ_proximity.npl","inedx_EJ_proximity.npdes")
  # #  xxx <- gsub(pattern = 'inedx_ej_', replacement = 'EJ.DISPARITY.eo.', x = tolower(wrongnames))
  # #  xxx <- gsub('traffic', 'traffic.score', xxx)
  # #  xxx <- gsub('lead', 'pctpre1960', xxx)
  # #  xxx <- gsub('ozone', 'o3', xxx)
  # #  # or just
  #     xxx <- c(
  #       "EJ.DISPARITY.eo.traffic.score", "EJ.DISPARITY.eo.pctpre1960", "EJ.DISPARITY.eo.pm", "EJ.DISPARITY.eo.o3",
  #       "EJ.DISPARITY.eo.cancer", "EJ.DISPARITY.eo.dpm", "EJ.DISPARITY.eo.resp", "EJ.DISPARITY.eo.neuro",
  #       "EJ.DISPARITY.eo.proximity.tsdf", "EJ.DISPARITY.eo.proximity.rmp", "EJ.DISPARITY.eo.proximity.npl", "EJ.DISPARITY.eo.proximity.npdes"
  #     )
  #   newnames[match(wrongnames, newnames)] <- xxx
  #
  ##   [31,] "inedx_EJ_Traffic"         "EJ.DISPARITY.eo.traffic.score"   "FALSE"
  ##   [32,] "inedx_EJ_Lead"            "EJ.DISPARITY.eo.pctpre1960"      "FALSE"
  ##   [33,] "inedx_EJ_PM"              "EJ.DISPARITY.eo.pm"              "FALSE"
  ##   [34,] "inedx_EJ_Ozone"           "EJ.DISPARITY.eo.o3"              "FALSE"
  ##   [35,] "inedx_EJ_Cancer"          "EJ.DISPARITY.eo.cancer"          "FALSE"
  ##   [36,] "inedx_EJ_DPM"             "EJ.DISPARITY.eo.dpm"             "FALSE"
  ##   [37,] "inedx_EJ_Resp"            "EJ.DISPARITY.eo.resp"            "FALSE"
  ##   [38,] "inedx_EJ_Neuro"           "EJ.DISPARITY.eo.neuro"           "FALSE"
  ##   [39,] "inedx_EJ_proximity.tsdf"  "EJ.DISPARITY.eo.proximity.tsdf"  "FALSE"
  ##   [40,] "inedx_EJ_proximity.rmp"   "EJ.DISPARITY.eo.proximity.rmp"   "FALSE"
  ##   [41,] "inedx_EJ_proximity.npl"   "EJ.DISPARITY.eo.proximity.npl"   "FALSE"
  ##   [42,] "inedx_EJ_proximity.npdes" "EJ.DISPARITY.eo.proximity.npdes" "FALSE"
  ############################################################# #


  data.table::setnames(result, names(result), newnames)

  ########################################################################################################################### #

  # Which columns to output, in what order? ########

  outputfields <- c(
    "FACID","FACID","LAT","LON","totpop","buff","stabbr","statename","region",

    "RAW_E_PM25",   "S_E_PM25_PER","R_E_PM25_PER","N_E_PM25_PER","S_E_PM25","R_E_PM25","N_E_PM25",
    "RAW_E_O3",     "S_E_O3_PER","R_E_O3_PER","N_E_O3_PER","S_E_O3","R_E_O3","N_E_O3",
    "RAW_E_DIESEL", "S_E_DIESEL_PER","R_E_DIESEL_PER","N_E_DIESEL_PER","S_E_DIESEL","R_E_DIESEL","N_E_DIESEL",
    "RAW_E_CANCER", "S_E_CANCER_PER","R_E_CANCER_PER","N_E_CANCER_PER","S_E_CANCER","R_E_CANCER","N_E_CANCER",
    "RAW_E_NEURO",  "S_E_NEURO_PER","R_E_NEURO_PER","N_E_NEURO_PER","S_E_NEURO","R_E_NEURO","N_E_NEURO",
    "RAW_E_RESP",   "S_E_RESP_PER","R_E_RESP_PER","N_E_RESP_PER","S_E_RESP","R_E_RESP","N_E_RESP",
    "RAW_E_TRAFFIC","S_E_TRAFFIC_PER","R_E_TRAFFIC_PER","N_E_TRAFFIC_PER","S_E_TRAFFIC","R_E_TRAFFIC","N_E_TRAFFIC",
    "RAW_E_LEAD",   "S_E_LEAD_PER","R_E_LEAD_PER","N_E_LEAD_PER","S_E_LEAD","R_E_LEAD","N_E_LEAD",
    "RAW_E_NPL",    "S_E_NPL_PER","R_E_NPL_PER","N_E_NPL_PER","S_E_NPL","R_E_NPL","N_E_NPL",
    "RAW_E_RMP",    "S_E_RMP_PER","R_E_RMP_PER","N_E_RMP_PER","S_E_RMP","R_E_RMP","N_E_RMP",
    "RAW_E_TSDF",   "S_E_TSDF_PER","R_E_TSDF_PER","N_E_TSDF_PER","S_E_TSDF","R_E_TSDF","N_E_TSDF",
    "RAW_E_NPDES",  "S_E_NPDES_PER","R_E_NPDES_PER","N_E_NPDES_PER","S_E_NPDES","R_E_NPDES","N_E_NPDES",

    "RAW_D_INDEX",  "S_D_INDEX_PER","R_D_INDEX_PER","N_D_INDEX_PER","S_D_INDEX","R_D_INDEX","N_D_INDEX",
    "RAW_D_MINOR",  "S_D_MINOR_PER","R_D_MINOR_PER","N_D_MINOR_PER","S_D_MINOR","R_D_MINOR","N_D_MINOR",
    "RAW_D_INCOME", "S_D_INCOME_PER","R_D_INCOME_PER","N_D_INCOME_PER","S_D_INCOME","R_D_INCOME","N_D_INCOME",
    "RAW_D_LING",   "S_D_LING_PER","R_D_LING_PER","N_D_LING_PER","S_D_LING","R_D_LING","N_D_LING",
    "RAW_D_LESSHS", "S_D_LESSHS_PER","R_D_LESSHS_PER","N_D_LESSHS_PER","S_D_LESSHS","R_D_LESSHS","N_D_LESSHS",
    "RAW_D_UNDER5", "S_D_UNDER5_PER","R_D_UNDER5_PER","N_D_UNDER5_PER","S_D_UNDER5","R_D_UNDER5","N_D_UNDER5",
    "RAW_D_OVER64", "S_D_OVER64_PER","R_D_OVER64_PER","N_D_OVER64_PER","S_D_OVER64","R_D_OVER64","N_D_OVER64"
  )

  result <- result[ , outputfields, with = FALSE]

########################################################################################################################### #

  # Round to significant digits ########
  #now for the significant numbers **** NOTE THESE RULES ARE ALSO DEFINED IN data(esigfigs, package='ejscreen')

  # 3 sigfigs
  formatfields <- c(
    "RAW_E_PM25",   "S_E_PM25",   "R_E_PM25",   "N_E_PM25",
    "RAW_E_O3",     "S_E_O3",     "R_E_O3",     "N_E_O3",
    "RAW_E_DIESEL", "S_E_DIESEL", "R_E_DIESEL", "N_E_DIESEL"
  )
  for (field in formatfields) {
    result[ , field] <- sapply(result[ , field, with = FALSE], function(N) formatC(signif(N, digits = 3), digits = 3, format = "fg", flag = "#"))
  }

  # 2 sigfigs
  formatfields <- c(
    "RAW_E_CANCER", "S_E_CANCER", "R_E_CANCER", "N_E_CANCER",
    "RAW_E_NEURO",  "S_E_NEURO",  "R_E_NEURO",  "N_E_NEURO",
    "RAW_E_RESP",   "S_E_RESP",   "R_E_RESP",   "N_E_RESP",
    "RAW_E_TRAFFIC","S_E_TRAFFIC","R_E_TRAFFIC","N_E_TRAFFIC",
    "RAW_E_LEAD",   "S_E_LEAD",   "R_E_LEAD",   "N_E_LEAD",
    "RAW_E_NPL",    "S_E_NPL",    "R_E_NPL",    "N_E_NPL",
    "RAW_E_RMP",    "S_E_RMP",    "R_E_RMP",    "N_E_RMP",
    "RAW_E_TSDF",   "S_E_TSDF",   "R_E_TSDF",   "N_E_TSDF",
    "RAW_E_NPDES",  "S_E_NPDES",  "R_E_NPDES",  "N_E_NPDES",
    "S_D_INDEX", "R_D_INDEX", "N_D_INDEX",
    "S_D_MINOR", "R_D_MINOR", "N_D_MINOR",
    "S_D_INCOME","R_D_INCOME","N_D_INCOME",
    "S_D_OVER64","R_D_OVER64","N_D_OVER64"
  )
  for (field in formatfields) {
    result[ , field] <- sapply(result[,field,with = FALSE], function(N) formatC(signif(N, digits = 2), digits = 2,format = "fg", flag = "#"))
  }


  #round to nearest integer  #!!!includes lingual isolation
  formatfields <- c(
    "RAW_D_MINOR","RAW_D_INCOME","RAW_D_LING","RAW_D_LESSHS","RAW_D_UNDER5","RAW_D_OVER64",
    "S_D_LING",  "R_D_LING",  "N_D_LING",
    "S_D_UNDER5","R_D_UNDER5","N_D_UNDER5",
    "N_D_LESSHS","R_D_LESSHS","S_D_LESSHS",
    "RAW_D_INDEX"
  )
  for (field in formatfields) {
    result[ , field] <- sapply(result[ , field, with = FALSE], function(N) round(N, 0))
  }


########################################################################################################################### #

# write.csv ###########

write.csv(result,filename)

return(result)
}


