#' weighted sum of linguistic isolation scores - not completely sure how the adjustment works
#'
#' @param data data.table with colnames hhlds, pctlingiso, scoringweight
#'
#' @export
#'
sum_pctlanguage <- function(data) {
  # works for the initial records 
  # subres <- data[hhlds!=0, j = list(pctlingiso =  ifelse(is.na(as.double(mean(pctlingiso))), as.double(NA) , sum(100*scoringweight*hhlds*pctlingiso, na.rm = TRUE)/sum(hhlds*scoringweight, na.rm = TRUE))), 
  # by = ID]
  #subres <- data[hhlds!=0, j = list(pctlingiso =  ifelse(is.na(as.double(mean(pctlingiso))), as.double(NA), sum(100*scoringweight*hhlds*pctlingiso, na.rm = TRUE)/sum(hhlds*scoringweight, na.rm = TRUE))), 
  # by = ID]

  # *** I do not understand this adjustment process.

  subres <- data[hhlds != 0, j = list(
    pctlingiso = ifelse(is.na(as.double(mean(pctlingiso))), as.double(NA),
                        sum(100*pctlingiso*scoringweight, na.rm = TRUE) /
                          sum(scoringweight, na.rm = TRUE)
    )), by = ID]
  return(subres)
}
