#' weighted mean of percent low income - do not fully understand adjustment process here
#'
#' @param data data.table with pctlowinc, povknownratio, Census2010Totalpop, POP100
#'    where povknownratio is count of those for whom the poverty ratio was known, which is
#'    the more correct denominator when calcuating percent poor or percent low income,
#'    although EJSCREEN does that but subsequently treats pctlowinc as essentially a
#'    percentage of all population in the block group, which is generally not very different
#'    than using the other denominator.
#'
#' @export
#'
sum_pctlowinc <- function(data) {
  #subres <- str2exec <- data[, j = list("pctlowinc" =  sum(100*POP100*scoringweight*pctlowinc, na.rm = TRUE)/sum(POP100*scoringweight, na.rm = TRUE)), by = ID]

  # *** I do not understand this adjustment process. Why not just calculate sums of numerator and denominator normally, then take ratio to get pct?

  # the data.table package lets you reference columns in a data.table without quotes around the colname, unlike data.frame:
  subres <- data[ , j = list(
    pctlowinc =  ifelse(is.na(as.double(mean(pctlowinc))), as.double(NA),
                        sum(100*POP100*povknownratio / Census2010Totalpop*pctlowinc, na.rm = TRUE) /
                          sum(POP100 * povknownratio / Census2010Totalpop, na.rm = TRUE)
    )), by = ID]
  return(subres)
}
