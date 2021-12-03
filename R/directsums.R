#' Weighted sum of counts of indicators, used to roll up population counts?
#'
#'  This function seems to just find the scaled and weighted sum(s) of field(s),
#'  using scaling value and scoringweight both multiplied by value of field in data.
#'  But it is unclear why the code uses  scoringweight  and what that is, where it is defined.
#'  Also, this seems to use only POP100 as the only fieldname in data.
#'
#'
#' @param data data.table with environmental and/or demographic indicators. Just total population is default.
#' @param fieldnames vector of column names in data
#' @param scaling multipliers like 1 or 100 to put into correct units
#'
#' @export
#'
directsums <- function(data, fieldnames='POP100', scaling=1) {
  # direct_summation_fields <- c("POP100")
  # **** WHY NOT also  mins, lowinc, povknownratio, age25up, hhlds, etc.?
  # scaling <- c(1)

  directsums_subres <- list()
  listindex <- 1

  for (field in fieldnames) {
    str2exec <- paste(
      "directsums_subres[[", listindex, "]] <- data[, j = list(", field, " = sum(", scaling[[listindex]], "*scoringweight*", field, ", na.rm = TRUE)), by = ID]",
      sep = ""
      )
    eval(parse(text = str2exec))
    #print(str2exec)
    data.table::setkey(directsums_subres[[listindex]], "ID")
    listindex <- listindex + 1
  }
  # merge together
  result <- Reduce(merge, directsums_subres)
  return(result)
}
