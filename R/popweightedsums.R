#' Get population weighted sums of indicators
#'
#' @param data data.table with demographic and/or environmental data
#' @param fieldnames vector of terms like pctmin, traffic.score, pm, etc.
#' @param fieldnames_out optional, should be same length as fieldnames
#' @param scaling number to multiply raw values by to put in right units like percent 0-100 vs 0.0-1.0
#' @param popname name of column with population counts to use for weighting
#'
#' @export
#'
popweightedsums <- function(data, fieldnames, fieldnames_out, scaling, popname='POP100') {

  if (missing(fieldnames)) {
    fieldnames <- c(
      "mins", "pctmin", "under5", "pctunder5", "over64", "pctover64",
      "traffic.score", "pctpre1960", "pm", "o3", "cancer", "dpm", "resp","proximity.tsdf", "proximity.rmp", "proximity.npl", "proximity.npdes","pctlths"
    )
    # TAKEN OUT:
    #"lowinc", "pctlowinc", "lths", "pctlths", "lingiso", "pctlingiso",
    scaling <- c(1,100,1,100,1,100,
                 1,1,1,1,1,1,1,1,1,1,1,1)
  }
  fieldnames_out <- fieldnames
  if (missing(fieldnames_out)) {
    fieldnames_out <- fieldnames
  }
  if (missing(scaling)) {
    scaling <- rep(1, times=length(fieldnames))
  }

  # ALSO, I do not understand why pctmin etc. are listed as using popwtd summation - did that actually mean popwtd average??


  # *** The rollup of all block group scores in buffer is done via block-population-weighted mean of raw %scores from relevant parts of blockgroups in buffer.
  # ESCREEN's existing buffering code as of 2021 has always just used popwtd mean of raw % scores, it looks like.***
  # Even for raw % scores that have original denominators other than pop:
  #   pctlowinc (actually % of those for whom poverty ratio was known),
  #   pctlths (actually % of those age 25up),
  #   pctlingiso (actually % of households or units?),
  #   pctpre1960 (actually % of households or units?).
  # Ideally a more exact method to find the overall %low income, etc., might be to sum the counts and then find the rolled up % as the ratio of counts,
  # using the correct denominator for each %, such as households, people over age 25, etc.
  # Would have to change this code to roll up to buffer using ratio of sums rather than popwtd mean.
  #  *** BUT ... EJSCREEN existing buffering as of 2021 has always just used popwtd mean of raw % scores, it looks like.  ***
  # and the answer is the same thing if the denominator is pop anyway,
  # or if the % in each blockgroup is interpreted and treated as the percent of people (not just hhlds, povknownratio, age25up, etc.),
  # or if those more correct denominators are roughly the same fraction of total pop for each of the blockgroups involved in the rollup.
  # The bottom line is we just can use popwtd mean of % values to do rollup for those % indicators.

  #aggregation of fields for demographics
  poweightedsums_subres <- list()
  listindex <- 1

  for (field in fieldnames) {

    str2exec <- paste(
      "poweightedsums_subres[[", listindex, "]] <- data[, j = list(", fieldnames_out[listindex],
      " =  ifelse(is.na(as.double(mean(", field, "))), as.double(NA), ",
      "sum(", scaling[[listindex]], "* ", popname, " * scoringweight *", field, ", na.rm = TRUE) / ",
      "sum(", popname, " * scoringweight, na.rm = TRUE))), by = ID]",
      sep = ""
    )
    eval(parse(text = str2exec))
    #print(str2exec)

    data.table::setkey(poweightedsums_subres[[listindex]], "ID")
    listindex <- listindex + 1
  }

  #merge together
  result <- base::Reduce(merge, poweightedsums_subres)

  return(result)
}
