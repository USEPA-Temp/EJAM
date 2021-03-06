% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/popweightedsums.R
\name{popweightedsums}
\alias{popweightedsums}
\title{Get population weighted sums of indicators}
\usage{
popweightedsums(data, fieldnames, fieldnames_out, scaling, popname = "POP100")
}
\arguments{
\item{data}{data.table with demographic and/or environmental data}

\item{fieldnames}{vector of terms like pctmin, traffic.score, pm, etc.}

\item{fieldnames_out}{optional, should be same length as fieldnames}

\item{scaling}{number to multiply raw values by to put in right units like percent 0-100 vs 0.0-1.0}

\item{popname}{name of column with population counts to use for weighting}
}
\description{
Get population weighted sums of indicators
}
