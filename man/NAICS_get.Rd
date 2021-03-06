% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NAICS_get.R
\name{NAICS_get}
\alias{NAICS_get}
\title{script to download NAICS file with code and name of sector}
\usage{
NAICS_get(
  year = 2017,
  urlpattern = "https://www.census.gov/naics/YYYYNAICS/2-6\%20digit_YYYY_Codes.xlsx",
  destfile = paste0("~/Downloads/", year, "NAICS.xlsx")
)
}
\arguments{
\item{year}{which vintage of NAICS codes to use, 2012, 2017, or 2022}

\item{urlpattern}{full url of xlsx file to use, but with YYYY instead of year}

\item{destfile}{full path and name of file to save as locally}
}
\value{
names list with year as an attribute
}
\description{
See source code. Mostly just a short script to get the 2017 or 2022 codes and names.
See \url{'https://www.census.gov/naics/?48967'}
}
