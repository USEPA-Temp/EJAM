#' script to download NAICS file with code and name of sector
#'
#' See source code. Mostly just a short script to get the 2017 codes and names.
#' See \url{'https://www.census.gov/naics/?48967'}
#' @param year
#' @param url
#' @param destfile
#'
#' @return names list with year as an attribute
#' @export
#'
NAICS_get <- function(year=2017, url='https://www.census.gov/naics/2017NAICS/2-6%20digit_2017_Codes.xlsx', destfile= '~/Downloads/2017NAICS.xlsx') {
# this can be used to create the NAICS dataset as for this package
# See \url{https://www.census.gov/naics/}
   
    download.file(
    url = url,
    destfile = destfile
  )
  x <- readxl::read_xlsx(path = destfile, skip = 2, col_names = c('n','code','title','b','c','d'))
  mynames <- paste(x$code, ' - ', x$title,sep='')
  mycodes <- as.numeric(as.list(x$code))
  # mynames[(is.na(mycodes))]  # remove the ones that are ranges instead of being a 2-digit or longer code
  # ###  "31-33 - Manufacturing"  "44-45 - Retail Trade"   "48-49 - Transportation and Warehousing"
  NAICS        <- as.list(mycodes[!is.na(mycodes)])
  names(NAICS) <- mynames[!is.na(mycodes)]
  # table(as.numeric(sapply((NAICS), FUN=nchar)))
  # head(cbind(NAICS[substr(NAICS,1,2)=='31']))

  # save as NAICS dataset for package, but with year attribute indicating vintage:
  NAICS <- structure(NAICS, year=year)
  # attr(NAICS, 'year')
  # [1] 2017 # for example
  cat('You can save the dataset using something like save(NAICS, file = \'yourpath/EJAM/data/NAICS.rdata\') \n')
  # save(NAICS, file = './data/NAICS.rdata')
  return(NAICS)
}
