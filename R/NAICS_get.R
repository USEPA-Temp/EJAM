#' script to download NAICS file with code and name of sector
#'
#' See source code. Mostly just a short script to get the 2017 or 2022 codes and names.
#' See \url{'https://www.census.gov/naics/?48967'}
#' @param year which vintage of NAICS codes to use, 2012, 2017, or 2022
#' @param urlpattern full url of xlsx file to use, but with YYYY instead of year
#' @param destfile full path and name of file to save as locally
#'
#' @return names list with year as an attribute
#' @export
#'
NAICS_get <- function(year=2017, urlpattern='https://www.census.gov/naics/YYYYNAICS/2-6%20digit_YYYY_Codes.xlsx', destfile= paste0('~/Downloads/',year,'NAICS.xlsx')) {
  # this can be used to create the NAICS dataset as for this package
  # See \url{https://www.census.gov/naics/}
  if (!(year %in% c(2012, 2017, 2020))) {stop('only works for 2012, 2017, 2020')}
  url <- gsub('YYYY',year, urlpattern)
  if (year == 2012) {url <- gsub('6%20', '', url)} 
  # 'https://www.census.gov/naics/2022NAICS/2-6%20digit_2022_Codes.xlsx'
  # 'https://www.census.gov/naics/2017NAICS/2-6%20digit_2017_Codes.xlsx'
  # 'https://www.census.gov/naics/2012NAICS/2-digit_2012_Codes.xls'
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
  
  cat('To update source code R package, try usethis::use_data(NAICS.rdata) or save(NAICS, file = \'yourpath/EJAM/data/NAICS.rdata\') \n')
  # usethis::use_data(NAICS.rdata)
  # save(NAICS, file = './data/NAICS.rdata')
  
  return(NAICS)
}
