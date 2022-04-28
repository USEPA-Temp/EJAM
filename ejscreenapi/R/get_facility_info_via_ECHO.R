get_facility_info_via_ECHO <- function(p_ncs=NULL, output='JSON', qcolumns=c(16,17), otherparameters=NULL, url_not_query=TRUE, testing=TRUE, getcsv=FALSE) {

  
  
  
  
  
  
  #  THIS IS JUST WORK IN PROGRESS, EXPLORING HOW TO USE THE ECHO API TO QUERY ECHO/FRS 
  #  AND GET A LIST OF LAT/LON/ETC BY NAICS
  
  
  
  
  
  
  
  
  
  
  
  
  if (testing) p_ncs = '325'
  
  baseurl <- 'https://echodata.epa.gov/echo/echo_rest_services.get_facilities_info'
  # basevalidate <- 'https://echodata.epa.gov/echo/echo_rest_services.get_facilities'
  # https://echo.epa.gov/tools/web-services/facility-search-all-data#/Facility%20Info/get_echo_rest_services_get_facility_info
  # https://echodata.epa.gov/echo/echo_rest_services.get_facilities?output=JSON&p_ncs=325%2C331410&qcolumns=16%2C17
  # https://www.naics.com/search/ 
  # p_ncs=c(325,331410)
  # good url #  https://echodata.epa.gov/echo/echo_rest_services.get_facility_info?p_ncs=325&qcolumns=16%2C17
  #
  # JUST ONE STATE, ONE 3-DIGIT NAICS, ASK FOR LAT / LON COLUMNS:
  #'https://echodata.epa.gov/echo/echo_rest_services.get_facility_info?p_st=DE&p_ncs=325&qcolumns=16%2C17'
  #
  # PROBLEM WITH ECHO API is you get lat/lon per cluster of facilities, not every facility, 
  #  if you try to ask for too many in one query, via get_facility_info
  
  if (is.null(p_ncs)) {
    p_ncs <- ''
  } else {
    p_ncs <- paste0('&p_ncs=', paste(p_ncs, collapse = ','))
  }
  
  if (is.null(qcolumns)) {
    qcolumns <- ''
  } else {
    qcolumns <- paste0('&qcolumns=', paste(qcolumns, collapse = ','))
  }
  
  if (is.null(output)) {
    output <- ''
  } else {
    output <- paste0('output=', output)
  }
  
  urlquery <- paste0(baseurl, '?', output, p_ncs, qcolumns)
  if (!is.null(otherparameters)) {
    urlquery <- paste0(urlquery, otherparameters)
  }
  
  if (testing)  urlquery <- paste0(urlquery, '&p_st=DE') # testing
  
  urlquery <- URLencode(urlquery) # urltools::url_encode(urlquery) 
  
  if (testing) {
    print(p_ncs)
    print(qcolumns)
    print(urlquery)
  }
  
  # just return URL that would give full results on facilities ####
  
  if(url_not_query) {return(urlquery)}
  
  
  # stop if cannot validate query,  and get basic info    ####
  
  urlvalidation <- gsub('get_facilities_info', 'get_facilities', urlquery)
  x <- httr::GET(urlvalidation) 
  x <- as.data.frame(as.list(unlist((jsonlite::fromJSON(rawToChar(x$content)))[['Results']])))
  if (x$Message != "Success") {warning('failed query'); return()}
  
  hits <- x$QueryRows
  print(paste0(hits,' results found'))
  
  
  # do full query for csv format, save as csv file and stop #### 
  
  if (getcsv) {
    warning('does not work completely - glitches in format of csv data')
    # QueryID to get csv download
    # https://echodata.epa.gov/echo/echo_rest_services.get_download?output=csv&qid=311
    mycsv <- paste0('https://echodata.epa.gov/echo/echo_rest_services.get_download?output=csv&qid=', x$QueryID)
    mycsv <- try(httr::GET(mycsv))
    if (mycsv$Message != "Success") {stop('csv query failed')}
    mycsv <- (rawToChar(mycsv$content))
    # write to file
    cat(mycsv, file = 'testfile.csv')
    invisible(mycsv) # cat()  on the returned value shows it as csv format
  }
  
 
  # do full query and turn into a data.frame  ####
 
  requested <- try(httr::GET(urlquery))  # THIS FAILS NOW ********
  if  (any(class(requested) == 'try-error')) {stop('GET failed')}
  
  # parse results to data.frame format, for just this one buffer ####
  sitelist <- try(data.table::as.data.table(jsonlite::fromJSON(rawToChar(requested$content))))
  
  # if error in JSON parsing ####
  if  (any(class(sitelist) == 'try-error')) {
    warning('error in parsing JSON returned by ECHO API ', i, ' - Returning no result.')
    sitelist <- NA
  }
  return(sitelist)
}
