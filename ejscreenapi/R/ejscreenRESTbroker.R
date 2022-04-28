#' Use EJScreen API for one circular buffer, get raw json or report output
#' 
#' See \url{'https://www.epa.gov/ejscreen/ejscreen-api'}
#' @details  Note the 
#'  # # public IP is 204.47.252.51 and internal is different. 
#'  
#' @param lon a longitude 
#' @param lat a latitude
#' @param distance radius of the circular buffer 
#' @param unit 9035 for miles, 9036 for kilometers
#' @param wkid spatial reference
#' @param f pjson for JSON, report for pdf report
#' @param url URL base for API
#' @param ipurl fixed ip or domain/URL to try
#' 
#' @examples \dontrun{
#'   browseURL(ejscreenRESTbroker(lon = -80, lat = 42, f = 'report'))
#'   x = (ejscreenRESTbroker(lon = -80, lat = 42))
#'   names(jsonlite::fromJSON(rawToChar(x$content)))
#'   }
#'
ejscreenRESTbroker <- function(lon, lat, url='https://awsgeopub.epa.gov/mapper/ejscreenRESTbroker.aspx?namestr=', wkid=4326, 
                               distance=1, unit=9035, f='pjson', ipurl='awsgeopub.epa.gov') {
  
  
  # The public fixed IP is 204.47.252.51     'ejscreen.epa.gov', 
  # The internal fixed IP is  10.147.194.116    'awsgeopub.epa.gov'
  # An app published to the early version of RStudio Connect server cannot resolve ejscreen.epa.gov properly, 
  # since it tries to resolve ejscreen.epa.gov to the internal IP but RStudio Connect Staging server cannot reach that. 
  # url='https://204.47.252.51/mapper/ejscreenRESTbroker.aspx?namestr='
  # Eventually this should work, and it does work if shiny app is launched locally rather than using version on staging server:
   # url='https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx?namestr='
  # awsgeopub.epa.gov  is the IP scripts should use, however. 
  # The current EPA NCC DMZ and Intranet network configuration require the use of split EPA DNS records for all EPA public access systems where EPA internal DNS is different from EPA external DNS.  EPA GIS apps developers should not have problem here as long as they use the proper FQDN. 
  # 
  # Our RStudio Connect server is on an NCC AWS staging environment that is not peered with the NCC AWS production environment which puts us in a situation where the DNS for those servers resolves to the internal EPA 10.x.x.x addresses but since the VPCs are not peered, we cannot physically reach a 10.x.x.x IP address.  If ejscreen/geopub instead has the public IP 204.47.252.51 for both internal and external IP we should be able to reach it.
  # EPA GIS apps should not use the IP# (204.47.252.51) in their scripts to access EPA EJSCREEN apps like the below:
  # https://204.47.252.51/mapper/ejscreenRESTbroker.aspx?namestr=&geometry={"spatialReference":{"wkid":4326},"x":-76.95082031250448,"y":38.764021709177456}&distance=1&unit=9035&areatype=&areaid=&f=pjson
  # 
  # It should use the FQDN instead:
  # https://awsgeopub.epa.gov/mapper/ejscreenRESTbroker.aspx?namestr=&geometry={"spatialReference":{"wkid":4326},"x":-76.95082031250448,"y":38.764021709177456}&distance=1&unit=9035&areatype=&areaid=&f=pjson
  # https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx?namestr=&geometry={"spatialReference":{"wkid":4326},"x":-76.95082031250448,"y":38.764021709177456}&distance=1&unit=9035&areatype=&areaid=&f=pjson
  
  url <- sub('(https://).*?(/mapper)', paste0('\\1',ipurl,'\\2'), url)
   
   if (any(NROW(lon) > 1, NROW(lat) > 1, NROW(distance) > 1 )) {stop('input must be only one point with one distance')}
  
  # MAY WANT TO SPLIT THIS OUT AS A FUNCTION, TO MAKE IT EASIER TO GET JSON AND ALSO APPEND THE PDF URL TO THAT
  this_request <-  paste0(url,
                          '&geometry={"spatialReference":{"wkid":',wkid,'},',
                          '"x":', lon, ',"y":', lat, '}',
                          '&distance=', distance,
                          '&unit=', unit, 
                          '&areatype=',
                          '&areaid=',
                          '&f=', f
  )
  # geometry <- paste0('{"spatialReference":{"wkid":',wkid, '},','"x":', lon, ',"y":', lat, '}')
  # url <- urltools::param_set(url, key = "geometry", value = geometry)
  # url <- urltools::param_set(url, key = "distance", value = distance)
  # url <- urltools::param_set(url, key = "unit", value = unit)
  # url <- urltools::param_set(url, key = "f",    value = f)
  
  if (f == 'report') {
    PDFURL <- gsub('ejscreenRESTbroker', 'EJSCREEN_report', this_request)
    # https://ejscreen.epa.gov/mapper/EJSCREEN_report.aspx?namestr=&geometry={"spatialReference":{"wkid":4326},"x":-88.14039550781403,"y":40.06610618160108}&distance=1&unit=9036&areatype=&areaid=&f=report 
    return(PDFURL) # returns the URL
  } else {
    # print(this_request)
    return( try(httr::GET(this_request)))
    # return( httr::GET(this_request)$content)
  }
  
  # example that works:
  # httr::GET(
  #    paste0('https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx?namestr=&',
  #                  'geometry={"spatialReference":{"wkid":4326},"x":',
  #                  -80, ',"y":',42,
  #                  '}&distance=', 1,'&unit=9035&areatype=&areaid=&f=pjson')
  # )
  ### https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx?namestr=&geometry={"spatialReference":{"wkid":4326},"x":-80,"y":42}&distance=1&unit=9035&areatype=&areaid=&f=pjson 
}
