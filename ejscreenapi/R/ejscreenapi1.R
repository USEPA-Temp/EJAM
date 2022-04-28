#' Use EJScreen API to get stats on ONLY ONE circular buffer
#' 
#' Requests a standard EJScreen report for one circular buffer.
#' Specify a radius and vector of latitude longitude points,
#' and get for a buffer the population weighted mean value of each raw indicator
#' like percent low-income, and total population count, and percentiles for those
#' raw indicator scores, all from EJScreen, as in an EJScreen standard report. 
#' Note that this API is fairly slow, so it is fine for 10 sites, but not large numbers.
#' See \url{'https://www.epa.gov/ejscreen/ejscreen-api'}
#' 
#' @param lon Longitude numeric vector
#' @param lat Latitude numeric vector
#' @param radius radius of circular buffer 
#' @param unit miles (default) or kilometers
#' @param wkid optional spatial reference code
#' @param format_report_or_json Not implemented. default is pjson but could modify to allow it to be report to get a pdf 
#' @param ipurl IP or URL start
#' 
#' @examples  
#'  \dontrun{
#'  # Specify size of buffer circle and pick random points as example data
#'  myradius <- 3
#'  pts <- structure(list(lon = c(-96.4798957, -111.7674343, -75.4173589, 
#'  -95.9573172, -87.8402677, -77.9996191, -73.920702, -79.9545638, 
#'  -76.0638877, -114.9881473), lat = c(31.782716, 33.7522735, 39.8697972, 
#'  33.2522474, 41.9763992, 38.4661259, 41.2940801, 32.8099327, 40.9888266, 
#'  36.0043628), id = 1:10), row.names = c(NA, -10L), class = "data.frame")
#'  
#'   out <- ejscreenapi(pts$lon, lat=pts$lat, radius = myradius)
#'  
#'  t(out[1:2,]) 
#'  }
ejscreenapi1 <- function(lon, lat, radius=5, unit ='miles', wkid=4326, 
                         format_report_or_json='pjson', ipurl='awsgeopub.epa.gov') {
  
  # error checking ####
  #################################################################################### #
  if (!(unit %in% c('miles', 'kilometers'))) {stop('unit must be miles or kilometers')}
  unitcode = switch (unit,
                 'miles' = 9035,
                 'kilometers' = 9036
  )
  # if (!(format_report_or_json %in% 'pjson')) {stop('format_report_or_json must be pjson')}
  if (!(format_report_or_json %in% c('pjson', 'report'))) {stop('format_report_or_json must be pjson or report')}
  if (any(is.na(radius)) | any(radius <= 0) | any(radius > 100)) {stop('radius outside allowed range')}
  
  valid_lat_lon <- function(lat, lon) {
    # TRUE only if both lat and lon seem valid
    if( any(badlat <- is.na(lat)) || any(badlon <- is.na(lon)) ) {
      bad <- is.na(lat) | is.na(lon)
    } else {
      bad <- rep(FALSE, length(lat))
    }
    bad <- bad | lat < -90 | lat > 90 | lon < -180 | lon > 180
    return(!bad)
  }
 

  # warn if invalid lat lon values ####
  ok_point <- valid_lat_lon(lat, lon)
  if (!all(ok_point)) {
    warning(paste0(sum(!ok_point), ' lat lon values look invalid'))
    lat[!ok_point] <- NA
    lon[!ok_point] <- NA
    # rather than drop those rows, try to return NA values
  }
  
  
  ej.data <- ejscreenRESTbroker(
    lon = lon, lat = lat, 
    distance=radius, unit=unitcode, wkid=wkid,
    f=format_report_or_json, ipurl=ipurl
  )
  
  # THIS WILL SLOW IT DOWN BY ASKING FOR PDF URL AFTER ASKING FOR REPORT ON BUFFER 
  #  SINCE THAT MEANS CALLING THE api twice, or else one could split out the 
  #  function that constructs the URL and use it to GET json and just use gsub to edit it to have the pdf URL to add to a table made from the json.
  if (format_report_or_json == 'report') {
    # just a pdf report (return the URL for it)
    return(ej.data)
  } else {
    PDFURL <- ejscreenRESTbroker(
      lon = lon, lat = lat, 
      distance=radius, unit=unit, wkid=wkid,
      f='report', ipurl=ipurl
    )
  }
  
  # parse results to data.frame format, for just this one buffer ####
  ej.data <- try(data.table::as.data.table(jsonlite::fromJSON(rawToChar(ej.data$content))))
  # parsed that way it creates 3 rows, identical except for geometry column, which has NA, lon, lat.
  ej.data$geometry <- NULL 
  ej.data <- unique(ej.data)
  #################################################################################### #
  
  # Column names in 2021 - note it excludes the geometry column ####
  # THIS IS ONLY USED TO ENSURE THAT WHEN NO VALID RESULT FOUND FOR A POINT, IT RETURNS AN EMPTY ROW OF CORRECT LENGTH AND WITH THE CORRECT HEADINGS
  
  outcolnames <-  c(
    
    "RAW_E_PM25", "RAW_E_O3", "RAW_E_DIESEL", "RAW_E_CANCER", "RAW_E_RESP", "RAW_E_TRAFFIC", "RAW_E_LEAD", "RAW_E_NPL", "RAW_E_RMP", "RAW_E_TSDF", "RAW_E_NPDES", "RAW_E_UST",
    "RAW_D_INDEX", "RAW_D_MINOR", "RAW_D_INCOME", "RAW_D_LING", "RAW_D_LESSHS", "RAW_D_UNDER5", "RAW_D_OVER64", "RAW_D_UNEMPLOYED",
    
    # STATE 
    
    "S_E_PM25",     "S_E_O3", "S_E_DIESEL", "S_E_CANCER", "S_E_RESP", "S_E_TRAFFIC", "S_E_LEAD", "S_E_NPL", "S_E_RMP", "S_E_TSDF", "S_E_NPDES", "S_E_UST", 
    "S_D_INDEX",      "S_D_MINOR", "S_D_INCOME", "S_D_LING", "S_D_LESSHS", "S_D_UNDER5", "S_D_OVER64", "S_D_UNEMPLOYED",
    "S_P_PM25",     "S_P_O3", "S_P_DIESEL", "S_P_CANCER", "S_P_RESP", "S_P_TRAFFIC", "S_P_LEAD", "S_P_NPL", "S_P_RMP", "S_P_TSDF", "S_P_NPDES", "S_P_UST", 
    "S_E_PM25_PER", "S_E_O3_PER", "S_E_DIESEL_PER", "S_E_CANCER_PER", "S_E_RESP_PER", "S_E_TRAFFIC_PER", "S_E_LEAD_PER", "S_E_NPL_PER", "S_E_RMP_PER", "S_E_TSDF_PER", "S_E_NPDES_PER", "S_E_UST_PER",
    "S_D_INDEX_PER",  "S_D_MINOR_PER", "S_D_INCOME_PER", "S_D_LING_PER", "S_D_LESSHS_PER", "S_D_UNDER5_PER", "S_D_OVER64_PER", "S_D_UNEMPLOYED_PER", 
    
    # REGIONAL
    
    "R_E_PM25",     "R_E_O3", "R_E_DIESEL", "R_E_CANCER", "R_E_RESP", "R_E_TRAFFIC", "R_E_LEAD", "R_E_NPL", "R_E_RMP", "R_E_TSDF", "R_E_NPDES", "R_E_UST", 
    "R_D_INDEX",      "R_D_MINOR", "R_D_INCOME", "R_D_LING", "R_D_LESSHS", "R_D_UNDER5", "R_D_OVER64",  "R_D_UNEMPLOYED",
    "R_P_PM25",     "R_P_O3", "R_P_DIESEL", "R_P_CANCER", "R_P_RESP", "R_P_TRAFFIC", "R_P_LEAD", "R_P_NPL", "R_P_RMP", "R_P_TSDF", "R_P_NPDES", "R_P_UST",
    "R_E_PM25_PER", "R_E_O3_PER", "R_E_DIESEL_PER", "R_E_CANCER_PER", "R_E_RESP_PER", "R_E_TRAFFIC_PER", "R_E_LEAD_PER", "R_E_NPL_PER", "R_E_RMP_PER", "R_E_TSDF_PER", "R_E_NPDES_PER", "R_E_UST_PER", 
    "R_D_INDEX_PER",  "R_D_MINOR_PER", "R_D_INCOME_PER", "R_D_LING_PER", "R_D_LESSHS_PER", "R_D_UNDER5_PER", "R_D_OVER64_PER", "R_D_UNEMPLOYED_PER", 
    
    # NATIONAL
    
    "N_E_PM25",     "N_E_O3", "N_E_DIESEL", "N_E_CANCER", "N_E_RESP", "N_E_TRAFFIC", "N_E_LEAD", "N_E_NPL", "N_E_RMP", "N_E_TSDF", "N_E_NPDES", "N_E_UST", 
    "N_D_INDEX",      "N_D_MINOR", "N_D_INCOME", "N_D_LING", "N_D_LESSHS", "N_D_UNDER5", "N_D_OVER64",  "N_D_UNEMPLOYED",
    "N_P_PM25",     "N_P_O3", "N_P_DIESEL", "N_P_CANCER", "N_P_RESP", "N_P_TRAFFIC", "N_P_LEAD", "N_P_NPL", "N_P_RMP", "N_P_TSDF", "N_P_NPDES", "N_P_UST",
    "N_E_PM25_PER", "N_E_O3_PER", "N_E_DIESEL_PER", "N_E_CANCER_PER", "N_E_RESP_PER", "N_E_TRAFFIC_PER", "N_E_LEAD_PER", "N_E_NPL_PER", "N_E_RMP_PER", "N_E_TSDF_PER", "N_E_NPDES_PER", "N_E_UST_PER", 
    "N_D_INDEX_PER",  "N_D_MINOR_PER", "N_D_INCOME_PER", "N_D_LING_PER", "N_D_LESSHS_PER", "N_D_UNDER5_PER", "N_D_OVER64_PER", "N_D_UNEMPLOYED_PER", 
    
    "stateAbbr", "stateName", "epaRegion", 
    "totalPop", 
    "NUM_NPL", "NUM_TSDF", 
    "statLayerCount", "statLayerZeroPopCount", "weightLayerCount", "timeSeconds", 
    "distance", "unit", "statlevel", "inputAreaMiles"
  )
  
  
    # if error in JSON parsing ####
    if  (any(class(ej.data) == 'try-error')) {
      warning('error in parsing JSON returned by API for point number ', i, ' - Returning no result for that point.')
      ej.data <- emptyresults
    }
       

      
      
    # add Latitude Longitude (inputs) to the output? ####

  
  # Fix numeric columns that got turned into character format ####
  results <- makenumericdfFORSHINY(ej.data)
  
  # get URL for pdf report link #### 
  PDFURL <- gsub('ejscreenRESTbroker', 'EJSCREEN_report', this_request)
  # https://ejscreen.epa.gov/mapper/EJSCREEN_report.aspx?namestr=&geometry={"spatialReference":{"wkid":4326},"x":-88.14039550781403,"y":40.06610618160108}&distance=1&unit=9036&areatype=&areaid=&f=report 
  # # public IP is 204.47.252.51 and internal is different. 
  results$pdf_report <- PDFURL
  
  print(t(results))
  # Return results invisibly
  invisible(results)
}
