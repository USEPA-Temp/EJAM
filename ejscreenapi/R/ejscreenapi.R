#' Use EJScreen API to get stats on each circular buffer
#' 
#' Requests a standard EJScreen report for each of one or more circular buffers.
#' Specify a radius and vector of latitude longitude points,
#' and get for a buffer the population weighted mean value of each raw indicator
#' like percent low-income, and total population count, and percentiles for those
#' raw indicator scores, all from EJScreen, as in an EJScreen standard report. 
#' Note that this API is fairly slow, so it is fine for 10 sites, but not large numbers.
#' It does maybe about 7k to 10k sites per hour, for circular buffers of 1 or 3 mile radius.
#' It sometimes crashes, with a JSON lexical error, 
#' which may be caused by unreliable results from the API
#' rather than the code requesting results via the API.
#' See \url{'https://www.epa.gov/ejscreen/ejscreen-api'}
#' 
#' @param lon Longitude numeric vector
#' @param lat Latitude numeric vector
#' @param radius radius of circular buffer 
#' @param unit  miles (default) or kilometers
#' @param wkid optional spatial reference code
#' @param report_every_n Should it report ETA snd possibly save interim file after every n points
#' @param save_when_report optional, write .rdata file to working directory 
#'   with results so far, after ever n points, to have most results even if it crashes
#' @param format_report_or_json Not implemented. default is pjson but could modify to allow it to be report to get a pdf 
#' @param on_server_so_dont_save_files FALSE by default, but TRUE prevents saving any progress or crash-related files
#' @param ipurl which URL or IP to try
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
#'  t(out[1:2,]) 
#'  }
ejscreenapi <- function(lon, lat, radius=5, unit ='miles', wkid=4326 , report_every_n=1000, save_when_report=FALSE, 
                        format_report_or_json='pjson', on_server_so_dont_save_files=FALSE, ipurl='awsgeopub.epa.gov') {
  
  # error checking ####
  #################################################################################### #
  if (!(unit %in% c('miles', 'kilometers'))) {stop('unit must be miles or kilometers')}
  unitcode = switch (unit,
                 'miles' = 9035,
                 'kilometers' = 9036
  )
  if (!(format_report_or_json %in% 'pjson')) {stop('format_report_or_json must be pjson')}
  # if (!(format_report_or_json %in% c('pjson', 'report'))) {stop('format_report_or_json must be pjson or report')}
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
  
  finished_without_crashing <- FALSE
  outlist <- list()  # outlist[!(sapply(outlist, is.null))]
  on.exit({if (!finished_without_crashing) { 
    if (!on_server_so_dont_save_files) {save(outlist, file='saved_this_before_crash.rdata')}
    } })
  # example of an error code crashing a large batch run:
  # Iteration #: 6479
  # Iteration #: 6480Error: lexical error: invalid char in json text.
  # <!DOCTYPE html>  <html>      <h
  # (right here) ------^
  #   In addition: Warning message:
  #   In ejscreenapi(x$lon, lat = x$lat, radius = radius,  :
  #                                      
  #                                      Error: lexical error: invalid char in json text.
  #                                    <!DOCTYPE html>  <html>      <h
  #                                    (right here) ------^ > View(x)
  #################################################################################### #
  
  # Column names in 2021 returned by API - note it excludes the geometry column ####
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
  
  ##  # sort order returned by API: as of 3/2022:  ####
  # c("RAW_D_MINOR", "RAW_D_INCOME", "RAW_D_LESSHS", "RAW_D_LING", 
  #   "RAW_D_UNDER5", "RAW_D_OVER64", "RAW_D_UNEMPLOYED", "RAW_D_INDEX", 
  #   "RAW_E_LEAD", "RAW_E_DIESEL", "RAW_E_CANCER", "RAW_E_RESP", "RAW_E_TRAFFIC", 
  #   "RAW_E_NPDES", "RAW_E_NPL", "RAW_E_RMP", "RAW_E_TSDF", "RAW_E_O3", 
  #   "RAW_E_PM25", "RAW_E_UST",            "S_D_MINOR", "S_D_INCOME", "S_D_LESSHS", 
  #   "S_D_LING", "S_D_UNDER5", "S_D_OVER64", "S_D_UNEMPLOYED", "S_D_INDEX", 
  #   "S_E_LEAD", "S_E_DIESEL", "S_E_CANCER", "S_E_RESP", "S_E_TRAFFIC", 
  #   "S_E_NPDES", "S_E_NPL", "S_E_RMP", "S_E_TSDF", "S_E_O3", "S_E_PM25", 
  #   "S_E_UST", "S_D_MINOR_PER", "S_D_INCOME_PER", "S_D_LESSHS_PER", 
  #   "S_D_LING_PER", "S_D_UNDER5_PER", "S_D_OVER64_PER", "S_D_UNEMPLOYED_PER", 
  #   "S_D_INDEX_PER", "S_E_LEAD_PER", "S_E_DIESEL_PER", "S_E_CANCER_PER", 
  #   "S_E_RESP_PER", "S_E_TRAFFIC_PER", "S_E_NPDES_PER", "S_E_NPL_PER", 
  #   "S_E_RMP_PER", "S_E_TSDF_PER", "S_E_O3_PER", "S_E_PM25_PER", 
  #   "S_E_UST_PER", "S_P_LEAD", "S_P_DIESEL", "S_P_CANCER", "S_P_RESP", 
  #   "S_P_TRAFFIC", "S_P_NPDES", "S_P_NPL", "S_P_RMP", "S_P_TSDF", 
  #   "S_P_O3", "S_P_PM25", "S_P_UST",           "R_D_MINOR", "R_D_INCOME", "R_D_LESSHS", 
  #   "R_D_LING", "R_D_UNDER5", "R_D_OVER64", "R_D_UNEMPLOYED", "R_D_INDEX", 
  #   "R_E_LEAD", "R_E_DIESEL", "R_E_CANCER", "R_E_RESP", "R_E_TRAFFIC", 
  #   "R_E_NPDES", "R_E_NPL", "R_E_RMP", "R_E_TSDF", "R_E_O3", "R_E_PM25", 
  #   "R_E_UST", "R_D_MINOR_PER", "R_D_INCOME_PER", "R_D_LESSHS_PER", 
  #   "R_D_LING_PER", "R_D_UNDER5_PER", "R_D_OVER64_PER", "R_D_UNEMPLOYED_PER", 
  #   "R_D_INDEX_PER", "R_E_LEAD_PER", "R_E_DIESEL_PER", "R_E_CANCER_PER", 
  #   "R_E_RESP_PER", "R_E_TRAFFIC_PER", "R_E_NPDES_PER", "R_E_NPL_PER", 
  #   "R_E_RMP_PER", "R_E_TSDF_PER", "R_E_O3_PER", "R_E_PM25_PER", 
  #   "R_E_UST_PER", "R_P_LEAD", "R_P_DIESEL", "R_P_CANCER", "R_P_RESP", 
  #   "R_P_TRAFFIC", "R_P_NPDES", "R_P_NPL", "R_P_RMP", "R_P_TSDF", 
  #   "R_P_O3", "R_P_PM25", "R_P_UST",           "N_D_MINOR", "N_D_INCOME", "N_D_LESSHS", 
  #   "N_D_LING", "N_D_UNDER5", "N_D_OVER64", "N_D_UNEMPLOYED", "N_D_INDEX", 
  #   "N_E_LEAD", "N_E_DIESEL", "N_E_CANCER", "N_E_RESP", "N_E_TRAFFIC", 
  #   "N_E_NPDES", "N_E_NPL", "N_E_RMP", "N_E_TSDF", "N_E_O3", "N_E_PM25", 
  #   "N_E_UST", "N_D_MINOR_PER", "N_D_INCOME_PER", "N_D_LESSHS_PER", 
  #   "N_D_LING_PER", "N_D_UNDER5_PER", "N_D_OVER64_PER", "N_D_UNEMPLOYED_PER", 
  #   "N_D_INDEX_PER", "N_E_LEAD_PER", "N_E_DIESEL_PER", "N_E_CANCER_PER", 
  #   "N_E_RESP_PER", "N_E_TRAFFIC_PER", "N_E_NPDES_PER", "N_E_NPL_PER", 
  #   "N_E_RMP_PER", "N_E_TSDF_PER", "N_E_O3_PER", "N_E_PM25_PER", 
  #   "N_E_UST_PER", "N_P_LEAD", "N_P_DIESEL", "N_P_CANCER", "N_P_RESP", 
  #   "N_P_TRAFFIC", "N_P_NPDES", "N_P_NPL", "N_P_RMP", "N_P_TSDF", 
  #   "N_P_O3", "N_P_PM25", "N_P_UST",            "stateAbbr", "stateName", "epaRegion", 
  #   "totalPop", "NUM_NPL", "NUM_TSDF", "statLayerCount", "statLayerZeroPopCount", 
  #   "weightLayerCount", "timeSeconds", "distance", "unit", "statlevel", 
  #   "inputAreaMiles")
  ################################################################################### #
  
  # prepare to store results ####
  #  prepare to return an empty row when no valid results found for that point
  emptyresults <- data.frame(matrix(data = NA, nrow = 1, ncol = length(outcolnames))) 
  colnames(emptyresults) <- outcolnames
  
  benchmark.start <- Sys.time()  
  
  # warn if invalid lat lon values ####
  ok_point <- valid_lat_lon(lat, lon)
  if (!all(ok_point)) {
    warning(paste0(sum(!ok_point), ' lat lon values look invalid'))
    lat[!ok_point] <- NA
    lon[!ok_point] <- NA
    # rather than drop those rows, try to return NA values
  }
  
  pts <- data.frame(lon=lon, lat=lat)
  n <- NROW(pts)
  cat("\n")
  cat("Buffering for", n, 'points for radius of', radius, '\n')
  cat('\n')
  
  # pre-allocate space for results
  outlist <- vector(mode = 'list', length = n)
  pdfurl <- vector(mode = 'list', length = n)
  noresults_count <- 0
  
  # define speedreport function ####
  speedreport <- function(start,end,n) {
    # report time elapsed and avg speed
    benchmark.start <- start
    benchmark.end <- end
    total.benchmark <- difftime(benchmark.end, benchmark.start)
    total.seconds <- difftime(benchmark.end, benchmark.start, units = 'secs')
    perhour <- round((n/ as.numeric(total.seconds))*3600,0)
    cat('\n')
    cat(paste0(
      'Rate of ',
      format(round((n / as.numeric(total.seconds))*3600,0), big.mark=',', scientific=FALSE), 
      ' buffers per hour (',
      format(n,big.mark = ',', scientific = FALSE),
      ' lat/long pairs per ',
      format(round(as.numeric(total.seconds),0), big.mark = ',', scientific = FALSE),
      ' seconds)'
    )  )
    cat('\n')
    print(round(total.benchmark, 1))
    invisible(perhour)
  }
  
  ###########################  LOOP OVER POINTS ####
  
  for (i in 1:dim(pts)[1]){
    cat(paste0('Iteration #: ', i))
    
    # ***Contact EJScreen server*** via GET(), read results via fromJSON() ####
    # GET() is in the httr package but also in other packages
    # fromJSON() is in RJSONIO and  jsonlite  and  rjson
    
    # One could call ejscreenapi1() here or go directly to the ejscreenRESTbroker() and then fromJSON etc below
    # URL for pdf report is created in ejscreenapi1 but this ejscreenapi would need to use that ejscreenapi1() to get it.
    # ej.data <- ejscreenapi1(
    #   lon = pts$lon[[i]], lat = pts$lat[[i]], 
    #   distance=radius, unit=unit, wkid=wkid,
    #   f=format_report_or_json
    #   
    # )
    failed <- FALSE
    
    ej.data <- try(ejscreenRESTbroker(
      lon = pts$lon[[i]], lat = pts$lat[[i]], 
      distance=radius, unit=unitcode, wkid=wkid,
      f=format_report_or_json, ipurl=ipurl)
    )
    if  (any(class(ej.data) == 'try-error')) {failed=TRUE;warning('API not accessible or failed')}
    
    pdfurl[[i]] <- try(ejscreenapi1(
      lon = pts$lon[[i]], lat = pts$lat[[i]], 
      radius =radius, unit=unit, wkid=wkid,
      format_report_or_json ='report', ipurl=ipurl)
    )
    if  (any(class(pdfurl[[i]]) == 'try-error')) {failed=TRUE;warning('API not accessible or failed')}
    
    # parse results to data.frame format, for just this one buffer ####
    ej.data <- try(data.table::as.data.table(jsonlite::fromJSON(rawToChar(ej.data$content))))

    # if error in JSON parsing ####
    if  (failed | any(class(ej.data) == 'try-error')) {
      failed=TRUE
      warning('error in parsing JSON returned by API for point number ', i, ' - Returning no result for that point.')
      ej.data <- emptyresults
    }
    
    # if null results if area too small or sparsely populated ####
    # BLANK RESULTS THAT API RETURNS DUE TO SMALL BUFFER OR SPARSELY POPULATED AREA
    #
    # Note: api does not return values for coords in highly nonpopulated areas.
    # it just returns this as ej.data:
    #    message                                                messageType
    # 1: Input area too small or sparsely populated for output  underLimit
    #
    # lat lon are stored in a second and third row but the rest of those rows is just duplicated info so drop them
    # lon is in   ej.data[2,'geometry']  and also  is  pts$lon[i]
    # save all but geometry column if got valid results back, i.e. got more than 100 columns instead of an error message
    if (NCOL(ej.data) > 100){
      ej.data$geometry <- NULL 
      outlist[[i]] <- unique(ej.data) 
    } else {
      # assume got error message and save as a table full of NA values
      cat(' No results - (probably because) area too small or sparsely populated for output')
      # confirm that the empty results template is actually the right size (eg maybe API changed what columns are returned)
      if (i>1 & NCOL(emptyresults) != NCOL(outlist[[i - 1]])) {
        warning('Check API and code for updates - No results returned for this point and unexpected number of indicators returned for prior point - API output not matching number of indicators expected by this code')
        # try to use format of the prior element, hopefully not also empty
        outlist[[i]] <- outlist[[i - 1]]
        
        warning('not finished coding for situation with no results at some points, especially for the 1st in the list - may need to output NA values here ')
      }
      outlist[[i]] <- emptyresults
      noresults_count <- noresults_count + 1
    }
    
    # add Latitude Longitude (inputs) to the output ####
    outlist[[i]][,'lon'] <- pts$lon[i]
    outlist[[i]][,'lat'] <- pts$lat[i]
    
    # Report/save progress so far in loop.   ####
    # - SAVE INTERIM RESULTS IF NEEDED, in case it fails after a large number were done.
    # report_every_n=1000, save_when_report=FALSE
    if (i %% report_every_n == 0 | (i > report_every_n & i == n)) {
      # this is end of a group of report_every_n points or is the last incomplete chunk assuming we had a big enough n to bother chunking at all
      chunknum <- ceiling(i/report_every_n)
      chunkstart <- ((chunknum - 1) * report_every_n) + 1  #( i + 1 - report_every_n)
      chunkend <- i
      if (save_when_report & (!on_server_so_dont_save_files) ) {
        x <- outlist[chunkstart:chunkend]
        save(x, file = paste('temp_ejscreenapibatch_outlist_chunk', chunknum, '.rdata', sep = ''))
      }
      # Report speed so far in loop####
      cat('---------------------\n')
      hourly <- speedreport(benchmark.start, Sys.time(), i)
      remaining <- n - i
      hrsleft <- remaining / hourly
      cat('Estimated', round(hrsleft * 60,0), 'minutes remaining \n')
      cat('Results were unavailable for', noresults_count,'out of these', n, 'sites. \n')
    } 
    cat('\n') # END OF LOOP
  } 
  # end of loop over buffers####
  
  # Save interim results if still need to  ####
  if (n < report_every_n & save_when_report & (!on_server_so_dont_save_files) ) {
    # we did not save any chunks, so save whole, in case rbind fails
    save(outlist, file='temp_ejscreenapibatch_outlist_full.rdata')
  }
  
  # ***Format results as a single data.table (like a data.frame) ####
  results <- data.table::rbindlist(outlist, fill = T, idcol = 'id')
  # results <- do.call(rbind, outlist) # if data.frame not data.table
  
  # Fix numeric columns that got turned into character format ####
  results <- makenumericdfFORSHINY(results)
  
  # add URL for pdf report, in results table ####
  # but the URL is created in ejscreenRESTbroker() and not returned
  # URL for pdf report is created in ejscreenapi1 but this ejscreenapi would need to use that ejscreenapi1() to get it.
  # PDFURL <- gsub('ejscreenRESTbroker', 'EJSCREEN_report', this_request)
  # https://ejscreen.epa.gov/mapper/EJSCREEN_report.aspx?namestr=&geometry={"spatialReference":{"wkid":4326},"x":-88.14039550781403,"y":40.06610618160108}&distance=1&unit=9036&areatype=&areaid=&f=report 
  # # # public IP is 204.47.252.51 and internal is different. 
  results$pdfurl <- unlist(pdfurl)
  
  # Report to console on speed ####
  speedreport(benchmark.start,Sys.time(),n)  
  finished_without_crashing <- TRUE
  
  # Return results invisibly
  invisible(results)
}
