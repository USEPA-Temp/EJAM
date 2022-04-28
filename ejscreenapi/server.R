server <- function(input, output) {
  
  ##################### Upload point locations csv file ####
  
  pts <- shiny::reactive({
    pts_filecontents <- readr::read_csv(file = input$pointsfile$datapath)
    if ('lat' %in% names(pts_filecontents) & 'lon' %in% names(pts_filecontents)) {
    } else {
      # if not uploaded yet, show default 2 point example. if uploaded with bad colnames, show popup error message
      if (0 == length(pts_filecontents)) { 
        pts_filecontents <- default_points_shown_at_startup  # defined in global.R
      } else {
        if ('FacLong' %in% names(pts_filecontents) & 'FacLat' %in% names(pts_filecontents)) {
          # for now, allow those 2 ECHO column names and just silently rename to be lat and lon
          names(pts_filecontents) <- gsub('FacLat', 'lat', names(pts_filecontents)); names(pts_filecontents) <- gsub('FacLong', 'lon', names(pts_filecontents)) # as used by leaflet, and so names are unique even when uploaded table is merged with EJScreen results
          # the variable names latitude and longitude are compatible with leaflet() but we will not rename them except for that one purpose right when mapping
        } else {
          showModal(modalDialog(title = "Error",paste0("csv file must have columns named lat and lon (the words lat and lon should be in the 1st row) ", ''), easyClose = TRUE))
          pts_filecontents <- default_points_shown_at_startup  # defined in global.R   This line is so default example is shown instead of uploaded file that does not have correct columns 
        }
      }
    }
    pts_filecontents
  })
  output$count <- renderText({paste0(NROW(pts()), ' points uploaded')})
  output$speed <- renderText({speedmessage(NROW(pts()), perhourslow = perhourslow, perhourfast = perhourfast, perhourguess = perhourguess)})  # speedmessage defined as function in R folder, default variables defined in global.R
  output$maxn  <- renderText({paste0(maxpts, ' points max. allowed')})   # maxpts defined in global.R
  
  output$rendered_input_table <- DT::renderDataTable({pts()})
  
  ##################### Get results via EJScreen API ####
  
  results_table <- bindEvent(reactive({ 
    # bindEvent makes it wait until click run/start button
    # ignoreInit = TRUE, 
    results_table <- 0
    results_table <- isolate({   # isolate this to avoid refresh every time pts or radius changes, but button not yet pushed
      if (length(pts()) != 0) {
        if (NROW(pts()) <= maxpts) {
          showModal(modalDialog(
            title="RESULTS TABLE LOADING - PLEASE WAIT...",
            speedmessage(NROW(pts()), perhourslow = perhourslow, perhourfast = perhourfast, perhourguess = perhourguess),
            # "Please wait for results to be downloaded into table before proceeding.",
            size="l",footer=NULL))
          
          whichip <- input$whichip
          
          batchtableout <- ejscreenapi(lon = pts()$lon, lat=pts()$lat, radius = input$radius, on_server_so_dont_save_files=TRUE,
                                       unit ='miles', wkid=4326 , report_every_n=1000, save_when_report=FALSE, format_report_or_json='pjson',ipurl=whichip)
          cbind(pts(), batchtableout)
          
        } else {
          showModal(modalDialog(title = "Warning",paste0("Maximum number of points allowed here is ", maxpts), easyClose = TRUE))
        }
      }})
    
    # Make pdf report URL clickable in table, move to near 1st column, and put id column first
    # can print to pdf with margins = c(0.3, 0.3, 0.3, 1.75) # Left Top Right Bottom
    encodedlink <- URLencode(results_table$pdfurl)
    pdfurl <- paste0('<a href=\"', encodedlink, '\", target=\"_blank\">View Report ', rownames(results_table), '</a>')
    results_table$pdfurl <- NULL
    # put id and pdf link as first couple of columns
    idcolnum <- which(names(results_table) == 'id')
    results_table <- data.frame(id = results_table$id, EJScreenPDF=pdfurl, results_table[,-idcolnum], stringsAsFactors = FALSE)
    
    results_table
    
  }), input$runbutton  ) # this makes it react only when button is pushed (other other non-isolated reactives above change, like input$usenicenames)
  
  output$rendered_results_table <- DT::renderDataTable({
    table_as_displayed <- results_table()
    if (input$usenicenames) {
      names(table_as_displayed) <- fixnames(names(table_as_displayed))
    } 
    table_as_displayed
  }, 
  options = list(scrollX = TRUE), 
  escape = FALSE) 
  # escape false allows ejscreen pdf URL to be a link, but then links from ECHO wont be clickable in the table
  
  ##################### Remove popup window when results are ready ####
  
  observe({
    req(results_table())
    removeModal()
    # maybe remove UI here, to not show the input table anymore
  })
  
  ##################### popup link to ECHO search ####
  
  bindEvent(observe({
    showModal(
      modalDialog(title = "Use ECHO facility search tools to specify list of sites",
                  echo_message,
                  HTML(paste('<a href=\"', echo_url, '\", target=\"_blank\">', echo_url,  '</a>', sep = '')),
                  easyClose = TRUE 
      )
    )
  }), input$echobutton )
  
  ##################### Download results ####
  
  # perhaps could make this button only appear once there is a dataset uploaded
  output$downloadbutton <- shiny::downloadHandler(
    filename = 'output.csv', contentType = 'text/csv', 
    content = function(file) {
      if (0 == length(results_table())) { 
        showModal(modalDialog(title = "Warning", "Results are not created until Start is clicked", easyClose = TRUE))
      } else {
        if (NROW(pts()) != NROW(results_table())) {
          # this might mean they uploaded a new dataset and did not hit Start again. could check for that more specifically, but this is probably good enough for now. might happen if API fails to return some rows? or maybe ejscreenapi function fixed that so it always returns same number as submitted.
          showModal(modalDialog(title = "Warning", "Uploaded points and results table have different numbers of rows", easyClose = TRUE))
        }
        
        table_as_displayed <- results_table()
        if (input$usenicenames) {
          names(table_as_displayed) <- fixnames(names(table_as_displayed))
        } 
        table_as_displayed
        
        write.csv(table_as_displayed, file, row.names = FALSE)
      }
    })
  
  ##################### Draw a Map ####
  
  output$mapout <- leaflet::renderLeaflet({
    mypoints <- pts()
    
    if (length(mypoints) != 0) {
      names(mypoints) <- gsub('lon','longitude', names(mypoints)); names(mypoints) <- gsub('lat','latitude', names(mypoints))
      isolate( # do not redraw entire map and zoom out and reset location viewed just because radius changed
        leaflet(mypoints) %>% addTiles() %>% addCircles(radius = input$radius * meters_per_mile) 
      )
    } else {
      leaflet() %>% addTiles() %>% setView(-110, 46, zoom = 3)
    }
    # # Another option would be to use mapView function in mapview package, which is easier in general but a bit trickier in shiny apps it seems
    # projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" #  lat / lon
    # try({mapview  ::  mapView(   (sf  ::  st_as_sf(x = (pts()), coords = c("lon", "lat"), crs = projcrs)))@map}, silent = TRUE)
  } )
  
  observe({
    mypoints <- pts()
    # need to fix this but trying to show simple numbering popup info when uploaded new data different row count than results (usually because uploaded points not run yet, but that is not quite right)
    # also just use row number if sitename column name not found?
    if (!('sitename' %in% names(mypoints)) | NROW(pts()) != NROW(results_table())) {
      mypoints$sitename <- seq_len(NROW(mypoints))
    } 
    
    if (length(mypoints) != 0 & NROW(pts()) == NROW(results_table())) {
      
      
      # #  could make popup more useful:
      
      mypoints = data.frame(
        lon = results_table()$lon,
        lat =  results_table()$lat,
        n =    results_table()$id,
        # name = results_table()$sitename,
        pop =  results_table()$totalPop,  # or pop
        pctmin =    results_table()$RAW_D_MINOR,
        pctlowinc = results_table()$RAW_D_INCOME
      )
      mypopup = paste(
        'Site #', mypoints$n, '<br>',
        # 'Name: ', mypoints$name, '<br>',
        'Pop= ', mypoints$pop, '<br>',
        mypoints$pctlowinc, '% low-income', '<br>',
        mypoints$pctmin, '% people of color',
        sep = '')
      
      leafletProxy("mapout", data = mypoints) %>%
        clearShapes() %>%
        addCircles(
          radius = input$radius * meters_per_mile,
          popup = mypopup # ~paste(sitename)
        )
    }
  })
}

