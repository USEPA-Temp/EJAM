#' ####################################################################################
# Interactive app for running a batch of buffer/proximity reports for a list of places,
# aggregating envt and demog indicators in each circular buffer around each facility or site
#' ####################################################################################

shinyServer(function(input, output,session) {

  # Select Facilities, Industry, or Locations ##########################################
  numUniverseSource <- function() {
    selInd=0
    if (nchar(input$selectIndustry1)>0 | length(input$selectIndustry2)>0) {
      selInd=1
    }
    numLoc=nrow(dataLocationList())
    numFac=nrow(dataFacList())
    if (!is.null(numFac)) {
      if (numFac>0){
        numFac=1
      }
    }
    if (!is.null(numLoc)) {
      if (numLoc>0){
        numLoc=1
      }
    }
    tot=sum(selInd,numLoc,numFac)
    return (tot)
  }

  getWarning1 <- function() {
    if (nchar(input$selectIndustry1)>0 & length(input$selectIndustry2)>0) {
      print("Please use a single industry select option.")
    }
  }

  getWarning2 <- function() {
    tot=numUniverseSource()
    length_selectIndustry1=0
    length_selectIndustry2=0
    if (!is.null(input$selectIndustry1)) {
      length_selectIndustry1=nchar(input$selectIndustry1)
    }
    if (!is.null(input$selectIndustry2)) {
      length_selectIndustry2=nchar(input$selectIndustry2)
    }
    if (length_selectIndustry1>0 & length_selectIndustry2>0) {
      print("Please use a single industry select option.")
    }
    else if(tot>1) {
      print(paste("Please use only one method of selecting universe (ie, select by industry, location, OR facility)"))
      #print(paste("Please use only one method of selecting universe (ie, select by industry, location, OR facility)",tot,"; numLoc=",nrow(dataLocationList()),"; numFac=",nrow(dataFacList())))
    }
  }

  # Outputs ##########################################

  output$inputWarning <- renderPrint({
    getWarning1();
  })

  output$inputWarning2 <- renderPrint({
    getWarning2();
  })

  output$selectInd1 <- renderPrint({
    if (length(input$selectIndustry1)>1) {
      x=paste(input$selectIndustry1,collapse=", ")
      paste("Selected industries ", x)
    }
    else {
      return("")
    }
  })

  output$selectInd2 <- renderPrint({
    if (length(input$selectIndustry2)>1) {
      x=paste(input$selectIndustry2,collapse=", ")
      paste("Selected industries ", x)
    }
    else {
      return("")
    }
  })

  output$selectScope1 <- renderPrint({
    input$goButton1
    isolate(input$selectFrom1)
  })

  output$selectScope2 <- renderPrint({
    input$goButton1
    isolate(input$selectFrom2) })

  output$inputValue <- renderPrint(input$goButton3)
  output$file1Df <- renderPrint({input$file1})
  output$file2Df <- renderPrint({input$file2})
  myfile1 <- reactive({input$file1})
  myfile2 <- reactive({input$file2})

  # Input Radius, Maxcutoff, etc. parameters ##########################################

  getCutoff <- reactive({
    return(input$cutoffRadius)
  })

  getMaxcutoff <- reactive({
    return(4000)
  })

  setUnique <- reactive({
    if (input$uniqueOutput=="no"){
      return(FALSE)
    }
    else {
      return(TRUE)
    }
  })

  #avoidorphans
  # Expand distance for facilities with no census block centroid within selected buffer distance
  doExpandradius <- reactive({
    if (input$expandRadius=="no"){
      return(FALSE)
    }
    else {
      return(TRUE)
    }
  })



  ####################################################################################################################### #
  # ** -----LOCATION LIST IS USED FOR BUFFERING** ######
  # imported location data
  ####################################################################################################################### #
  dataLocationList <- reactive({
    in2File <- input$file2
    if (is.null(in2File))
      return(NULL)
    isolate(read.table(file=in2File$datapath, sep=',', header=TRUE, quote='"'))
    # does that need to be like the copy below and say data.table::as.data.table(read.table ???
  }
  )

  output$inLocationList <- renderTable(
    {
      if(is.null(dataLocationList())) {return () }
      dataLocationList()
    }
  )
  ####################################################################################### #
  #  Find nearby blocks and aggregate for buffer - via quadtree and doaggregate  # *********** ########
  # IT GETS NEARBY BLOCKS AND AGGREGATES EJSCREEN DATA FROM THE BLOCKS NEAR EACH POINT
  ####################################################################################### #
  dataLocationListProcessed <- reactive({
    if(is.null(dataLocationList())) {return () }
    dataLocDT = data.table::as.data.table(dataLocationList())

    cutoff = getCutoff() # radius (units?)
    maxcuttoff = getMaxcutoff()  # reactive, max distance to search
    get_unique = setUnique()     # reactive, TRUE = stats are for dissolved single buffer to avoid doublecounting. FALSE = we want to count each person once for each site they are near.
    avoidorphans = doExpandradius() # Expand distance searched, when a facility has no census block centroid within selected buffer distance

    system.time(res <- getrelevantCensusBlocksviaQuadTree(dataLocDT,cutoff,maxcuttoff,get_unique,avoidorphans))

    system.time(dat <- doaggregate(dataLocDT,res))

    return(dat)
  })
  # End of functions used for the dataLocList option




  ####################################################################################################################### #
  # ** -----FACILITY LIST IS USED FOR BUFFERING** #######
  # imported facility list
  ####################################################################################################################### #
  dataFacList <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    input$goButton3
    isolate(data.table::as.data.table(read.table(file=inFile$datapath, sep=',', header=TRUE, quote='"')))
  }
  )
  output$inFacList <- renderTable(
    {
      if(is.null(dataFacList())) {return () }
      dataFacList()
    }
  )
  ####################################################################################### #
  #  Find nearby blocks and aggregate for buffer - via quadtree and doaggregate  # *********** ########
  # IT GETS NEARBY BLOCKS AND AGGREGATES EJSCREEN DATA FROM THE BLOCKS NEAR EACH POINT
  ####################################################################################### #
  dataFacListProcessed <- reactive({
    kimssampledata <- dataFacList()
    kimssamplefacilities <-data.table::as.data.table(merge(x = kimssampledata, y = facilities, by.x='REGISTRY_ID', by.y='REGISTRY_ID', all.x=TRUE))
    kimsunique <- data.table::as.data.table(unique(kimssamplefacilities[,.(REGISTRY_ID,LAT,LONG)]))

    rm(kimssampledata)
    rm(kimssamplefacilities)

    kimsunique$ID<- c(seq.int(nrow(kimsunique)))
    kimsunique<-as.data.table(unique(kimsunique[,.(ID,LAT,LONG)]))

    cutoff=getCutoff()
    maxcuttoff=getMaxcutoff()
    get_unique=setUnique()
    avoidorphans=doExpandradius()

    system.time(res <- getrelevantCensusBlocksviaQuadTree(kimsunique,cutoff,maxcuttoff,get_unique,avoidorphans))

    system.time(dat <- doaggregate(kimsunique, res))

    return(dat)
  })
  # End of functions used for the dataFacList option #######3
  ####################################################################################################################### #





  ####################################################################################################################### #
  # ** -----NAICS LIST IS USED FOR BUFFERING**  #######
  ####################################################################################################################### #

  # function that runs buffering on facilities selected via the NAICS dataset,
  # is defined here, not easily in separate file because it uses several reactives and facilities? which is in global env

  datasetNAICS <- function() {
    # parameters needed:  
    # input$selectIndustry1, input$selectIndustry2, getCutoff(), getMaxcutoff(), doExpandraduis(), input$selectNaicsDS1, 
    # 
    sub2 <- data.table::data.table(a = numeric(0), b = character(0))

    print(paste("Number of records in empty data table ",nrow(sub2)))

    if (nchar(input$selectIndustry1)>0 & length(input$selectIndustry2)>0) {
      return()
    }

    cutoff=getCutoff()  # reactive
    maxcuttoff=getMaxcutoff()  # reactive, max distance to search
    get_unique=setUnique() # reactive, TRUE = stats are for dissolved single buffer to avoid doublecounting. FALSE = we want to count each person once for each site they are near.
    avoidorphans=doExpandradius() # Expand distance searched, when a facility has no census block centroid within selected buffer distance

    # which datasystems are we searching?
    selectNaicsDS1 = input$selectNaicsDS1
    selectNaicsDS2 = input$selectNaicsDS2
    inNAICS1 = input$selectIndustry1
    inputnaics1 <- as.list(strsplit(inNAICS1, ",")[[1]])
    inNAICS2=input$selectIndustry2

    if (nchar(inNAICS1)>0 | length(inNAICS2)>0) {

      selectNaicsDS1 = c('OIL','AIRS/AFS')
      selectNaicsDS2 = c('RCRAINFO')
      nrow(selectNaicsDS1)

      inputnaics1 <- as.list(strsplit(inNAICS1, ",")[[1]])
      inputnaics <- input$selectIndustry2
      inputnaics=c(inputnaics1,inNAICS2)
      inputnaics=unique(inputnaics[inputnaics != ""])
      mytest <- facilities
      mytest$cnaics <- as.character(facilities$NAICS)
      x <- paste("^",inputnaics,collapse="|")
      y <- stringr::str_replace_all(string=x, pattern=" ", repl="")
      matches <- unique (grep(y, mytest$cnaics, value=TRUE))

      if (length(selectNaicsDS1)>0 & length(selectNaicsDS2)>0) {
        temp<-mytest[PROGRAM %in% selectNaicsDS1]
        temp<-temp[cnaics %in% matches]
        temp<-unique(temp[,.(REGISTRY_ID)])
        sub1 <-data.table::as.data.table(merge(x = mytest, y = temp, by.x='REGISTRY_ID', by.y='REGISTRY_ID'), all.y=TRUE)
        sub2<-sub1[PROGRAM %in% selectNaicsDS2]
        sub2$ID<- c(seq.int(nrow(sub2)))
      }
      else if (length(selectNaicsDS2)>0) {
        sub2<-mytest[PROGRAM %in% selectNaicsDS2]
        sub2$ID<- c(seq.int(nrow(sub2)))
      }
      else if (length(selectNaicsDS1)>0) {
        sub2<-mytest[cnaics %in% matches]
        sub2$ID<- c(seq.int(nrow(sub2)))
        colnames(sub2)
      }

      ####################################################################################### #
      #  Find nearby blocks and aggregate for buffer - via quadtree and doaggregate  # *********** ########
      # IT GETS NEARBY BLOCKS AND AGGREGATES EJSCREEN DATA FROM THE BLOCKS NEAR EACH POINT
      ####################################################################################### #
      # Run the results through the back end algorithm

      print(paste("Number of matches = ", nrow(sub2)))
      if (nrow(sub2)>0) {

        system.time(res <- getrelevantCensusBlocksviaQuadTree(sub2,cutoff,maxcuttoff,get_unique,avoidorphans))

        system.time(dat <- doaggregate(sub2,res))

        return(dat)
      }
      else {
        print("No matches were found")
        dat <-data.table::as.data.table(setNames(data.frame(matrix(ncol = 1, nrow = 0)),paste0("No matches were found"#, c(1:1)
        )))
        return(dat)
      }
    }
    else {
      print("Please submit an industry to run this query")
      dat <-data.table::as.data.table(setNames(data.frame(matrix(ncol = 1, nrow = 0)),paste0("Please submit an industry to run this query", c(1:1))))
      return(dat)
    }
  }


  # ###########################################'
  #### GET USER INFO AS METADATA for DOWNLOAD #########
  # ###########################################'
  # Support function for grabbing the user input that will be output as meta data in the data download
  addUserInput <- function(file,userin,mystring,strdesc){
    length_string = 0
    if(!is.null(mystring)) {
      length_string=nchar(mystring)
      userin=paste(mystring, userin,sep = "\n")
      userin=paste(strdesc, userin, " ")
      cat(userin,  file=file,append=T)
    }
    #if(!is.null(mystring)) {

    #if (!is.null(mystring) & nchar(mystring)>0)
    #{
    #  userin=paste(mystring, userin,sep = "\n")
    #    userin=paste(strdesc, userin, " ")
    #    cat(userin,  file=file,append=T)
    #}
    return(userin)
  }

  # DOWNLOAD RESULTS #######
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste0("ej-", Sys.Date(), "-",Sys.time(), ".txt",sep='')
    },
    content = function(file) {
      cat('\nTRYING TO DOWNLOAD downloadData1 as ', paste0("ej-", Sys.Date(), "-",Sys.time(), ".txt",sep=''), '\n\n')


      #write.csv(datasetResults(), file)
      #must go back to version control and add this back in   ????????????


      userin=""

      selectNaicsDS1=paste(input$selectNaicsDS1,collapse=", ")
      selectNaicsDS2=paste(input$selectNaicsDS2,collapse=", ")
      industryList=paste(input$selectIndustry1,collapse=", ")
      industryList=paste(industryList,input$selectIndustry2,collapse=", ")

      #"Individual facility statistics"

      definedOutput1=""
      definedOutput2=""
      if(!is.null(input$uniqueOutput)) {
        if (nchar(input$uniqueOutput)>1){
          if(input$uniqueOutput=="yes"){
            definedOutput1="Output for aggregating population statistics"
          }
          else {
            definedOutput2="Individual facility statistics"
          }
        }
      }

      f1=""
      if(!is.null(input$file1)) {
        f1=input$file1
        f1=f1[0]
      }

      f2=""
      if(!is.null(input$file2)) {
        f2=input$file2
        f2=f2[0]
      }

      #addUserInput <- function(file,userin,mystring,strdesc){
      userin=addUserInput(file,userin,definedOutput1,"Defined output: ")
      userin=addUserInput(file,userin,definedOutput2,"Defined output: ")
      userin=addUserInput(file,userin,input$expandRadius,"Expand distance for facilities with no census block centroid within selected buffer distance: ")
      userin=addUserInput(file,userin,as.character(getCutoff()),"Define Buffer Distance (in miles?): ") #as.character(getCutoff())
      userin=addUserInput(file,userin,selectNaicsDS2,"Include facilities with records in: ")
      userin=addUserInput(file,userin,selectNaicsDS1,"Match your NAICS code selection with: ")
      userin=addUserInput(file,userin,industryList,"Industry/Industries: ")
      userin=addUserInput(file,userin,f1,"Upload list of FRS. Filename: ") #input$file1
      userin=addUserInput(file,userin,f2,"Upload list of locations with coordinates. Filename: ") #input$file2

      cat(userin,  file=file)

      # write file of results ####
      write.table(datasetResults(), file, append = T, sep=",")

      cat('\n\n Wrote to ', file)

      #session$reload()

    }
  )

  datasetResults <- function(){
    if (length(getWarning1()>1) | length(getWarning2()>1)) {
      return()
    }
    else if (nchar(input$selectIndustry1)>0 | length(input$selectIndustry2)>0) {
      return(datasetNAICS( ))
    }
    else if (length(myfile2())>1) {
      return(dataLocationListProcessed())
    }
    else if (length(myfile1())>1) {
      return(dataFacListProcessed())
    }
  }

  output$inNAICSresult <- renderTable(
    {
      if(is.null(datasetNAICS( ))) {return () }
      datasetNAICS( )
    }
  )

})
