datasetNAICS <- function(selectIndustry1, selectIndustry2, cutoff, maxcuttoff=50, get_unique=TRUE, avoidorphans=TRUE, selectNaicsDS1, selectNaicsDS2) {   # * OVERALL: Pick NAICS + get BUFFER Report ###############
  # This is part of the Shiny app
  # It should help select facilities from the FRS using NAICS

  sub2 <- data.table::data.table(a = numeric(0), b = character(0))

  print(paste("Number of records in empty data table ",nrow(sub2)))

  if (nchar(input$selectIndustry1)>0 & length(input$selectIndustry2)>0) {
    return()
  }

  # cutoff=getCutoff()
  # maxcuttoff=getMaxcutoff()
  # get_unique=setUnique()
  # avoidorphans=doExpandradius()

  # which datasystems are we searching?
  selectNaicsDS1 = input$selectNaicsDS1
  selectNaicsDS2 = input$selectNaicsDS2
  inNAICS1 = input$selectIndustry1
  inNAICS2=input$selectIndustry2

  inputnaics1 <- as.list(strsplit(inNAICS1, ",")[[1]])

  if (nchar(inNAICS1)>0 | length(inNAICS2)>0) {

    selectNaicsDS1 = c('OIL','AIRS/AFS')
    selectNaicsDS2 = c('RCRAINFO')
    nrow(selectNaicsDS1)

    inputnaics1 <- as.list(strsplit(inNAICS1, ",")[[1]])
    inputnaics <- input$selectIndustry2
    inputnaics=c(inputnaics1,inNAICS2)
    inputnaics=unique(inputnaics[inputnaics != ""])

    mytest <- bufferfast::facilities  # THIS IS DATA IN THE PACKAGE
    # > names(facilities)
    # [1] "X"                      "REGISTRY_ID"            "FAC_NAME"               "FAC_STREET"             "FAC_CITY"               "FAC_STATE"              "FAC_ZIP"
    # [8] "FAC_COUNTY"             "FAC_FIPS_CODE"          "FAC_EPA_REGION"         "FAC_INDIAN_CNTRY_FLG"   "FAC_FEDERAL_FLG"        "FAC_US_MEX_BORDER_FLG"  "FAC_CHESAPEAKE_BAY_FLG"
    # [15] "FAC_NAA_FLAG"           "FAC_LAT"                "FAC_LONG"               "FACT_LAT_RAD"           "FACT_LONG_RAD"          "FAC_X"                  "FAC_Y"
    # [22] "FAC_Z"

    mytest$cnaics <- as.character(bufferfast::facilities$NAICS)
    x <- paste("^",inputnaics,collapse="|")
    y <- stringr::str_replace_all(string=x, pattern=" ", replacement = "")
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

    # Run the results through the back end algorithm

    print(paste("Number of matches = ", nrow(sub2)))
    if (nrow(sub2)>0) {

      system.time(res <- getrelevantCensusBlocksviaQuadTree(sub2,cutoff,maxcuttoff,get_unique,avoidorphans))

      system.time(dat <- doaggregate(sub2,res))

      return(dat)
    } else {
      print("No matches were found")
      dat <-data.table::as.data.table(setNames(data.frame(matrix(ncol = 1, nrow = 0)),paste0("No matches were found"#, c(1:1)
      )))
      return(dat)
    }

  } else {
    print("Please submit an industry to run this query")
    dat <-data.table::as.data.table(setNames(data.frame(matrix(ncol = 1, nrow = 0)), paste0("Please submit an industry to run this query", c(1:1))))
    return(dat)
  }
}
