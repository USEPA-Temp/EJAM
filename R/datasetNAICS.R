#' clean up selected NAICS facilities by NAICS industrial sector code 
#' 
#'  NOTE:   A VERSION OF THIS IS WITHIN server.R 
#'  and this was work in progress to possibly move it to a separate function
#'  but that would require passing all these tables as parameters 
#'  or assuming they are in memory. 
#' \preformatted{
#'     To pass all the reactives as parameters, you would do this: 
#'     
#'  selectIndustry1=input$selectIndustry1, 
#'  selectIndustry2=input$selectIndustry2,
#'  cutoff=getCutoff(), 
#'  maxcutoff=getMaxcutoff(), 
#'  get_unique=TRUE, 
#'  avoidorphans=TRUE,
#'  doExpandradius=doExpandradius(), 
#'  selectNaicsDS1= input$selectNaicsDS1, 
#'  selectNaicsDS2 =input$selectNaicsDS2)
#'  }
#'  
#' @param selectIndustry1 reactive from shiny input$selectIndustry1
#' @param selectIndustry2 reactive from shiny input$selectIndustry2
#' @param cutoff  getCutoff()
#' @param maxcutoff  getMaxcutoff() 
#' @param get_unique default is FALSE now but was TRUE, but likely to get rid of this?! 
#' @param avoidorphans default TRUE
#' @param doExpandradius obsolete?
#' @param selectNaicsDS1 was from shiny app input$selectNaicsDS1
#' @param selectNaicsDS2  was from shiny app input$selectNaicsDS1
#'
#' @export
#'
datasetNAICS <- function(selectIndustry1, selectIndustry2, 
                         cutoff, maxcutoff=50, get_unique=FALSE, 
                         avoidorphans=TRUE, doExpandradius=NULL,
                         selectNaicsDS1, selectNaicsDS2) {   
  
  ################################################################## #
  # Prep full FRS that has NAICS of all sites and their lat lon ####
  # Dataset of FRS sites and NAICS in long format (used to be facdata.rdata)
  ################################################################## #
  
  mytest <- frsdata::frs_naics_2016 # frsdata::facilities
  mytest$cnaics <- as.character(mytest$NAICS)
  
  sub2 <- data.table::data.table(a = numeric(0), b = character(0))
  
  ################################################################## #
  # CLEAN UP USER'S NAICS SELECTIONS ? ####
  ################################################################## #
  
  if (nchar(input$selectIndustry1)>0 & length(input$selectIndustry2)>0) {
    return()
  }
  # cutoff=getCutoff()
  # maxcutoff=getMaxcutoff()
  # get_unique=setUnique()
  # avoidorphans=     ???  doExpandradius()
  
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
    x <- paste("^",inputnaics,collapse="|")
    y <- stringr::str_replace_all(string=x, pattern=" ", replacement = "")
    
    ################################################################## #
    # MATCH USER NAICS TO NAICS IN FRS DATASET TO GET LAT/LON OF MATCHED SITES ####
    ################################################################## #
    
    matches <- unique(grep(y, mytest$cnaics, value=TRUE))
    
    if (length(selectNaicsDS1)>0 & length(selectNaicsDS2)>0) {
      temp <- mytest[PROGRAM %in% selectNaicsDS1]
      temp <- temp[cnaics %in% matches]
      temp <- unique(temp[,.(REGISTRY_ID)])
      sub1 <- data.table::as.data.table(merge(x = mytest, y = temp, by.x='REGISTRY_ID', by.y='REGISTRY_ID'), all.y=TRUE)
      sub2 <- sub1[PROGRAM %in% selectNaicsDS2]
      sub2$ID <- c(seq.int(nrow(sub2)))
    }
    else if (length(selectNaicsDS2)>0) {
      sub2 < -mytest[PROGRAM %in% selectNaicsDS2]
      sub2$ID <- c(seq.int(nrow(sub2)))
    }
    else if (length(selectNaicsDS1)>0) {
      sub2 <- mytest[cnaics %in% matches]
      sub2$ID <- c(seq.int(nrow(sub2)))
      colnames(sub2)
    }
    print(paste("Number of FRS facility matches for specified NAICS = ", nrow(sub2)))
    
    
    
    
    
    ################################################################## #
    # CALL FUNCTIONS DOING DISTANCES (BUFFERS) AND AGGREGATION  ######
    ################################################################## #
    
    if (nrow(sub2)>0) {
      
      system.time({
        res <- getrelevantCensusBlocksviaQuadTree(
          sitepoints = sub2, cutoff, maxcutoff, get_unique, avoidorphans
          )
        })
      system.time({
        dat <- doaggregate(sub2, res)
        })
      
      return(dat)
    } else {
      print("No matches were found")
      dat <- data.table::as.data.table(setNames(data.frame(matrix(ncol = 1, nrow = 0)),paste0("No matches were found"#, c(1:1)
      )))
      return(dat)
    }
    
  } else {
    print("Please submit an industry to run this query")
    dat <- data.table::as.data.table(setNames(data.frame(matrix(ncol = 1, nrow = 0)), paste0("Please submit an industry to run this query", c(1:1))))
    return(dat)
  }
}
