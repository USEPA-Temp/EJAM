if (FALSE) {
# 
# clean up selected NAICS facilities by NAICS industrial sector code
# 
#  NOTE:   A VERSION OF THIS IS WITHIN server.R
#  and this was work in progress to possibly move it to a separate function
#  but that would require passing all these tables as parameters
#  or assuming they are in memory.
# \preformatted{
#     To pass all the reactives as parameters, you would do this:
# 
#  selectIndustry1_byNAICS=input$selectIndustry1_byNAICS,
#  selectIndustry2_by_selectInput=input$selectIndustry2_by_selectInput,
#  cutoff=getCutoff(),
#  maxcutoff=getMaxcutoff(),
#  get_unique=TRUE,
#  avoidorphans=TRUE,
#  doExpandradius=doExpandradius(),
#  selectNaics_in_Datasystem1= input$selectNaics_in_Datasystem1,
#  selectNaics_and_Datasystem2 =input$selectNaics_and_Datasystem2)
#  }
# 
# @param selectIndustry1_byNAICS reactive from shiny input$selectIndustry1_byNAICS
# @param selectIndustry2_by_selectInput reactive from shiny input$selectIndustry2_by_selectInput
# @param cutoff  getCutoff()
# @param maxcutoff  getMaxcutoff()
# @param get_unique default is FALSE now but was TRUE, but likely to get rid of this?!
# @param avoidorphans default TRUE
# @param doExpandradius obsolete?
# @param selectNaics_in_Datasystem1 was from shiny app input$selectNaics_in_Datasystem1
# @param selectNaics_and_Datasystem2  was from shiny app input$selectNaics_in_Datasystem1

##################################  @export

datasetNAICS <- function(selectIndustry1_byNAICS, selectIndustry2_by_selectInput, 
                         cutoff, maxcutoff=50, get_unique=FALSE, 
                         avoidorphans=TRUE, doExpandradius=NULL,
                         selectNaics_in_Datasystem1, selectNaics_and_Datasystem2) {   
  
  ################################################################## #
  # Prep full FRS that has NAICS of all sites and their lat lon ####
  # Dataset of FRS sites and NAICS in long format (used to be facdata.rdata)
  ################################################################## #
  
  mytest <- frsdata::frs_naics_2016 # frsdata::facilities
  mytest$cnaics <- as.character(mytest$NAICS) # was stored as factor w/ 22 levels
  
  sub2 <- data.table::data.table(a = numeric(0), b = character(0))
  
  ################################################################## #
  # CLEAN UP USER'S NAICS SELECTIONS ? ####
  ################################################################## #
  
  if (nchar(input$selectIndustry1_byNAICS)>0 & length(input$selectIndustry2_by_selectInput)>0) {
    return()
  }
  # cutoff=getCutoff() # reactive
  # maxcutoff=getMaxcutoff() # reactive
  # get_unique=setUnique() # reactive
  # avoidorphans=     ???  doExpandradius()  # reactive
  
  # which datasystems are we searching?
  selectNaics_in_Datasystem1 = input$selectNaics_in_Datasystem1
  selectNaics_and_Datasystem2 = input$selectNaics_and_Datasystem2
  inNAICS1 = input$selectIndustry1_byNAICS
  inNAICS2 = input$selectIndustry2_by_selectInput
  
  inputnaics1 <- as.list(strsplit(inNAICS1, ",")[[1]])
  
  if (nchar(inNAICS1)>0 | length(inNAICS2)>0) {
    
    selectNaics_in_Datasystem1 = c('OIL','AIRS/AFS')
    selectNaics_and_Datasystem2 = c('RCRAINFO')
    nrow(selectNaics_in_Datasystem1)
    
    inputnaics1 <- as.list(strsplit(inNAICS1, ",")[[1]])
    inputnaics <- input$selectIndustry2_by_selectInput
    inputnaics=c(inputnaics1,inNAICS2)
    inputnaics=unique(inputnaics[inputnaics != ""])
    x <- paste("^",inputnaics,collapse="|")
    y <- stringr::str_replace_all(string=x, pattern=" ", replacement = "")
    
    ################################################################## #
    # MATCH USER NAICS TO NAICS IN FRS DATASET TO GET LAT/LON OF MATCHED SITES ####
    ################################################################## #
    
    matches <- unique(grep(y, mytest$cnaics, value=TRUE))
    
    if (length(selectNaics_in_Datasystem1)>0 & length(selectNaics_and_Datasystem2)>0) {
      temp <- mytest[PROGRAM %in% selectNaics_in_Datasystem1]
      temp <- temp[cnaics %in% matches]
      temp <- unique(temp[,.(REGISTRY_ID)])
      sub1 <- data.table::as.data.table(merge(x = mytest, y = temp, by.x='REGISTRY_ID', by.y='REGISTRY_ID'), all.y=TRUE)
      sub2 <- sub1[PROGRAM %in% selectNaics_and_Datasystem2]
      sub2$ID <- c(seq.int(nrow(sub2)))
    }
    else if (length(selectNaics_and_Datasystem2)>0) {
      sub2 < -mytest[PROGRAM %in% selectNaics_and_Datasystem2]
      sub2$ID <- c(seq.int(nrow(sub2)))
    }
    else if (length(selectNaics_in_Datasystem1)>0) {
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
}
