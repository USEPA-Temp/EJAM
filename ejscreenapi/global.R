################################ Specify defaults, parameters for app ####

ips <- c('10.147.194.116', 'awsgeopub.epa.gov', '204.47.252.51', 'ejscreen.epa.gov')
whichip <- ips[2]
apptitle <- "EJScreen interim circular buffer batch tool"
maxpts <- 320
maxradius <- 5  # for slider input where user specifies radius
minradius <- 0.5
stepradius <- 0.5 
perhourslow <- 3000  # to give an estimate of how long it will take
perhourguess <- 6000 # seeing 8k if 1 mile, 4.7k if 5 miles, roughly. 207 ECHO run was 2.1 minutes, 5.9k/hr.
perhourfast <- 12000
meters_per_mile <- 1609.34
# to show some sample points at app startup:
default_points_shown_at_startup <- structure(
  list(siteid = c(1, 2), 
       sitename = c("example site 1", "example site 2"), 
       lon = c(-91.132107, -91.09), lat = c(30.494982, 30.45)), 
  row.names = c(NA, -2L), class = "data.frame")
echo_url <-  'https://echo.epa.gov/facilities/facility-search'
echo_message <- paste0('To use the ECHO website to search for and specify a list of regulated facilities, 
                       1) go to ', echo_url, ' and 2) under Facility Characteristics Results View select data table, 
                       click Search, then 3) click Customize Columns, use checkboxes to include Latitude and Longitude, 
                       then 4) click Download Data, then 5) return to this app to upload that ECHO site list.\n')
 # enableBookmarking(store = "url")

# could be stored as data in a package or sourced, but this is ok for now
map_names <- read.csv('map_headernames.csv')

################################ Get packages, functions #### 

library(shiny)
library(DT) # used in ui.R and in server.R for displaying tabular data
# library(htmltools) # probably not needed... imported by shiny? provides tags like h5() etc
library(readr) # used in server.R
require(leaflet) # ; require(mapview) # for mapping. leaflet is used in ui.R and in ejscreenapi.R
# require(sf)
require(data.table) # used in ejscreenapi1.R, ejscreenapi.R
require(jsonlite)  # mostly for using API 
require(magrittr) # for the pipe used with leaflet in server.R,   %>%  
require(httr) # used by ejscreenRESTbroker() for httr::GET()
library(htmltools) # probably not needed... imported by shiny? provides tags like h5() etc
# library(urltools) # probably want to switch to using this nice package for working with URL-encoded parameters, encoding, parsing, etc. URLs

# These are in R folder so they get sourced just by running the app, as if you did
# for (i in 1:length(list.files('./R/'))) {source(file = paste('./R/', list.files('./R/')[i],sep = ''))}

# ------------------------ Notes ------------------------ 
#
#  How to launch the app ####
# 
##  To use version that is hosted on a staging server (not yet working as of 4/27/22, DNS change needed)
#   browseURL('https://rstudio-connect.dmap-stage.aws.epa.gov/content/374e6403-c660-4692-b62f-61139d1fef69/')
# 
##  To run the app via the copy that is on github 
#    using R but without having to separately download/install anything: 
# 
#   shiny::runUrl('https://github.com/USEPA-Temp/EJAM/ejscreenapi.zip')
# 
##  To run the app if already downloaded/ cloned 
#   runApp()  
# 
## To view and use test data  ####
#   See the folder called "inst" within the ejscreenapi downloaded source code/app
# 
# 
## To get create test data  ####
#   n=101
#   x <- data.frame(siteid=1:n, sitename=paste('site', 1:n), lat=runif(n,34.4,36.4), lon=runif(n,-119,-72), stringsAsFactors = FALSE)
#   ## x <- data.frame(siteid=1:n, sitename=paste('site', 1:n), proxistat::testpoints_bg20(n), stringsAsFactors = FALSE)
#   write.csv(x, file = paste0('testpoints_',n,'.csv'), row.names = FALSE)
# 
# 
# ------------------ Help / documentation ------------------------ 

### Shiny, hosting ####
# 
# https://mastering-shiny.org/scaling-packaging.html
# Hosting shiny apps 
# https://docs.rstudio.com/shinyapps.io/index.html
# rsconnect::deployApp() # to deploy the app
# browseURL('https://rstudio-connect.dmap-stage.aws.epa.gov/content/374e6403-c660-4692-b62f-61139d1fef69/')
# 
### EJScreen report API ####
# For reports on a buffered circle (data or pdf-like report:)
# browseURL(https://www.epa.gov/ejscreen/ejscreen-api)
# https://ejscreen.epa.gov/mapper/EJAPIinstructions.pdf
#  public access and internal IPs differ, it seems, and 
#  this shiny app on RStudio Connect server staging vs production version of app may have differing ability to resolve via DNS and reach different IPs 
# For variable names and definitions
#  https://edg.epa.gov/metadata/rest/document?id=%7B795F81AB-1908-4CAA-9366-7F4B6E60136B%7D 
# 
### EJScreen REST services (data, maps) - NOT USED HERE AS OF 4/22 ####
# 
# For EJScreen map services, datasets, etc. 
#  https://ejscreen.epa.gov/arcgis/rest/services/ejscreen 
# https://ejscreen.epa.gov/arcgis/sdk/rest/index.html#//02ss0000000r000000
# https://edg.epa.gov/metadata/rest/document?id=%7B795F81AB-1908-4CAA-9366-7F4B6E60136B%7D
# 
### Server-related ####
#
# https://shiny.rstudio.com/articles/upload.html #https://mastering-shiny.org/action-transfer.html
# https://shiny.rstudio.com/articles/action-buttons.html
# https://shiny.rstudio.com/articles/modal-dialogs.html
# https://shiny.rstudio.com/articles/datatables.html
# https://shiny.rstudio.com/articles/download.html
# 
### UI-related (data tables and layouts etc.) ####
# 
# https://shiny.rstudio.com/articles/datatables.html
# https://shiny.rstudio.com/articles/layout-guide.html
# https://shiny.rstudio.com/articles/#user-interface
# https://shiny.rstudio.com/articles/download.html
#
# # notes on datatable options to use: hover, compact, order-columns, stripe?
# note that user can click a column name to sort on that column, then shift click on 2d column to sort on the 2d one within each unique value of the 1st column 

### Maps ####
# 
# https://rstudio.github.io/leaflet/shiny.html  
# https://towardsdatascience.com/create-interactive-map-applications-in-r-and-r-shiny-for-exploring-geospatial-data-96b0f9692f0f
# https://walker-data.com/census-r/mapping-census-data-with-r.html#interactive-mapping-with-leaflet
# https://rdrr.io/cran/mapview/
# https://github.com/r-spatial/mapview
# Census data and maps
# http://walker-data.com/umich-workshop/census-data-in-r/slides/#9  
# 
############################## #
