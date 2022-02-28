# global.R defines variables needed in global environment
# init.getdata(TRUE)  #


######################################################################################################## #
############  Things needed even if not using Shiny app, to use the package at all:############
######################################################################################################## #

############ NOTE ON LOADING PACKAGES ############

# DO I NEED TO LOAD SOME OR ALL PACKAGES HERE?
# Should not load packages using library or require in a package, but specify in DESCRIPTION file.
# but need to in a Shiny app.

# pkgs <- list()

library(EJAM) # This package's functions and data (block points, blockgroup indicators, facility points, NAICS, etc.)
library(foreach) # main reason for using foreach::foreach() is that it supports parallel execution, that is, it can execute those repeated operations on multiple processors/cores on your computer (and there are other advantages as well)
library(sp) # https://cran.r-project.org/web/packages/sp/vignettes/over.pdf
library(SearchTrees)# efficient storage of block points info and selection of those within a certain distance
library(data.table)  # faster than data.frame
library(pdist)
library(blockdata)
library(frsdata)

# library(doSNOW) ; library(foreach)  # parallel processing, efficient looping?
# library(rgdal) ; library(maps) ; library(pdist) #?  # Geospatial tools
# library(RMySQL) # only if loading data from SQL or using SQL for buffering as was tested in an alternative to getrelevantCensusBlocks...

######################################################################################################## #
############  Things needed only if running this Shiny app:############
######################################################################################################## #

library(shiny)
library(frsdata)
s_options <- EJAM::NAICS  # lazy loaded from this package as data, used in ui.R
s_dropdown_naics <- c()
options(shiny.maxRequestSize = 9*1024^2)
server <- "127.0.0.1"
# setwd("~")


############ PARAMETERS SPECIFIC TO USER OR SERVER ############

# these probably should get passed as user-specified parameters to functions with default values, rather than putting them in a script
# CountCPU <- 2
CountCPU <- parallel::detectCores() # this only matters if using getrelevantCensusBlocksviaQuadTree_Clustered 
indexgridsize <- 10  # need to confirm if and how this grid is actually used
translate_fieldnames <- TRUE # may depend on dataset - this is about whether to rename the columns to friendlier variable names in init.getdata.R

maxcutoff_default <- 4000 # 4000 miles seems excessive, so just check if it matters for performance. 

############ GET DATASETS? in package ############
# 
# For names of the data files needed and their colnames, see
# EJAM-package.R via help(package='EJAM')

############ CONSTANTS ############

earthRadius_miles <- 3959 # hard coded this elsewhere but not everywhere

crd <- function(x){
  return( 2*sin(x/2) )
}

#### calculate DEMOGRAPHIC INDEX for US overall, needed later #####
#
# As seen in the lookup table:
# National_Demographic_Index <- EJAM::usastats[EJAM::usastats$PCTILE == 'mean', 'VULEOPCT']
# EJSCREEN2019 (ACS2013-2017) value is  0.3588634
#
# As Calculated from the full latest dataset:
# This is a close enough approximation (see examples below) of more careful method where povknownratio is denominator for pctlowinc:

National_Demographic_Index <- (weighted.mean(EJAM::blockgroupstats$pctmin, EJAM::blockgroupstats$pop, na.rm = TRUE) +
                                 weighted.mean(EJAM::blockgroupstats$pctlowinc, EJAM::blockgroupstats$pop, na.rm = TRUE) ) / 2
# National_Demographic_Index <- (weighted.mean(EJAM::blockgroupstats$pctmin, EJAM::blockgroupstats$pop, na.rm = TRUE) +
#                                  weighted.mean(EJAM::blockgroupstats$pctlowinc, EJAM::blockgroupstats$pop, na.rm = TRUE) ) / 2

# cat(
#   '\n CALCULATED DEMOG US INDEX AVG: \n National_Demographic_Index <- usastats[usastats$PCTILE == \'mean\',
#   \'VULEOPCT\'] # 2016 value is  0.3588634 \n\n'
# )




# comparing ways of calculating national overall avg of us pctlowinc and us pctmin:
#
#  pctlowinc.us.popwtd <- weighted.mean(blockgroupstats2020$pctlowinc, blockgroupstats2020$pop, na.rm = TRUE)
#  pctmin.us.popwtd    <- weighted.mean(blockgroupstats2020$pctmin,    blockgroupstats2020$pop, na.rm = TRUE)
#  pctlowinc.us.careful <- sum(blockgroupstats2020$lowinc, na.rm = TRUE) / sum(blockgroupstats2020$povknownratio, na.rm = TRUE)
#  pctmin.us.careful    <- sum(blockgroupstats2020$mins,   na.rm = TRUE) / sum(blockgroupstats2020$pop,           na.rm = TRUE)
#
#  vsi.us.popwtd.vsi <- weighted.mean(blockgroupstats2020$VSI.eo, blockgroupstats2020$pop, na.rm = TRUE)
#  vsi.us.popwtd.each <- mean(c( pctlowinc.us.popwtd,  pctmin.us.popwtd),  na.rm = TRUE)
#  vsi.us.careful     <- mean(c( pctlowinc.us.careful, pctmin.us.careful), na.rm = TRUE)
#
#  rbind(pctlowinc.us.careful, pctlowinc.us.popwtd, pctmin.us.careful,pctmin.us.popwtd)
# #[,1]  # these differ only slightly for pctlowinc, and not at all for pctmin:
# # pctlowinc.us.careful 0.3233540  # **This is what Census would report as the US overall %lowinc
# # pctlowinc.us.popwtd  0.3247405  # **This is what EJSCREEN effectively uses, since % in bg, once calculated, is treated as a % of pop.
# # pctmin.us.careful    0.3956014 # already popwtd
# # pctmin.us.popwtd     0.3956014 # already popwtd
#
# rbind(vsi.us.careful, vsi.us.popwtd.each, vsi.us.popwtd.vsi)
# # [,1]
# # vsi.us.careful     0.359 477   # ** only slighly different -- mean of the two US values that are exact overall US % of correct denominator
# # vsi.us.popwtd.each 0.360 171   # mean of the two US values that are popwtd means of bgs
# # vsi.us.popwtd.vsi  0.360 142   # mean of popwtd means is almost same as popwtd mean of means



 # if (exists(usastats)) {
#   # To calculate this constant that is needed later:
#   National_Demographic_Index <- usastats[usastats$PCTILE == 'mean', 'VULEOPCT'] # 2016 value is  0.3588634
#   cat(
#     '\n CALCULATED DEMOG US INDEX AVG: \n National_Demographic_Index <- usastats[usastats$PCTILE == \'mean\',
#   \'VULEOPCT\'] # 2016 value is  0.3588634 \n\n'
#   )
# } else {
#   # approximation while testing
#   National_Demographic_Index <- 0.36
# }
