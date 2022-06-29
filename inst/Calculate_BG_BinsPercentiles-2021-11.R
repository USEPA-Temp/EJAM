

# THIS IS THE CODE EJSCREEN HAS BEEN USING TO ASSIGN PERCENTILES TO ALL BLOCKGROUPS NATIONWIDE EACH YEAR (not for buffer analysis)
# . it is very old, probably will get replaced to adjust for ties, etc.
# There is 



if (FALSE) {
  ###############################################################################
#
# Rewritten R code to replace 2013 code 
# to assign percentiles and bins to all block groups / places
# FOR EJSCREEN
#
# Mark A. Corrales, 5/2014
###############################################################################
 
#################################
#
# To use this code in R, see 'How to run EJSCREEN R scripts 2014-05.R'
#
#################################


#################################
# SPECIFY NULL / NA / MISSING VALUES CODE
#################################

# As of 5/8/2014, -9999999  is the missing value indicator the contractor & EPA agreed to use.
default.MISSING.VALUE.SYMBOL <- -9999999

#################################
# SPECIFY COLUMNS TO IMPORT & THEIR TYPES
#################################
#
# As of 5/8/2014, these are the field names and their classes, needed for correct import to R from csv:
# copy of actual first line and headers:
#OBJECTID,ID,ACSTOTPOP,ACSIPOVBAS,ACSEDUCBAS,ACSTOTHH,ACSTOTHU,MINORPOP,MINORPCT,LOWINCOME,LOWINCPCT,LESSHS,LESSHSPCT,LINGISO,LINGISOPCT,UNDER5,UNDER5PCT,OVER64,OVER64PCT,PRE1960,PRE1960PCT,VULEOPCT,VULSVI6PCT,VULEO,VULSVI6,DISPEO,DISPSVI6,DSLPM,CANCER,RESP,NEURO,PTRAF,PWDIS,PNPL,PRMP,PTSDF,OZONE,PM25,D_LDPNT_2,LDPNT_D6,LDPNT_B2,LDPNT_B6,LDPNT_P2,LDPNT_P6,D_DSLPM_2,DSLPM_D6,DSLPM_B2,DSLPM_B6,DSLPM_P2,DSLPM_P6,D_CANCR_2,CANCR_D6,CANCR_B2,CANCR_B6,CANCR_P2,CANCR_P6,D_RESP_2,RESP_D6,RESP_B2,RESP_B6,RESP_P2,RESP_P6,D_NEURO_2,NEURO_D6,NEURO_B2,NEURO_B6,NEURO_P2,NEURO_P6,D_PTRAF_2,PTRAF_D6,PTRAF_B2,PTRAF_B6,PTRAF_P2,PTRAF_P6,D_PWDIS_2,PWDIS_D6,PWDIS_B2,PWDIS_B6,PWDIS_P2,PWDIS_P6,D_PNPL_2,PNPL_D6,PNPL_B2,PNPL_B6,PNPL_P2,PNPL_P6,D_PRMP_2,PRMP_D6,PRMP_B2,PRMP_B6,PRMP_P2,PRMP_P6,D_PTSDF_2,PTSDF_D6,PTSDF_B2,PTSDF_B6,PTSDF_P2,PTSDF_P6,D_OZONE_2,OZONE_D6,OZONE_B2,OZONE_B6,OZONE_P2,OZONE_P6,D_PM25_2,PM25_D6,PM25_B2,PM25_B6,PM25_P2,PM25_P6,ST_ABBREV,REGION
#1,010479573004,813,813,446,278,392,763,0.938499385,699,0.859778598,75,0.168161435,0,0,33,0.040590406,102,0.125461255,122,0.31122449,0.899138991,0.35541518,731,288.9525411,446.9887131,141.9592552,0.044492982,39.45929954,1.499406775,0.028588049,23.76317494,0.273887027,0.014282724,0.790443471,0.018397846,41.841424,10.699287,139.1138342,44.18119677,227.505102,89.92910718,0.279834074,0.110613908,19.88786098,6.316190656,32.5243702,12.85636035,0.040005375,0.015813481,17637.86152,5601.612773,28844.74796,11401.86487,35.47939479,14.02443404,670.2179047,212.854669,1096.066352,433.2573977,1.348175095,0.532911928,12.77853524,4.058338147,20.89786384,8.26058941,0.02570463,0.010160627,10621.87099,3373.402616,17370.88088,6866.429784,21.36639715,8.445793092,122.4244096,38.88079833,200.2114166,79.14035236,0.246262505,0.097343607,6.38421654,2.027564899,10.44067144,4.127029471,0.012842154,0.005076297,353.3193097,112.2107664,577.814177,228.4006494,0.710718545,0.280935608,8.223629569,2.611744535,13.44882553,5.316104392,0.016542221,0.006538874,18702.64427,5939.777388,30586.08094,12090.18579,37.62125577,14.87107723,4782.460527,1518.862814,7821.178797,3091.586167,9.620146122,3.802689012,AL,4
# *** Note that FIPS is saved as a number with leading zero, but not quoted, so unless we specify character as class, it will be imported as numeric and lose the leading zero!
# *** Note that reading just 1 row doesn't quite get the field classes correct. VULEO was interpreted as integer but we want it to be numeric. ID which is FIPS is interpreted as numeric but we want to read it as character. State abbreviation can be factor (takes less RAM than character class).
# So the classes are hard coded here, and I can also set some to NULL class to avoid reading them in, for those where I know we don't need to calculate the percentile and bin.
#################################

#################################
# As of 11/19/21, modified to add Unemployment 
#    and UST elements and keep only Primary Indexes
#################################

myclasses <- structure(c(
"integer", "character", 
"integer", "integer", "integer", "integer", "integer", "integer",
"integer", "numeric", 
"integer", "numeric", 
"integer", "numeric", 
"integer", "numeric", 
"integer", "numeric", 
"integer", "numeric", 
"integer", "numeric",
"integer", "numeric",
"numeric", "numeric", "numeric", 
"numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
"numeric",
"numeric",
"numeric",
"numeric",
"numeric",
"numeric",
"numeric",
"numeric",
"numeric",
"numeric",
"numeric",
"numeric",
"factor", "integer"), 
.Names = c(
"OBJECTID", "ID", 
"ACSTOTPOP", "ACSIPOVBAS", "ACSEDUCBAS", "ACSTOTHH", "ACSTOTHU", "ACSUNEMPBAS",
"MINORPOP", "MINORPCT", 
"LOWINCOME", "LOWINCPCT", 
"LESSHS", "LESSHSPCT", 
"LINGISO", "LINGISOPCT", 
"UNDER5", "UNDER5PCT", 
"OVER64", "OVER64PCT", 
"PRE1960", "PRE1960PCT",
"UNEMPLOYED", "UNEMPPCT",
"VULEOPCT", "VULEO", "DISPEO",
"DSLPM", "CANCER", "RESP", "PTRAF", "PWDIS", "PNPL", "PRMP", "PTSDF", "OZONE", "PM25", "UST", 
"D_LDPNT_2",
"D_DSLPM_2",
"D_CANCR_2",
"D_RESP_2", 
"D_PTRAF_2",
"D_PWDIS_2",
"D_PNPL_2", 
"D_PRMP_2", 
"D_PTSDF_2",
"D_OZONE_2",
"D_PM25_2",
"D_UST_2",
"ST_ABBREV", "REGION"))

#################################
# Use NULL as class to designate the fields we don't want to read in from the csv, since we don't need percentile or bin for those.
#################################

# NOTE: 
# We do want to import ID (FIPS CODE), ACSTOTPOP (population which is the weights), and
# We do want to import ST_ABBREV & REGION in case we want to calculate the state and regional percentiles here (e.g. if needed to map them) instead of just using lookup tables to find them on the fly.

myclasses[names(myclasses) %in% c(
  'ACSIPOVBAS', 'ACSEDUCBAS', 'ACSTOTHH', 'ACSTOTHU',
  'MINORPOP', 'LOWINCOME', 'LESSHS', 'LINGISO', 'UNDER5', 'OVER64',
  'PRE1960', 
  'VULEO', 'VULSVI6', 'DISPEO', 'DISPSVI6')] <- 'NULL'

#################################
# That creates the following initial set of classes (before some are set to NULL):

#cbind(myclasses)
#           myclasses  
#OBJECTID   "integer"  
#ID         "character"
#ACSTOTPOP  "integer"  
#ACSIPOVBAS "integer"  
#ACSEDUCBAS "integer"  
#ACSTOTHH   "integer"  
#ACSTOTHU   "integer"  
#MINORPOP   "integer"  
#MINORPCT   "numeric"  
#LOWINCOME  "integer"  
#LOWINCPCT  "numeric"  
#LESSHS     "integer"  
#LESSHSPCT  "numeric"  
#LINGISO    "integer"  
#LINGISOPCT "numeric"  
#UNDER5     "integer"  
#UNDER5PCT  "numeric"  
#OVER64     "integer"  
#OVER64PCT  "numeric"  
#PRE1960    "integer"  
#PRE1960PCT "numeric"  
#VULEOPCT   "numeric"  
#VULSVI6PCT "numeric"  
#VULEO      "numeric"  
#VULSVI6    "numeric"  
#DISPEO     "numeric"  
#DISPSVI6   "numeric"  
#DSLPM      "numeric"  
#CANCER     "numeric"  
#NONCAN     "numeric"  
#PTRAF      "numeric"  
#PWDIS      "numeric"  
#PNPL       "numeric"  
#PRMP       "numeric"  
#PTSDF      "numeric"  
#OZONE      "numeric"  
#PM25       "numeric"  
#D_LDPNT_2  "numeric"  
#LDPNT_D6   "numeric"  
#LDPNT_B2   "numeric"  
#LDPNT_B6   "numeric"  
#LDPNT_P2   "numeric"  
#LDPNT_P6   "numeric"  
#D_DSLPM_2  "numeric"  
#DSLPM_D6   "numeric"  
#DSLPM_B2   "numeric"  
#DSLPM_B6   "numeric"  
#DSLPM_P2   "numeric"  
#DSLPM_P6   "numeric"  
#D_CANCER_2  "numeric"  
#CANCR_D6   "numeric"  
#CANCR_B2   "numeric"  
#CANCR_B6   "numeric"  
#CANCR_P2   "numeric"  
#CANCR_P6   "numeric"  
#D_RESP_2   "numeric"  
#RESP_D6    "numeric"  
#RESP_B2    "numeric"  
#RESP_B6    "numeric"  
#RESP_P2    "numeric"  
#RESP_P6    "numeric"  
#D_PTRAF_2  "numeric"  
#PTRAF_D6   "numeric"  
#PTRAF_B2   "numeric"  
#PTRAF_B6   "numeric"  
#PTRAF_P2   "numeric"  
#PTRAF_P6   "numeric"  
#D_PWDIS_2  "numeric"  
#PWDIS_D6   "numeric"  
#PWDIS_B2   "numeric"  
#PWDIS_B6   "numeric"  
#PWDIS_P2   "numeric"  
#PWDIS_P6   "numeric"  
#D_PNPL_2   "numeric"  
#PNPL_D6    "numeric"  
#PNPL_B2    "numeric"  
#PNPL_B6    "numeric"  
#PNPL_P2    "numeric"  
#PNPL_P6    "numeric"  
#D_PRMP_2   "numeric"  
#PRMP_D6    "numeric"  
#PRMP_B2    "numeric"  
#PRMP_B6    "numeric"  
#PRMP_P2    "numeric"  
#PRMP_P6    "numeric"  
#D_PTSDF_2  "numeric"  
#PTSDF_D6   "numeric"  
#PTSDF_B2   "numeric"  
#PTSDF_B6   "numeric"  
#PTSDF_P2   "numeric"  
#PTSDF_P6   "numeric"  
#D_OZONE_2  "numeric"  
#OZONE_D6   "numeric"  
#OZONE_B2   "numeric"  
#OZONE_B6   "numeric"  
#OZONE_P2   "numeric"  
#OZONE_P6   "numeric"  
#D_PM25_2   "numeric"  
#PM25_D6    "numeric"  
#PM25_B2    "numeric"  
#PM25_B6    "numeric"  
#PM25_P2    "numeric"  
#PM25_P6    "numeric"  
#ST_ABBREV  "factor"   
#REGION     "integer"


###############################################################
#	DEFINE FUNCTION assign.pctiles()
#	FUNCTION TO ASSIGN THE EXACT WEIGHTED PERCENTILE NUMBER TO EACH LOCATION'S RAW INDICATOR SCORE
#	ALSO, THIS GIVES A SIMPLE UNWEIGHTED PERCENTILE IF PASSED NULL FOR THE WEIGHTS PARAMETER (THE DEFAULT IF UNSPECIFIED).
###############################################################

assign.pctiles <- function(values, weights=NULL) {  

  #  FUNCTION TO ASSIGN THE EXACT WEIGHTED PERCENTILE NUMBER TO EACH LOCATION'S RAW INDICATOR SCORE
	
  require(Hmisc)

  wtd.Ecdf.results <- wtd.Ecdf(values, weights, type='i/n', na.rm=TRUE) 

	# If the first CDF estimate is greater than zero, a point (min(x),0) is placed at the beginning of the estimates.
	if (length(wtd.Ecdf.results$x) == 1+ length(unique(values)) ) { wtd.Ecdf.results$x <- wtd.Ecdf.results$x[-1]; wtd.Ecdf.results$ecdf <- wtd.Ecdf.results$ecdf[-1] }

	# This finds the rank of each value
	myindex <- findInterval(values, wtd.Ecdf.results$x) 
	myindex[myindex==0] <- 1	# if for some reason the minimum value doesn't match, which has occurred in testing

	exact.wtd.pctile <- wtd.Ecdf.results$ecdf[ myindex ]
	
	return(exact.wtd.pctile)
}

###############################################################
#	NOTES ON THIS FUNCTION
#
#	For documentation of wtd.Ecdf() from Hmisc package, see 
#	http://127.0.0.1:26624/library/Hmisc/html/wtd.stats.html
#	wtd.Ecdf returns a list whose elements x and Ecdf correspond to unique sorted values of x. 
#
#	Min and max percentiles assigned:
#	Note that if the first CDF estimate is greater than zero, a point (min(x),0) is placed at the beginning of the estimates. [so we remove that since it is just confusing]
#	There will never be exactly 0 assigned as the percentile. 
#	The smallest that may be assigned is 1/n and this is assigned to the min input if it is unique. 
#	If min is not unique, all those N ties for min of n total values are assigned N/n (not 1/n) as their percentile.
#	There highest percentile assigned is exactly 1.00 -- This happens to the maximum value, and for any tied for the maximum.
#
#	TIES: Ties in input values are all assigned the same percentile, the percentile at the upper end of that bin's range
#	so if for example the top 3 values of 100 values are tied, they are all assigned 1.000 as the percentile & the next is assigned 0.97, etc.
#
#	type="i/n" to use the inverse of the empirical distribution function, using, ... wt/T, 
#		where wt is the cumulative weight and T is the total weight (usually total sample size). 
#
#	For documentation on findInterval() function:
#	http://127.0.0.1:26624/library/base/html/findInterval.html 
#
#	**** Note that this assign.pctiles() function, which relies on wtd.Ecdf(), 
#	will ignore NA values, and return NA as the percentile assigned to those, 
#	and the other percentiles assigned are among valid (non-NA) values only.
#	If na.rm=TRUE (default), this version treats NA values in input by excluding them from ranking, 
#	making their percentile NA and finding %iles among all valid values, 
#	so if there were 105 values with 5 being NA and the other 100 being unique numbers,  
#	this function would return 1-100 as the valid %iles and return "NA" as %ile for other 5.
###############################################################


###############################################################
#	DEFINE FUNCTION TO ASSIGN MAP BIN NUMBER 1-11, BASED ON THESE %ILE CUTPOINTS
###############################################################

assign.map.bins <- function(pctiles.vector, cutpoints=c( (0:9)/10, 0.95, 1), labels=1:11) {

  #  FUNCTION TO ASSIGN MAP BIN NUMBER 1-11, BASED ON THESE %ILE CUTPOINTS: defaults are c( (0:9)/10, 0.95, 1)

  # Inputs must be fractions 0-1, not integers 0-100, if default fractional cutpoints are used.
  mylabels <- labels
  
  bin.vector <- cut(pctiles.vector, breaks=cutpoints, labels=mylabels, right=FALSE, include.lowest=TRUE)
	bin.vector <- as.numeric(bin.vector)

	# Put NA values in a bin zero (i.e., where missing data in inputs so percentile is NA)

	bin.vector[is.na(bin.vector)] <- 0
	return(bin.vector)
  
  # The default bins 0-11 are defined as follows:
  # bin 0: PCTILE=NA
  # ...
  # bin 9:  0.80<=PCTILE<0.90
  # bin 10: 0.90<=PCTILE<0.95
  # bin 11: 0.95<=PCTILE<=1.00 
}


###############################################################
#	DEFINE FUNCTION TO ASSIGN PCTILES 
#	CREATING MULTIPLE percentile COLUMNS FOR MULTIPLE RAW DATA COLUMNS
###############################################################

make.pctile.cols <- function(raw.data.frame, weights, as.df=TRUE) {

  #  FUNCTION TO ASSIGN PCTILES CREATING MULTIPLE COLUMNS FOR MULTIPLE RAW DATA COLUMNS
  
  pctile.df <- sapply(raw.data.frame, FUN=function(x) {assign.pctiles(x, weights) })
	colnames(pctile.df) <- paste("pctile.", colnames(pctile.df), sep="")
	if (as.df) {pctile.df <- as.data.frame(pctile.df, stringsAsFactors=FALSE)}
	return(pctile.df)

	# Example of usage:  
	#  new.pctile.cols <- make.pctile.cols(places[ , names.e])
}


###############################################################
#	DEFINE FUNCTION TO ASSIGN PCTILES 
#	CREATING MULTIPLE bin number COLUMNS FOR MULTIPLE percentile COLUMNS
###############################################################

make.bin.cols <- function(pctile.df, as.df=TRUE, cutpoints=c( (0:9)/10, 0.95, 1), labels=1:11) {

  #  FUNCTION TO ASSIGN PCTILES CREATING MULTIPLE bin number COLUMNS FOR MULTIPLE percentile COLUMNS
  
  bin.df <- sapply(pctile.df, FUN=function(x) {assign.map.bins(x, cutpoints, labels) })
	colnames(bin.df) <- paste("bin.", colnames(bin.df), sep="")
	colnames(bin.df) <- gsub('bin.pctile.', 'bin.', colnames(bin.df))  # get rid of pctile. in field names, if created by make.pctile.cols()
	if (as.df) {bin.df <- as.data.frame(bin.df, stringsAsFactors=FALSE)}
	return(bin.df)
	
	# Example of usage:  
	#  new.bin.cols <- make.bin.cols(places[ , names.e.pctile])
	# or 
	#  new.bin.cols <- make.bin.cols( new.pctile.cols ) 
}
##############################################################################################################################


###############################################################
#	DEFINE FUNCTION TO 
#	RETURN COLUMNS OF PERCENTILE (0-100 BUT EXACT NOT USING FLOOR OR ROUND) AND BIN RESULTS, NAMED AS EXPECTED IN ESRI/SAIC CODE FOR EJSCREEN
#	GIVEN RAW DATA AND WEIGHTS
###############################################################

weightedCDFFun <- function(data, weightField) {
  # contains the pctiles code and calls the bins fxn setBin(one pctile in one block group), return df of P_ and B_ fields. 
  # if weight field's name was specified, get it as a vector, otherwise use 1 for every weight (not weighted). 
  if (missing(weightField)) {wts <- 1} else { wts <- data[ , weightField]}
  # Remove weights from data.frame (don't need percentiles of the weights field)
  data[ , weightField] <- NULL
  cat('Started creating percentiles... \n'); print(Sys.time())
  pctile.df <- as.data.frame( make.pctile.cols(data, wts) , stringsAsFactors=FALSE)
  cat('Finished creating percentiles... \n'); print(Sys.time())
  cat('Started creating bins... \n'); print(Sys.time())
  bin.df    <- as.data.frame( make.bin.cols(pctile.df), stringsAsFactors=FALSE)
  cat('Finished creating bins... \n'); print(Sys.time())
  rownames(pctile.df) <- NULL; rownames(bin.df) <- NULL # otherwise 100* pctile.df won't work!
  pctile.df <- 100 * pctile.df

  # To return the floored integer percentiles instead,
  # if needed (but python code does that later for text fields):
  #
  #   integer.pctiles <- data.frame(lapply(pctile.df, floor), stringsAsFactors=FALSE)
  #   names(integer.pctiles) <- gsub('pctile.', 'P_', names(integer.pctiles))
  #   names(bin.df) <- gsub('bin.', 'B_', names(bin.df))
  #   return(data.frame( integer.pctiles, bin.df, stringsAsFactors=FALSE))  

  # To return the exact percentiles:
  # Change fieldnames to convention used by python code:
  
  names(pctile.df) <- gsub('pctile.', 'P_', names(pctile.df))
  names(bin.df) <- gsub('bin.', 'B_', names(bin.df))
  return(data.frame( pctile.df, bin.df, stringsAsFactors=FALSE))  
}
##############################################################################################################################


#####################################################################
#	weightedCDF is a FUNCTION THAT 
#	reads a CSV file from disk (with percentile values), 
#	checks the input dataset of rows and columns of percentile values, 
#	and then for the appropriate input columns, writes the bin numbers.
#####################################################################

weightedCDF <- function(inCSV, outCSV, weightField = NA, IDFIELD = 'ID', colNames = c(), colClasses, nRows = 250000, MISSING.VALUE.SYMBOL=default.MISSING.VALUE.SYMBOL) {

   # Made default nRows 250k, since that should be a safe overestimate of block group count and speed up import a lot.  as of 5/8/2014
   
    if (missing(colClasses)) {
  
  # 	NEW CODE USES THIS DEFAULT SPECIFIED EARLIER IN THE FILE DEFINING THIS FUNCTION as of 5/8/2014
  
  colClasses <- myclasses
  
  #	THIS CODE IS OBSOLETE - IT USED TO READ 1 ROW TO FIGURE OUT THE COLUMN CLASSES, BUT IT DIDN'T WORK WELL, SO REPLACED WITH DEFAULT COL CLASSES ABOVE.   as of 5/8/2014
  if (1==0) { 
    ####################################################################################
    # first check the file's data [ONLY IF colClasses were not specified]
    ####################################################################################
    
    #### Read First Row to Assess Numeric and Get Column Names ####
    firstRow = read.csv(inCSV, nrows = 1) 
    # in some cases would need to say na.strings=c('NULL','NA'); 
    # na.strings=(-9999999) won't work since numeric, and anyway this import of just first line needs to see numeric to get column type right, so manually adjust those later, below.
    allNames = colnames(firstRow)

    #### If no Column Names Specified, Start with All of Them ####
    if (length(colNames) == 0){
        colNames = colnames(firstRow)
        }

    #### Add Weight Field to vector of fieldnames ####
    if (is.na(weightField) == FALSE){
        colNames = append(colNames, weightField)
        }

    ############################
    #### Assess Which columns (fields) are Numeric ####
    isNumer = sapply(firstRow, is.numeric)
    finalColNames = c()
    badColNames = c()
    colList = c()
    for (i in 1:length(isNumer)){
        colName = allNames[i]
        if ((colName %in% colNames) & (isNumer[i] == TRUE)){
            finalColNames = append(finalColNames, colName)
            colList = append(colList, 'numeric')
            }
        else {
            badColNames = append(badColNames, colName)
            colList = append(colList, "NULL")
            }
        }

    str = paste(badColNames, collapse= ", ")
    badStr = "The following fields are not numeric and had to be dropped from the analysis: %s"
    sprintf(badStr, str)
    ############################
	# This step is obsolete now that a default was added for nRows:
    #### Assess How Many Rows to Read/Write ####
    #if (is.na(nRows)){
    #    #### Set to All Rows ####
    #    nRows = -1
    #    }
    }    

    } 
    
    colList <- colClasses

    ################################################################################################################
    # Now actually do the number crunching
    # now that columns have been checked, re-read percentile data and write bin results
    ################################################################################################################

    ############################
    #### Read valid Data FROM DISK AGAIN ####
    ############################

    cat('Started reading csv file... '); print(Sys.time())
    data <- read.csv(inCSV, colClasses = colList, nrows = nRows) 
    cat('Finished reading csv file... ');  print(Sys.time())  # took 33 seconds on fast machine, 80 on slow one

    cat('Started creating NA values for missing data... ') ; print(Sys.time())
    data[data==MISSING.VALUE.SYMBOL] <- NA  # took 25 seconds on fast machine
    cat('Finished creating NA values for missing data... '); print(Sys.time())
    
    # NOTES ON NA / MISSING / NULL handling: 
    #
    # ESRI USES NULL TO INDICATE MISSING, BUT 
    # R MUST READ MISSING DATA AS NA AND THEN ESRI MUST SEE THOSE AS NULL AGAIN
    # WHEN EXPORTING FROM ARCGIS TO CSV TO R TO CSV TO ARCGIS
    # so we use a special number to indicate NA / NULL / missing data.
    # *** The data in the csv here, exported from python/arcgis, will be -9999999 to designate missing data. 
    # *** Since otherwise python would export zero for a NULL value.
    #
    # Save missing values not as NA but based on MISSING.VALUE.SYMBOL, using na=  parameter of write.table()
    # outData[is.na(outData)] <- MISSING.VALUE.SYMBOL
    #
    # NOT SURE IF as.is=TRUE is needed or just uses more memory. as.is=FALSE would save RAM if many text fields have many duplicate values, like state abbreviation.
    # I don't think numeric fields get turned into factors ever.
    # But we are only importing numeric columns here so it should not matter.
    #
    # Note: We don't use a character string, which wastes RAM and makes you import character class. If we did, we would use
    #   added na.strings=c('NULL','NA') 
    #   and then later would say write.table( ... , na='NULL')  or something like that.

    ############################
    #### Run Analysis, BY CALLING FUNCTIONS DEFINED PREVIOUSLY ####
    # to convert raw scores to percentiles, then to bins, and save all as a csv file written to disk.
    ############################

    cat('Starting percentiles and bins function... '); print(Sys.time())
    outData <- weightedCDFFun(data, weightField = weightField) # Took about 8 minutes on fast machine 5/8/2014
    cat('Finished percentiles and bins function... '); print(Sys.time())
     save(data, file='data.RData')
     save(outData, file='outData.RData')
  # ADD BACK IN THE IDFIELD COLUMN THAT HAS THE UNIQUE ID TO ENABLE JOINING THESE RESULTS TO THE REST OF THE DATASET
    # (Note you shouldn't really need this since results are returned in the exact same row order as the inputs.)
    outData <- data.frame(data[ , IDFIELD], outData, stringsAsFactors=FALSE)
    names(outData)[1] <- IDFIELD  
    rm(data) # can save some RAM before writing table

    cat('Starting to convert NA values to MISSING.VALUE.SYMBOL... '); print(Sys.time())
    outData[is.na(outData)] <- MISSING.VALUE.SYMBOL
    cat('Finished converting NA values to MISSING.VALUE.SYMBOL... '); print(Sys.time())
  
    cat('Starting to write output csv file... '); print(Sys.time())
    write.table(outData, file = outCSV, sep = ",", row.names = FALSE)
    cat('Finished writing output csv file... '); print(Sys.time())

    cat('All finished... '); print(Sys.time())
  
    # also return the resulting data, after having written it to a file on disk
    return(outData)

    }
#############################################################################################################################
}