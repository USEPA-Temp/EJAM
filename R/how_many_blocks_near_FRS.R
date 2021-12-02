
########################################################### #
# HOW MANY CENSUS BLOCKS ARE WITHIN 1 OR 3 MILES OF 
# THE AVERAGE FACILITY IN EPA FRS?
########################################################### #

# There can be from 1 to 1076 blocks, or median of 86 blocks, mean of 136, within 1 mile of avg FRS site.
# There can be from 1 to 6861 blocks, or median of 567 blocks, mean of 926, within 3 miles.

########################################################## #
# Load FRS, Pick random sample of Facilities ####
########################################################## #

# This is the larger FRS dataset of 3.1 million, not the 1.5 mill actives in ECHO. 
load('C:/Program Files/R/mypackages/EJAM-FRSdata/data/frs.rdata')
# dim(frs)  # [1] 3,144,206       9
# > names(frs)
# [1] "REGISTRY_ID"    "PRIMARY_NAME"   "SITE_TYPE_NAME" "PGM_SYS_ACRNMS" "INTEREST_TYPES"
# [6] "NAICS_CODES"    "SIC_CODES"      "LAT"            "LONG"  
# t(frs[19,])
# [,1]
# REGISTRY_ID    "110045685941"
# PRIMARY_NAME   "OCEAN BEAUTY SEAFOODS ALITAK"
# SITE_TYPE_NAME "STATIONARY"
# PGM_SYS_ACRNMS "AIR:AK0000000215000019, AIRS/AFS:0215000019, CEDRI:CEDRI1550, EIS:10585311, NPDES:AKG520036, SFDW:AK2250315, SFDW:AK2250315 32219"
# INTEREST_TYPES "AIR EMISSIONS CLASSIFICATION UNKNOWN, AIR SYNTHETIC MINOR, COMPLIANCE AND EMISSIONS REPORTING, ICIS-NPDES NON-MAJOR, TRANSIENT NON-COMMUNITY WATER SYSTEM, WATER TREATMENT PLANT"
# NAICS_CODES    "311710, 311712" # There can be multiple codes for one site
# SIC_CODES      "2091, 2092"
# LAT            "56.8987"
# LONG           "-154.2475"

frs1000 <- frs[sample(1:NROW(frs), 1000), c('REGISTRY_ID', 'LAT', 'LONG', 'NAICS_CODES')]
rm(frs)
names(frs1000) <-  gsub('LAT', 'lat', names(frs1000))
names(frs1000) <-  gsub('LONG', 'lon', names(frs1000))
frs1000$frsid <- 1:1000
save(frs1000, file = '~/Downloads/frs1000_random_sample_from4m.rdata')

########################################################## #
# Load Census 2020 block points 
########################################################## #

load('C:/Users/mcorrale/OneDrive - Environmental Protection Agency (EPA)/Downloads/blocks2020.rdata')
# this is in EPA github for EJAM
#blocks2020 <- blocks2020[ , c('fipsblock', 'pop', 'lat', 'lon')]

########################################################### #
# find distances ####
########################################################### #

library(proxistat) # install_github('ejanalysis/proxistat')
# Using proxistat::get.distances.chunked()  since crashes lacking RAM if nonchunked used on 1000 frompoints.
#  is very very slow... only about 500 points per hour, so about 2 hours for just 1000 FRS at 1 distance.

########################################################### #
# radius of 1 mile ###########################################################
########################################################### #
if (1==0) { 
  # don't really need 1 mile if also getting 3 miles... just a subset of the 3 mile one.
distancesfilenames <- get.distances.chunked(frompoints = frs1000, topoints = blocks2020, radius = 1, fromchunksize = 10, folder = '~/Downloads/1mile')

for (i in 1:(1000/10)) {
  load(gsub('x', i, '~/Downloads/1mile/outputx.RData'))
  output <- cbind(output, chunk=i)
  if (i==1) {
    outall <- output
  } else {
    outall <- rbind(outall, output)
  }
}
outall <- cbind(outall, frsid=paste(outall[,'fromrow'], outall[,'chunk']))
print('Average number of Census2020 blocks within 1 mile of FRS site')
mean(table(outall[,'frsid']))
outall_1mile <- outall

outall_1mile.df <- as.data.frame(outall_1mile, stringsAsFactors = FALSE)
outall_1mile.df$fromrow <- as.numeric(outall_1mile.df$fromrow)
outall_1mile.df$torow <- as.numeric(outall_1mile.df$torow)
outall_1mile.df$d <- as.numeric(outall_1mile.df$d)
outall_1mile.df$chunk <- as.numeric(outall_1mile.df$chunk)

outall_1mile.df$chunk <- NULL
outall_1mile.df$fromrow <- NULL
outall_1mile.df$frsidx <- as.numeric(factor(outall_1mile.df$frsid))
outall_1mile.df$frsid <- NULL
names(outall_1mile.df) <- gsub('frsidx', 'frsid', names(outall_1mile.df))
names(outall_1mile.df) <- gsub('torow', 'blockid', names(outall_1mile.df))
outall_1mile.df <- outall_1mile.df[ , c('frsid', 'blockid', 'd')]
head(outall_1mile.df)
frs_to_blocks_1_miles <- outall_1mile.df
rm(outall_1mile.df)
save(frs_to_blocks_1_miles, file = '~/Downloads/frs_to_blocks_1_miles.rdata')

length(unique(frs_to_blocks_1_miles$frsid)) # only 992, not 1000 for some reason
# save(outall_1mile.df, file = '~/Downloads/outall_1mile.df.rdata')
# save(outall_1mile, file = '~/Downloads/outall_1mile.rdata')
# rm(outall)
# save.image(file = '~/Downloads/block to FRS distance image2 work in progress.RData')

# Starting chunk 100 / 100 , 990 frompoints done in 2 hours ( 8 /minute), 1.2 mins est. left to completion
# Time difference of 2 hours
#  This function got full set of distances at a rate of only 500 buffers per hour, super slow.
# Compared to just near/far list of blocks rate of about 5,000/hr using any other tools!
# and EJAM could get to 200k/hr. 
}

########################################################### #
# radius of 3 miles ###########################################################
########################################################### #

distancesfilenames <- get.distances.chunked(frompoints = frs1000, topoints = blocks2020, radius = 3, fromchunksize = 10, folder = '~/Downloads/3mile')

for (i in 1:(1000/10)) {
  load(gsub('x', i, '~/Downloads/3mile/outputx.RData'))
  output <- cbind(output, chunk=i)
  if (i==1) {
    outall <- output
  } else {
    outall <- rbind(outall, output)
  }
}
outall <- cbind(outall, frsid=paste(outall[,'fromrow'], outall[,'chunk']))
print('Average number of Census2020 blocks within 3 miles of FRS site')
mean(table(outall[,'frsid']))
outall_3mile <- outall

outall_3mile.df <- as.data.frame(outall_3mile, stringsAsFactors = FALSE)
outall_3mile.df$fromrow <- as.numeric(outall_3mile.df$fromrow)
outall_3mile.df$torow <- as.numeric(outall_3mile.df$torow)
outall_3mile.df$d <- as.numeric(outall_3mile.df$d)
outall_3mile.df$chunk <- as.numeric(outall_3mile.df$chunk)

outall_3mile.df$chunk <- NULL
outall_3mile.df$fromrow <- NULL
outall_3mile.df$frsidx <- as.numeric(factor(outall_3mile.df$frsid))
outall_3mile.df$frsid <- NULL
names(outall_3mile.df) <- gsub('frsidx', 'frsid', names(outall_3mile.df))
names(outall_3mile.df) <- gsub('torow', 'blockid', names(outall_3mile.df))
outall_3mile.df <- outall_3mile.df[ , c('frsid', 'blockid', 'd')]
head(outall_3mile.df)
frs_to_blocks_3_miles <- outall_3mile.df
rm(outall_3mile.df)
save(frs_to_blocks_3_miles, file = '~/Downloads/frs_to_blocks_3_miles.rdata')

# > length(unique(frs_to_blocks_3_miles$frsid))
# [1] 999 # only 999, not 1000 for some reason
# > length(unique(frs_to_blocks_3_miles$blockid))
# [1] 707291

# save(outall_3mile, file = '~/Downloads/outall_3mile.rdata')
# rm(outall)
# save.image(file = '~/Downloads/block to FRS distance image3 work in progress.RData')
# Starting chunk 100 / 100 , 990 frompoints done in 2 hours ( 8 /minute), 1.2 mins est. left to completion
# Time difference of 2 hours

###########################################################
# merge with other columns of FRS data? ####
###########################################################
# > dim(frs1000)
# [1] 1000    4
# > dim(blocks2020)
# [1] 8174955       5
rm(blocks2020)
rm(output, i, outall_1mile, outall_3mile, distancesfilenames)

frs_to_blocks_3_miles_plus <- merge(frs_to_blocks_3_miles, frs1000, by = 'frsid', all.x = TRUE, all.y = FALSE, sort = TRUE)
frs_to_blocks_3_miles_plus$NAICS_CODES <- NULL
# dim(frs_to_blocks_1_miles)
dim(frs_to_blocks_3_miles_plus)
head(frs_to_blocks_3_miles_plus)
# frsid blockid        d  REGISTRY_ID      lat       lon
# 1     1 3048332 2.939475 110003364555 30.44977 -91.17343
# 2     1 3048333 2.990966 110003364555 30.44977 -91.17343
# 3     1 3048334 2.729069 110003364555 30.44977 -91.17343
# 4     1 3048338 2.915495 110003364555 30.44977 -91.17343
# 5     1 3048339 2.844886 110003364555 30.44977 -91.17343
# 6     1 3048340 2.786961 110003364555 30.44977 -91.17343
dim(frs_to_blocks_3_miles_plus)
# [1] 924605      6

# Save this larger table with extra columns for REGISTRY_ID and lat and lon, for convenience.
save(frs_to_blocks_3_miles_plus, file = '~/Downloads/frs_to_blocks_3_miles_plus.rdata')

########################################################## #
# Get summary stats and graphics on how many blocks are nearby ####
########################################################## #

avg1 = mean(table(frs_to_blocks_1_miles[,'frsid']))
avg3 = mean(table(frs_to_blocks_3_miles[,'frsid']))
summary(as.numeric(table(frs_to_blocks_3_miles[,'frsid']))) 
# There can be from 1 to 1076 blocks, or median of 86 blocks, mean of 136, within 1 mile.
# There can be from 1 to 6861 blocks, or median of 567 blocks, mean of 926, within 3 miles.

plot(ecdf(as.numeric(table(frs_to_blocks_1_miles[,'frsid']))), 
     ylab='fraction of all FRS with no more than than this many blocks nearby', 
     sub=paste('mean =',round(avg1, 0), 'blocks nearby'), 
     main = 'CDF of number of blocks within a mile of an FRS site', 
     xlab = 'Count of nearby blocks')
abline(v=avg1)

# boxplot(as.numeric(table(outall_1mile.df[,'frsid'])), main = 'number of blocks within a mile of an FRS site', ylab = 'Count of nearby blocks')
# hist(as.numeric(table(frs_to_blocks_1_miles[,'frsid'])), 100, 
#      main = 'Histogram of number of blocks within a mile of an FRS site', 
#      xlab = 'Count of nearby blocks')

# Draw a US map of those 1000 random points. 
plot(frs1000$lon, frs1000$lat, pch='.')
points(frs1000$lon[1], frs1000$lat[1], col='red')

########################################################## #
# could try aggregating in a buffer for EJSCREEN indicator data ####
########################################################## #

# require(ejscreen) # install_github('ejanalysis/ejscreen')
# x=bg20
# x=x[, c(1:9,110:118)]

