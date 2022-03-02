if (1 == 0 ) {
  
  #  RENAME ALL as   sites2blocks  NOT   blocks2sites   via global find replace. 
  
  
  #######################################
  # BUFFER STATS FOR COUNTS:
  #   
  #   see EJAM-package.R for names of data files needed and column names.
  # 
  # Question: When to join the 200 or so blockgroupstats columns to the other info? 3 options a/b/c:
  #   
  #   A (no) - join/add 200 cols to the full 5m blocks in blockwts table, or
  # 
  # B (maybe) - join/add 200 cols to the maybe <1m or 100k blocks nearby, in sites2blocks table, [10x-100x fewer rows] 
  # 
  # C (YES?)  - join/add 200 cols to the maybe 300k-30k BLOCKGROUPS nearby, in bg2sites [again 1/30x rows] (if 25-50 blocks/bg, say 30 in avg sites?)
  
  ###################### #)  ###################### #)  ###################### #)  ###################### #
  
  # HOW TO AGGREGATE STATS ACROSS BLOCKGROUPS (IN EACH BUFFER OR OVERALL ACROSS ALL BUFFERS): 
  #   
  #   PERCENT DEMOGRAPHICS: 
  #   QUESTION IS WHETHER TO USE POPWTD MEANS OR FORMULAS APPLIED TO WTD SUMS OF COUNT VARIABLES - AGGREG AS POPWTD MEANS VS RECALCULATED FROM AGGREGATED COUNTS:
  #   We want to replicate EJSCREEN but we want users to be able to replicate our overall stats like %poor, and those may not both be possible...
  # If the numerator and denominator of some %-style indicator are available as counts by bg, then one could recalculate per site once counts were summed per site... but that is not what EJSCREEN reports seem to do???... They take pop wtd means of all raw score indicators (raw, meaning not expressed as percentiles) regardless of whether pop was the true denominator of that pct (it is not for some ejscreen pcts), which is probably OK, and we want to replicate EJSCREEN? 
  #   But then it will not replicate some other GIS analyses that actually did it more correctly and calculated overall percents from the aggregated, overall counts! The recalculation method requires providing the formulas for the calculated variables to ensure correct denominators, etc. We have formulas in ejscreen::ejscreenformulas$formula 
  # (such as for percent low income, pre1960 units, linguistic isolation of hhld, whose denominators are households, built units, age25up, those with known poverty ratio)
  # 
  # EJ INDEX: 
  #   simplest to explain or do is just pop wtd mean of bg-level EJ index values in a buffer, and probably is how EJSCREEN does it.
  # 
  # PERCENTILES: 
  #   The raw score is found for a buffer and then that is looked up in a percentile lookup table to see how to express it as a percentile (US, Region, and State percentiles are 3 separate values). For a user-defined custom indicator, the lookup table has to be created first, so the user indicator scores data must be available for every blockgroup in the State or US if we want to report in percentile terms (not just raw score mean per buffer or for avg person).
  #   # PERCENTILES AND BINS and pctile.text..(popup text)  ETC. MUST BE CREATED TOO
  #   Separately must calculate pctile, bin, pctile.text, US mean, State means, Region means, etc. 
  
  countcols <- c(
    "pop", "mins", 'nonmins',
    "lowinc", "povknownratio",   
    "lths", "age25up", 
    "lingiso", "hhlds", 
    "under5", "over64",
    "unemployed","unemployedbase",
    'pre1960','builtunits',
    "nhwa", "hisp", "nhba", "nhaa", "nhaiana", "nhnhpia", "nhotheralone", "nhmulti"
  )
  # **** but we probably treat pctpre1960 as pop wtd mean like other Evars?
  
  # dput(ejscreen::names.d.subgroups.count)
  # c("nhwa", "hisp", "nhba", "nhaa", "nhaiana", "nhnhpia", "nhotheralone", "nhmulti")
  # dput(ejscreen::names.d.subgroups.pct)
  # c("pctnhwa", "pcthisp", "pctnhba", "pctnhaa", "pctnhaiana", "pctnhnhpia", 
  #   "pctnhotheralone", "pctnhmulti")
  
  #   Not needed for buffer scale.  in formulas but not in bg21, since optional or just  needed only to create the variables in blockgroupstats:  
  # c("lingisospanish", "lingisoeuro","lingisoasian", "lingisoother")
  # c("built1950to1959", "built1940to1949",  "builtpre1940") 
  # c("nonhisp", "pop3002")
  # c('num1pov', 'pov50', 'pov99', 'num15pov', 'num1pov', 'pov124', 'pov149',"pov2plus", 'num2pov', 'num1pov', 'pov124', 'pov149', 'pov184', 'pov199', 'num2pov.alt' )
  # c("ageunder5m", "age5to9m", "age10to14m", "age15to17m", "age65to66m","age6769m", "age7074m", "age7579m", "age8084m", "age85upm", 
  # "ageunder5f", "age5to9f", "age10to14f", "age15to17f", "age65to66f", "age6769f", "age7074f", "age7579f", "age8084f", "age85upf") 
  # c("m0", "m4", "m6", "m8", "m9", "m10", "m11", "m12", "f0", "f4", "f6", "f8", "f9", "f10", "f11", "f12")
  
  popmeancols <- c(ejscreen::names.ej, ejscreen::names.e)  
  # *** we treat pctpre1960 as if can do popwtd avg, right? Technically pctpre1960 should use ejscreenformulas... ratio of sums of counts pre1960 and denom builtunits  
  # only 3 of names.d are exactly popmeans,  ("pctmin", "pctunder5", "pctover64") since denominators are pop. 
  #   May as well just calculate all of the names.d.pct exactly not some as popwtd mean and others not.
  # flagged is a variable that maybe has an obvious single way to be aggregated for a buffer? 
  # It could signal if any EJ>80 for avg person as avg of each EJ index for all residents in buffer, 
  # (or alternatively could perhaps tell us if there is any flagged bg at all in buffer?).
  
  calculatedcols <- c(ejscreen::names.d, 'flagged') 
  # These must be calculated after aggregating count variables and using those at siteid level. 
  # Use ejscreen::ejscreenformulas$formula to calculate these.
  
  approxmeancols  <- c(ejscreen::names.d) 
  # ONE COULD GET A ROUGH APPROXIMATION THIS WAY, AND THAT MIGHT BE HOW EJSCREEN DOES IT.
  #  'VSI.eo' is part of names.d. This is ONLY approx but NOT really correct, though, except for pctmin and pctunder5 and pctover64 
  # VSI.eo is the demog indicator so we can approximate it as popwtd mean but exact formula needed to do it right via sums of counts.
  # "VSI.eo", (AND DO NOT USE "VNI.eo", "VDI.eo" once bg scores made)   #  VSI,VDI,VNI do not make sense if calculated for a large area!! same for EJ index
  # "VDI.eo" - we do not really need VDI.eo rolled up since it is only used for EJ index and that cannot be calculated except at bg scale.
  
  
  #################################
  # # 1) AGGREGATE TO BLOCKGROUP SCALE by site ***   #################################
  # 
  # # output of getrelevant is   sites2blocks[ , .(siteid, blockid, distance)]
  # 
  # # I DON'T KNOW HOW TO DO THIS JOIN and rollup to bg scale at same time... 
  # # WHEN THE LONG DT IS blockwts, and subset I want is what matches sites2blocks, 
  
  # cannot roll up to bg level and drop blockid info until removed duplicate blockid for overall stat calc, or
  # if keeping it by siteid, then retain duplicate blockids, and can roll up to bg level but some blocks and some bg are near 2+ siteid, 
  # so make sure that is accounted for... 
  
  
    # when aggr bgs by siteid, also need to weight this way:  bgwt*pop 
  
  # sites2bg <- sites2blocks[blockwts, .(siteid, bgfips, distance, bgwt = sum(blockwt, na.rm=TRUE)), on = .(blockid), by =.(siteid, bgfips)] 
  # rm(sites2blocks) # unless need save that to analyze distance distribution 
  
  # 2)  JOIN that mid-sized table OF BLOCKGROUPS, to blockgroupstats (200 cols)  ###################### #
  
  sites2bg[, lapply(.SD, popmeancols := sum(pop * blockwt * blockgroupstats[,popmeancols], na.rm=T) / sum(pop * blockwt, na.rm=T)), by = .(siteid), .SDcols = popmeancols]
  
  sites2bg[, lapply(.SD, countcols   := sum(      blockwt * blockgroupstats[,  countcols], na.rm=T)), by = .(siteid), .SDcols = countcols]  
  
  
  # Notes on using variable with list of colnames, to 
  # apply function to specified columns and 
  # assign results with specified variable names to the original data.
  #  but I don't need to rename the indicators actually.
  # 
  # in_cols  = c("dep_delay", "arr_delay")
  # out_cols = c("max_dep_delay", "max_arr_delay")
  # flights[, c(out_cols) := lapply(.SD, max), by = month, .SDcols = in_cols]
  
  
  #######################################  
  # 
  # 'pctile.pctunemployed' should be added to ejscreen::names.d.pctile  and saved as data
  # "bin.unemployed" should be added to ejscreen::names.d.bin  and saved as data
  
  }