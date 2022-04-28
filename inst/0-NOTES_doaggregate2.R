if (1 == 0 ) {
  
  # Maybe want to use the official EJScreen block weights numbers from here:
  # 
  # Description: Centroids with statistic weight factors 
  #   for both 2010 Census SF3 and 2012 ACS, block level
  # 
  # Layer: SF1_2010_BK_WEIGHT (ID: 71)
  # Name: SF1_2010_BK_WEIGHT
  # Display Field: GEOID10
  # Type: Feature Layer
  # Geometry Type: esriGeometryPoint
  # MaxRecordCount: 1000    "capabilities": "Map,Query,Data",
  # Supported Query Formats: JSON, geoJSON
  # https://ejscreen.epa.gov/arcgis/rest/services/ejscreen/ejquery/MapServer/71
  # Fields:
  # OBJECTID ( type: esriFieldTypeOID, alias: OBJECTID )
  # Shape ( type: esriFieldTypeGeometry, alias: Shape )
  # ***   GEOID10 ( type: esriFieldTypeString, alias: GEOID10, length: 15 )
  # ***   POP_WEIGHT ( type: esriFieldTypeDouble, alias: POP_WEIGHT )
  # HOU_WEIGHT ( type: esriFieldTypeDouble, alias: HOU_WEIGHT )
  # STCNTRBG10 ( type: esriFieldTypeString, alias: STCNTRBG10, length: 12 )
  # STCNTRBG12 ( type: esriFieldTypeString, alias: STCNTRBG12, length: 12 )
  # AREA_WEIGHT ( type: esriFieldTypeDouble, alias: AREA_WEIGHT )
  # POP_WEIGHT12 ( type: esriFieldTypeDouble, alias: POP_WEIGHT12 )
  # HOU_WEIGHT12 ( type: esriFieldTypeDouble, alias: HOU_WEIGHT12 )
  # STCNTRBG14 ( type: esriFieldTypeString, alias: STCNTRBG14, length: 12 )
  # POP_WEIGHT14 ( type: esriFieldTypeDouble, alias: POP_WEIGHT14 )
  # HOU_WEIGHT14 ( type: esriFieldTypeDouble, alias: HOU_WEIGHT14 )
  # AREA_WEIGHT14 ( type: esriFieldTypeDouble, alias: AREA_WEIGHT14 )
  # STCNTRBG15 ( type: esriFieldTypeString, alias: STCNTRBG15, length: 12 )
  # POP_WEIGHT15 ( type: esriFieldTypeDouble, alias: POP_WEIGHT15 )
  # HOU_WEIGHT15 ( type: esriFieldTypeDouble, alias: HOU_WEIGHT15 )
  # AREA_WEIGHT15 ( type: esriFieldTypeDouble, alias: AREA_WEIGHT15 )
  # PLFIPS ( type: esriFieldTypeString, alias: PLFIPS, length: 7 )
  # NEAR_FID ( type: esriFieldTypeInteger, alias: NEAR_FID )
  # NEAR_DIST ( type: esriFieldTypeDouble, alias: NEAR_DIST )
  
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
# 
#     popmeancols=popmeancols_default, countcols=countcols_default, indicator_formulas=indicator_formulas_default
# 
  # approxmeancols  <- c(ejscreen::names.d)  # approx mean??
  # ONE COULD GET A ROUGH APPROXIMATION THIS WAY, AND THAT MIGHT BE HOW EJSCREEN DOES IT.
  #  'VSI.eo' is part of names.d. This is ONLY approx but NOT really correct, though, except for pctmin and pctunder5 and pctover64 
  # VSI.eo is the demog indicator so we can approximate it as popwtd mean but exact formula needed to do it right via sums of counts.
  # "VSI.eo", (AND DO NOT USE "VNI.eo", "VDI.eo" once bg scores made)   #  VSI,VDI,VNI do not make sense if calculated for a large area!! same for EJ index
  # "VDI.eo" - we do not really need VDI.eo rolled up since it is only used for EJ index and that cannot be calculated except at bg scale.
 
 
  # # I DON'T KNOW HOW TO DO THIS JOIN and rollup to bg scale at same time... 
  # # WHEN THE LONG DT IS blockwts, and subset I want is what matches sites2blocks, 
  
  # cannot roll up to bg level and drop blockid info until removed duplicate blockid for overall stat calc, or
  # if keeping it by siteid, then retain duplicate blockids, and can roll up to bg level but some blocks and some bg are near 2+ siteid, 
  # so make sure that is accounted for... 
  
    # when aggr bgs by siteid, also need to weight this way:  bgwt*pop 
  
  
  }