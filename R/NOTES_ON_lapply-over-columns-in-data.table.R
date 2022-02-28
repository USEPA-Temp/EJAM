if (0==1){
  
  
  ######################################################
  # notes - do something to each column, for some set of columns
  
  # The .SDcols argument is quite flexible. 
  #You can supply .SDcols as any of the following:
  #  vector of quoted column names
  #  names in a variable
  #  grep & value=TRUE (regular expression-based pattern matching)
  #  is.character(), or any logical vector to include/exclude each column
  #  column numbers as an integer vector
  
  fkt = c('teamIDBR', 'teamIDlahman45', 'teamIDretro')
  Teams[ , (fkt) := lapply(.SD, factor), .SDcols = fkt]
  # Note that we must wrap fkt in parentheses ()    !!!!!! ****** https://rdatatable.gitlab.io/data.table/articles/datatable-reference-semantics.html 
  # to force data.table to interpret this as column names, 
  # instead of trying to assign a column named 'fkt'.
  
  # using variable with list of colnames
  in_cols  = c("dep_delay", "arr_delay")
  out_cols = c("max_dep_delay", "max_arr_delay")
  flights[, c(out_cols) := lapply(.SD, max), by = month, .SDcols = in_cols]
  
  # using grep
  team_idx = grep('team', names(Teams), value = TRUE)
  Teams[ , (team_idx) := lapply(.SD, factor), .SDcols = team_idx]
  
  # using is.numeric, etc.
  # while .SDcols  accepts a logical vector ,  
  #   :=  does not accept a logical vector, so we 
  #  need to convert to column numbers using  which() 
  fkt_idx = which(sapply(Teams, is.factor))
  Teams[ , (fkt_idx) := lapply(.SD, as.character), .SDcols = fkt_idx]
  
  ######################################################
  
  
  
  
  # a data.table (or data.frame) is a list where each element is a column – thus, 
  # sapply/lapply applies the FUN argument (in this case, is.character) to each column 
  # and returns the result as sapply/lapply usually would.
  
  
  # confirm that they're stored as `character`
  Teams[ , sapply(.SD, is.character), .SDcols = fkt]
  #       teamIDBR teamIDlahman45    teamIDretro 
  #           TRUE           TRUE           TRUE
  setDF(Teams) # convert to data.frame for illustration
  sapply(Teams[ , fkt], is.character)
  setDT(Teams) # convert back to data.table
  
  
  # to convert these columns to factor 
  #  simply add the := assignment operator:
  
  # Note that we must wrap fkt in parentheses ()    !!!!!! ****** https://rdatatable.gitlab.io/data.table/articles/datatable-reference-semantics.html 
  # to force data.table to interpret this as column names, 
  # instead of trying to assign a column named 'fkt'.
  
  Teams[ , (fkt) := lapply(.SD, factor), .SDcols = fkt]
  
  # print out the first column to demonstrate success
  head(unique(Teams[[fkt[1L]]]))
  # [1] BOS CHI CLE KEK NYU ATH
  # 101 Levels: ALT ANA ARI ATH ATL BAL BLA BLN BLU BOS BRA BRG BRO BSN BTT BUF BWW CAL CEN CHC ... WSN
  
  
  
  # For example, we could do the following to convert all factor columns to character:
  
  # while .SDcols accepts a logical vector,
  #   := does not, so we need to convert to column
  #   positions with which()
  
  fkt_idx = which(sapply(Teams, is.factor))
  
  Teams[ , (fkt_idx) := lapply(.SD, as.character), .SDcols = fkt_idx]
  head(unique(Teams[[fkt_idx[1L]]]))
  # [1] "NA" "NL" "AA" "UA" "PL" "AL"
  
  # Lastly, we can do pattern-based matching of columns in .SDcols,
  # to select all columns which contain team, back to factor:
  
  Teams[ , .SD, .SDcols = patterns('team')]
  #       teamID teamIDBR teamIDlahman45 teamIDretro
  #    1:    BS1      BOS            BS1         BS1
  #    2:    CH1      CHI            CH1         CH1
  #    3:    CL1      CLE            CL1         CL1
  
  # now convert these columns to factor;
  #   value = TRUE in grep() is for the LHS of := to
  #   get column names instead of positions
  team_idx = grep('team', names(Teams), value = TRUE)
  Teams[ , (team_idx) := lapply(.SD, factor), .SDcols = team_idx]
  
  
  
  in_cols  = c("dep_delay", "arr_delay")
  out_cols = c("max_dep_delay", "max_arr_delay")
  flights[, c(out_cols) := lapply(.SD, max), by = month, .SDcols = in_cols]
  
  
  
}
