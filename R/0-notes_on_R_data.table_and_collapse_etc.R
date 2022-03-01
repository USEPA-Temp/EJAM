# notes_on_R_data.table_and_collapse_etc
# 
# 
# ################################################
# # general notes on R and data.table package
# ################################################
# 
# BIG BOOK OF R
# https://www.bigbookofr.com/index.html
# some graphics to look at:
#   library(inspectdf) 
# https://bbc.github.io/rcookbook/#how_does_the_bbplot_package_work 
#   https://www.desmog.com/2022/01/11/proximity-oil-gas-drilling-hypertension-pregnancy-willis/
#   
#   
#   DATA TABLE - UPDATE BY REFERENCE TO AVOID COPIES:
#   
#   General note on data.table and making copies vs just updating by reference:
#   https://stackoverflow.com/questions/10225098/understanding-exactly-when-a-data-table-is-a-reference-to-vs-a-copy-of-another
# 
# Use setattr set and :=
#   to avoid making huge copies.
# 
# Be careful about supposedly assigning a data.table as if you thought a copy were being made:
#   dtalias <- dt  
# because they are both aliases for the same actual object in memory
# so subsequently  altering dt also alters dtalias!
#   
#   Also, if you pass dt to a function then the function does not have to return(dt) ! 
#   the function's side effect  will alter the original dt back in the calling environment!! 
#  simply via any changes by ref to dt  inside the function passed dt  like  f(dt) 
# 
# Since [.data.table incurs overhead to check the existence and type of arguments (for example), set() provides direct (but less flexible) assignment by reference with low overhead, appropriate for use inside a for loop. See examples. := is more powerful and flexible than set() because := is intended to be combined with i and by in single queries on large datasets.
# 
# see
# https://rstudio-education.github.io/hopr/modify.html 
# 
# COLLAPSE: 
# 
# Even faster than data.table in some scenarios, is new package called collapse
# install.packages("collapse")
# library(collapse)
# 
# help('collapse-documentation')
# 
# fmean()
# 
# ??num_vars
# 
# ################################################