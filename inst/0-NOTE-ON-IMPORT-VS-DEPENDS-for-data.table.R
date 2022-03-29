# DESCRIPTION file:
#   
#   Imports: data.table
# 
#     ( Besides the Imports: field, 
#     you can also use Depends: data.table but we strongly discourage this way (and may disallow it in future)
#     because this loads data.table into your user’s workspace; 
#       i.e. it enables data.table functionality in your user’s scripts without them requesting that.)
#  **** Imports: is the proper way to use data.table within your package without inflicting data.table on your user. 
# In fact, we hope the Depends: field is eventually deprecated in R since this is true for all packages.
# 
# 
# NAMESPACE file {NAMESPACE}
# 
# The next thing is to define what content of data.table your package is using. This needs to be done in the NAMESPACE file. Most commonly, package authors will want to use 
# 
#   import(data.table) 
# 
# which will import all exported (i.e., listed in data.table’s own NAMESPACE file) functions from data.table.