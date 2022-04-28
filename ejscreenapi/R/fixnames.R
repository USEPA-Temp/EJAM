fixnames <- function(headernames) {
  
  oldnames <- map_names$oldnames
  newnames <- map_names$newnames
  
  newnames <- newnames[oldnames %in% headernames]
  oldnames <- oldnames[oldnames %in% headernames]
  headernames[match(oldnames, headernames)] <- newnames
  return(headernames)
}

