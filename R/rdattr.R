rdattr <- function(package='EJAM', which='year') {
  # utility to check if year attribute is set on each data file
  if (!isNamespaceLoaded(package)) {stop('package not loaded')}
  rdafiles <- data(package=package)
  rdafiles <- rdafiles$results[,'Item']
  rdafiles <- gsub(' .*' ,'', rdafiles)
  cbind(year = sapply(rdafiles, function(z) try(attr(get(z),which = which))))
}
