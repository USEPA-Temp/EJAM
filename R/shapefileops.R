#' Spatial overlay of facilities points and US States shapefiles to add STATE as column in facility table
#'
#' @param facs facilities LONG LAT table
#' @param shapefile shapefile of States
#'
#' @return Returns the facs that was passed to this function, but with a new column, STATE, that has the name of the State each point is inside of
#' @export
#'
merge_state_shapefiles <- function(facs, shapefile) {
  # require(sp)
  # require(rgdal)
  # require(maps)
  sp::coordinates(facs) <- c("LONG", "LAT")
  sp::proj4string(facs) <- sp::proj4string(shapefile)
  inside.states <- !is.na(sp::over(facs, as(shapefile, "SpatialPolygons")))
  facs$STATE <- sp::over(facs, shapefile)$NAME
  return(facs)
}
