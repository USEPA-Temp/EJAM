if (1==0) { # do not want to run the code even if this doc is saved in the R folder of the package


  ########################################################################

  # also see batch.summarizer::ejscreenapi


  ######### quick example of using sf::st_buffer() and mapview #########

  library(units)
  library(tidyverse)
  library(sf)
  library(mapview)
  #  help("mapview", package = "mapview")

  radius <- 5000
  # should also use units::install_unit()  ?

  # Specify site that is center of circular buffer:
  # in Baltimore
  lon <- -76.6
  lat <- 39.2
  elev <- units::set_units(242.0, "ft") # just an example


  site <- sf::st_point(x = c(lon, lat, elev), dim = "XYZ")
  site <- site %>% sf::st_sfc(crs = 4326)

  # # can transform your coordinate to Irish Grid, if viewing Dublin:
  # site = sf::st_transform(site, 29902)

  #### Create your buffer in meters around this point: #########

  buffer <- sf::st_buffer(site, radius)

  #### to plot just the site and circle, using plot #########
  # plot(buffer)
  # plot(site, add = TRUE)

  #### plot nice interactive map with basemaps, using mapview #########

  mapview::mapview(buffer, map.types = c("OpenStreetMap", "OpenTopoMap", "Esri.WorldImagery", "CartoDB.Positron"  ))
  # see help("mapview", package = "mapview")

  ########################################################################





  #### annulus (ring) #########

  buff1 <- sf::st_buffer(site, dist=1*radius)
  buff2 <- sf::st_buffer(site, dist=2*radius)
  ring  <- sf::st_difference(buff2, buff1)

  mapview::mapview(ring, map.types = c("OpenStreetMap", "OpenTopoMap", "Esri.WorldImagery", "CartoDB.Positron"  ))

  #### plot just using ggplot #########

  # ring %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_sf()

  ########################################################################

  #### putting a grid on top of a shapefile #########

  nc <- NULL
  demo(nc, ask = FALSE, echo = FALSE)
  sf::st_crs(nc)$proj4string
  # [1] "+proj=longlat +datum=NAD27 +no_defs"

  nc <- sf::st_transform(nc, crs=sf::st_crs(5070))
  cs <- c(10000, 10000)
  my_grid <- sf::st_make_grid(x = nc,
                              cellsize=cs)

  #### plot state and grid using MAPVIEW #########

  mapview::mapview(my_grid) + mapview::mapview(nc['NAME'])

  #### plot state and grid using plot #########

  # plot(nc['NAME'], main = 'North Carolina')
  # plot(my_grid, add = TRUE)

  }
