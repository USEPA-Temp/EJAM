#' @name NAICS
#' @docType data
#' @title named list ? or vector ? of NAICS codes (they get updated about every 4 years)
#' @description
#'  \preformatted{
#'   List of NAICS as numeric, where names are the code followed by the title of the industrial sector.
#'   To check the vintage of the dataset, check
#'    attr(NAICS, 'year')
#'   The format is like this, and for 2017 version it has 2193 entries:
#'  #  x <- list(
#'  #   `11 - Agriculture, Forestry, Fishing and Hunting` = 11,
#'  #   `111 - Crop Production` = 111,
#'  #   `1111 - Oilseed and Grain Farming` = 1111,
#'  #   `11111 - Soybean Farming` = 11111,
#'  #   `111110 - Soybean Farming` = 111110
#'  #   )
#'  }
#' @details
#'  \preformatted{
#'   ## see   https://www.census.gov/naics/
#'
#'   # to get 2017 version into this format, see NAICS_get()
#'   NAICS <- NAICS_get(); save(NAICS, file = 'yourpath/bufferfast/data/NAICS.rdata')
#'   }
#' @seealso  \link{NAICS_find}  \link{NAICS_categories}   \link{NAICS_get}
NULL
