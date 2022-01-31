#' @name NAICS
#' @docType data
#' @title NAICS (industry classification system codes) 
#' 
#' @description
#'   These industry names and codes get updated about every 4 years (2017 version replaced by 2022 version in January 2022).
#'   See \url{https://www.census.gov/naics/}
#'  \preformatted{
#'  This is a list (but may change to data.frame) of NAICS codes.
#'   The codes are numeric, where names are the code followed by the title of the industrial sector.
#'   To check the vintage of the dataset, check
#'    attr(NAICS, 'year')
#'   The format is like this, and for 2017 version it had 2193 entries:
#'  #  x <- list(
#'  #   `11 - Agriculture, Forestry, Fishing and Hunting` = 11,
#'  #   `111 - Crop Production` = 111,
#'  #   `1111 - Oilseed and Grain Farming` = 1111,
#'  #   `11111 - Soybean Farming` = 11111,
#'  #   `111110 - Soybean Farming` = 111110
#'  #   )

#'  About NAICS codes:
#'  
#'  The North American Industry Classification System (NAICS)
#'  is a system for classifying establishments (individual business locations) 
#'  by type of economic activity.
#'  https://www.census.gov/naics/ 
#'  
#'  The North American Industry Classification System (NAICS) is
#'  the standard used by Federal statistical agencies in
#'  classifying business establishments for the purpose of 
#'  collecting, analyzing, and publishing statistical data 
#'  related to the U.S. business economy.
#'  
#'  The codes were updated 2007, 2012, 2017, and for 2022 (announced Dec. 2021).
#'  Proposed changes https://www.census.gov/naics/federal_register_notices/notices/fr02jy21.pdf
#'  Finalized changes: https://www.census.gov/naics/federal_register_notices/notices/fr21dc21.pdf 
#'  Effective Date for 2022 NAICS
#'  United States codes and Statistical
#'  Policy Directives: Federal statistical
#'  establishment data published for
#'  reference years beginning on or after
#'  January 1, 2022, should be published
#'  using the 2022 NAICS United States
#'  codes. Publication of NAICS United
#'  States, 2022 Manual is planned for
#'  January 2022 on the NAICS website at
#'  www.census.gov/naics. The updated
#'  Statistical Policy Directive No. 8, North
#'  American Industry Classification
#'  System: Classification of
#'  Establishments, will be effective
#'  immediately and will be posted on the
#'  OMB Statistical Programs and Standards
#'  website at www.whitehouse.gov/omb/
#'    information-regulatory-affairs/
#'    statistical-programs-standards/.
#'  }
#' @details
#'  \preformatted{
#'   ## see   https://www.census.gov/naics/
#'
#'   # to get 2017 version into this format, see NAICS_get 
#'   NAICS <- NAICS_get(); save(NAICS, file = 'yourpath/EJAM/data/NAICS.rdata')
#'   }
#' @seealso  \link{NAICS_find}  \link{NAICS_categories}   \link{NAICS_get} 
NULL
