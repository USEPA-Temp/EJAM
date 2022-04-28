#' estimate how long it will take to get buffer batch results
#'
#' 
#' @param n number of points to buffer at
#' @param perhourslow n per hour if slow (conservative estimate of time needed)
#' @param perhourfast n per hour if fast
#' @param perhourguess n per hour best guess
#'
speedmessage <- function(n, perhourslow=1000,perhourfast=12000,perhourguess=6000) {
  
  fast_minutes  <- round(60 * n / perhourfast, 1)
  guess_minutes <- round(60 * n / perhourguess, 1)
  slow_minutes  <- round(60 * n / perhourslow, 1)
  
  msg <- paste0(
    'Results for ', n, ' points may take ',
    guess_minutes, ' minutes (or up to ',
    # fast_minutes, '-', 
    slow_minutes,')'
  )
  return(msg)
}