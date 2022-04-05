#' Get Days Per Year
#' 
#' Traverses up environments until it can find days
#' per year defined.
#' 
#' @return length one numeric containing days per year
#' 
#' @export
get_dpy <- function() {
  dpy <- 365
  for(i in 1:10) {
    try({
      pf <- parent.frame(i)
      cl_d <- pf$cycle_length_days
      cl_y <- pf$cycle_length_years
      if (!is.null(cl_d) && !is.null(cl_y)) {
        dpy <- (cl_d / cl_y)[1]
        break
      }
    })
  }
  return(dpy)
}

#' Caclulate Time in Days
#' 
#' Takes a string representing a unit of time and
#' calculates the number of days contained per unit.
#' 
#' @param x Either "days", "weeks", "months", or "years".
#' @param days_per_year the number of days in a year to be used in conversions
#' 
#' @return the numer of days in the unit of time
#' 
#' @export
time_in_days <- function(x, days_per_year) {
  switch(
    tolower(x),
    "days" = 1,
    "weeks" = 7,
    "months" = days_per_year / 12,
    "years" = days_per_year
  )
}