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

time_in_days <- function(x, days_per_year) {
  switch(
    tolower(x),
    "days" = 1,
    "weeks" = 7,
    "months" = days_per_year / 12,
    "years" = days_per_year
  )
}