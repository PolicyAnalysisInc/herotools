#' Convert All Factors in Dataframe to Strings
#' 
#' Takes a data.frame and converts any factor column
#' to strings.
#' 
#' @param x a data.frame
#' @return a data.frame with factor columns converted to string
#' 
#' @export
#' 
#' @tests
#' expect_equal(
#'  clean_factors(data.frame(a = 'foo', b = 1, stringsAsFactors = TRUE)),
#'  data.frame(a = 'foo', b = 1, stringsAsFactors = FALSE)
#' )
clean_factors <- function(x) {
  if (any(unlist(lapply(x, is.factor)))){
    for (i in seq_along(x)) {
      if (is.factor(x[[i]])) {
        x[[i]] <- as.character(x[[i]])
      }
    }
  }
  x
}