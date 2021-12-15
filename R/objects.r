#' @tests
#' expect_equal(
#'  class(create_list_object(c('a','b'),
#'  list())), c('a','b')
#' )
#' 
#' @export
create_list_object <- function(class, ...) {
    structure(list(...), class = class)
}