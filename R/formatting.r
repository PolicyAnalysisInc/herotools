#' @tests
#' expect_equal(
#'  quoted_list_string(c('a','b','c')),
#'  '"a", "b", "c"'
#' )
#' 
quoted_list_string <- function(x) {
    paste0(paste0('"', x, '"'), collapse = ', ')
}

#' @tests
#' 
#' expect_equal(
#'  get_indefinite_article('fruit'),
#'  'a'
#' )
#' 
#' expect_equal(
#'  get_indefinite_article('apple'),
#'  'an'
#' )
get_indefinite_article <- function(word) {
    if(substr(word, 0, 1) %in% word_start_vowels) {
        return('an')
    }

    'a'
}

#' @tests
#' 
#' expect_equal(
#'  create_param_formatter(digits = 4)(0.1234567),
#'  "0.1235"
#' )
create_param_formatter <- function(...) {
    args <- list(...)
    args$trim <- TRUE
    if (is.null(args$digits)) {
        args$digits <- 3
    }
    function(x) {
        format_args <- append(list(x), args)
        do.call(format, format_args)
    }
}

to_list_item_output <- function(x, n = 6, skip = 0) {
  output <- capture.output(print(x))
  if (skip > 0) {
    output <- output[-seq_len(skip)]
  }
  n_lines <- length(output)
  if (n_lines == 1) {
    return(output)
  }
  indents <- paste0(rep(' ', n + 2), collapse = '')
  indented_output <- paste0('\n', paste0(indents, output, collapse = '\n'))

  indented_output
}