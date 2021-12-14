#' Convert Between Probability & Odds
#' 
#' Functions to convert between probabilities and odds.
#' 
#' @name Probability and Odds
#' @rdname prob_and_odds
#' @export
#' 
#' @param p a probability to convert to odds
#' @param odds odds to convert to probability
#'
#' @tests
#' expect_equal(prob_to_odds(0.25), 1/3)
#' expect_equal(prob_to_odds(0.75), 3)
#' expect_equal(prob_to_odds(1), Inf)
#' expect_equal(prob_to_odds(0), 0)
prob_to_odds <- function(p) {
  ret <- vector(mode = 'numeric', length = length(p))
  one_index <- p == 1
  ret[one_index] <- Inf
  ret[!one_index] <- p[!one_index] / (1 - p[!one_index])
  ret
}

#' @name Probability and Odds
#' @rdname prob_and_odds
#' @export
#'
#' @tests
#' expect_equal(odds_to_prob(1/3), 0.25)
#' expect_equal(odds_to_prob(3), 0.75)
#' expect_equal(odds_to_prob(Inf), 1)
#' expect_equal(odds_to_prob(0), 0)
odds_to_prob <- function(odds) {
  ret <- vector(mode = 'numeric', length = length(odds))
  inf_index <- odds == Inf
  ret[inf_index] <- 1
  ret[!inf_index] <- odds[!inf_index] / (odds[!inf_index] + 1)
  ret
}