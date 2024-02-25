#' Get the optimal fraction of your bankroll to bet based on the Kelly Criterion
#'
#' @param p The probability of a positive outcome
#' @param a The fraction that is gained in a positive outcome.
#' @param b The fraction that is lost in a negative outcome.
#'
#' @return A proportion
#' @export
#'
#' @examples
#' get_kelly_bet(0.8, 0.5, 2)
get_kelly_bet <- function(p = 0.5, a = 1, b = 1) {
  if (p < 0 || p > 1) {
    stop("p must be between 0 and 1")
  }
  if (a < 0) {
    stop("a must be greater than 0")
  }
  if (b < 0) {
    stop("b must be greater than 0")
  }

  return(p * a - (1 - p) * b)
}
