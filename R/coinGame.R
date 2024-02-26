#' Get the optimal fraction of your bankroll to bet based on the Kelly Criterion
#'
#' @param rounds Number of rounds to simulate
#' @param heads_prob Probability of heads
#' @param starting_capital Initial amount of money
#' @param head_factor Factor to multiply bet by if heads
#' @param tails_factor Factor to multiply bet by if tails
#' @param min_bet Minimum bet
#' @param max_payout Maximum payout
#' @param betting_fraction Fraction of funds to bet
#'
#' @return A list with the results, toss results, and payout
#' @export
#'
#' @examples
#' coin_toss_seq(10, 0.5, 100, 2, 0.5, 1, 1000, 0.5)
#'
coin_toss_seq <- function(
    rounds = 1,
    heads_prob = 0.5,
    starting_capital = 100,
    head_factor = 2,
    tails_factor = 0.5,
    min_bet = 1,
    max_payout = Inf,
    betting_fraction = 1) {
  if (starting_capital < 0) {
    stop("starting_capital must be positive")
  }
  if (head_factor < 0) {
    stop("head_factor must be positive")
  }
  if (tails_factor < 0) {
    stop("tails_factor must be positive")
  }
  # betting_fraction must be between 0 and 1 for now, could add leverage later
  if (betting_fraction < 0 || betting_fraction > 1) {
    stop("betting_fraction must be between 0 and 1")
  }

  results <- c(starting_capital)
  toss_results <- stats::rbinom(rounds, size = 1, prob = heads_prob)

  for (res in toss_results) {
    current_funds <- results[length(results)]
    new_bet <- current_funds * betting_fraction

    payout_is_maxed <- current_funds >= max_payout
    bet_is_too_small <- new_bet < min_bet

    if (payout_is_maxed || bet_is_too_small) {
      break
    }

    reserve <- current_funds - new_bet

    # 1 for heads, 0 for tails
    current_factor <- ifelse(res, head_factor, tails_factor)

    results <- c(results, reserve + (new_bet * current_factor))

    results[length(results)] <- min(results[length(results)], max_payout)
  }

  return(
    list(
      results = results,
      toss_results = toss_results,
      payout = results[length(results)]
    )
  )
}
