#' Get Sequence of Coin Tosses and corresponding payouts
#' @param rounds Number of rounds to simulate
#' @param heads_prob Probability of heads
#' @param starting_capital Initial amount of money
#' @param head_factor Factor to multiply bet by if heads
#' @param tails_factor Factor to multiply bet by if tails
#' @param min_bet Minimum bet
#' @param max_payout Maximum payout
#' @param betting_fraction Fraction of funds to bet
#'
#' @return A list with the results (list of the payout after each round),
#'  toss results (list of coin toss results), and payout (integer)
#' @export
#'
#' @examples
#' coin_toss_seq(10, 0.5, 100, 2, 0.5, 1, 1000, 0.5)
coin_toss_seq <- function(
    rounds = 1,
    heads_prob = 0.5,
    starting_capital = 100,
    head_factor = 2,
    tails_factor = 0.5,
    min_bet = 0,
    max_payout = Inf,
    betting_fraction = 1) {
  toss_results <- stats::rbinom(rounds, size = 1, prob = heads_prob)

  results <- calc_score_for_coin_game(
    toss_results = toss_results,
    starting_capital = starting_capital,
    head_factor = head_factor,
    tails_factor = tails_factor,
    min_bet = min_bet,
    max_payout = max_payout,
    betting_fraction = betting_fraction
  )

  return(
    list(
      results = results,
      payout = results[length(results)],
      toss_results = toss_results
    )
  )
}


#' Perform the coin toss game, given a heads/tails sequence
#' @param toss_results A sequence of 1s and 0s, representing heads and tails
#' @param starting_capital Initial amount of money
#' @param head_factor Factor to multiply bet by if heads
#' @param tails_factor Factor to multiply bet by if tails
#' @param min_bet Minimum bet
#' @param max_payout Maximum payout
#' @param betting_fraction Fraction of funds to bet
#' @return A list with the results (list of the payout after each round),
#' toss results (list of coin toss results), and payout (integer)
#' @export
#' @examples
#' get_coin_game_results(0:1, 100, 2, 0.5, 1, 1000, 0.5)
get_coin_game_results <- function(
    toss_results,
    starting_capital = 100,
    head_factor = 2,
    tails_factor = 0.5,
    min_bet = 0,
    max_payout = Inf,
    betting_fraction = 1) {
  results <- calc_score_for_coin_game(
    toss_results = toss_results,
    starting_capital = starting_capital,
    head_factor = head_factor,
    tails_factor = tails_factor,
    min_bet = min_bet,
    max_payout = max_payout,
    betting_fraction = betting_fraction
  )

  return(
    list(
      results = results,
      payout = results[length(results)]
    )
  )
}

#' Calculate the payout after each round of the coin toss game
#' @param toss_results A sequence of 1s and 0s, representing heads and tails
#' @param starting_capital Initial amount of money
#' @param head_factor Factor to multiply bet by if heads
#' @param tails_factor Factor to multiply bet by if tails
#' @param min_bet Minimum bet
#' @param max_payout Maximum payout
#' @param betting_fraction Fraction of funds to bet
#' @return A list of the payout after each round
#' @export
#'
#' @examples
#' calc_score_for_coin_game(0:1, 100, 2, 0.5, 1, 1000, 0.5)
calc_score_for_coin_game <- function(
    toss_results,
    starting_capital = 100,
    head_factor = 2,
    tails_factor = 0.5,
    min_bet = 0,
    max_payout = Inf,
    betting_fraction = 1) {
  # treat coin toss game with heads factor and tails factor as 2-sided die game
  # return(calc_score_for_die(
  #   toss_results = toss_results,
  #   starting_capital = starting_capital,
  #   payoffs = c(head_factor, tails_factor),
  #   min_bet = min_bet,
  #   max_payout = max_payout,
  #   betting_fraction = betting_fraction
  # ))

  if (starting_capital < 0) {
    stop("starting_capital must be positive")
  }
  if (head_factor < 0) {
    stop("head_factor must be positive")
  }
  if (tails_factor < 0) {
    stop("tails_factor must be positive")
  }
  if (min_bet < 0) {
    stop("min_bet must be positive")
  }
  # betting_fraction must be between 0 and 1 for now, could add leverage later
  if (betting_fraction < 0 || betting_fraction > 1) {
    stop("betting_fraction must be between 0 and 1")
  }

  expected_num_values <- length(toss_results) + 1

  results <- c(starting_capital)
  for (res in toss_results) {
    current_funds <- results[length(results)]
    new_bet <- current_funds * betting_fraction

    payout_is_maxed <- current_funds >= max_payout
    bet_is_too_small <- new_bet < min_bet

    if (payout_is_maxed || bet_is_too_small) {
      # fill in the rest with current value
      if (length(results) < expected_num_values) {
        results <- c(results, rep(
          results[length(results)],
          expected_num_values - length(results)
        ))
      }

      break
    }

    reserve <- current_funds - new_bet

    # 1 for heads, 0 for tails
    current_factor <- ifelse(res, head_factor, tails_factor)

    results <- c(results, reserve + (new_bet * current_factor))

    results[length(results)] <- min(results[length(results)], max_payout)
  }

  return(results)
}



#' Generate all possible coin toss sequences
#' @param flips Number of coin flips
#' @return A matrix with all possible coin toss sequences
#' @export
#'
#' @examples
#' generate_coin_sequences(2)
generate_coin_sequences <- function(flips) {
  # cant swap this yet because the rolls are based on 0, 1 not 1,2
  # return(generate_die_sequences(rolls = flips, sides = 2))
  if (flips < 0) {
    stop("num_rolls must be positive")
  }
  outcomes <- c(0, 1)
  sequences <- as.matrix(
    expand.grid(replicate(flips, outcomes, simplify = FALSE))
  )
  return(sequences)
}

#' Generate all possible die sequences
#' @param rolls Number of rolls
#' @param sides Number of sides
#' @return A matrix with all possible coin toss sequences
#' @export
#'
#' @examples
#' generate_coin_sequences(2)
generate_die_sequences <- function(rolls, sides = 6) {
  if (rolls < 0) {
    stop("num_rolls must be positive")
  }
  if (sides < 1) {
    stop("sides must be >= 1")
  }
  outcomes <- seq(from = 1, to = sides)
  sequences <- as.matrix(
    expand.grid(replicate(rolls, outcomes, simplify = FALSE))
  )
  return(sequences)
}


#' Generate all possible die sequences
#' @param rolls Number of rolls
#' @param sides Number of sides
#' @param payoffs a list of the payoffs, one for each side of the die
#' @param starting_capital Initial amount of money
#' @param min_bet Minimum bet
#' @param max_payout Maximum payout
#' @param betting_fraction Fraction of funds to bet
#' @return A matrix with all possible coin toss sequences
#' @export
#'
#' @examples
#' die_game_seq(2, 6, c(2, 0.5, 1, 1, 1, 1), 100, 1, 1000, 0.5)
die_game_seq <- function(
    rolls,
    sides = 6,
    payoffs,
    starting_capital = 100,
    min_bet = 0,
    max_payout = Inf,
    betting_fraction = 1) {
  if (rolls < 0) {
    stop("num_rolls must be positive")
  }
  if (sides < 1) {
    stop("sides must be >= 1")
  }
  if (length(payoffs) != sides) {
    stop("Length of payoffs must equal sides.
    There must be a payoff for each side.")
  }
  # get sequence of rolls, each with equal likelihood of landing on any face

  toss_results <- sample(1:sides, size = rolls, replace = TRUE)

  results <- calc_score_for_die(
    toss_results = toss_results,
    starting_capital = starting_capital,
    payoffs = payoffs,
    min_bet = min_bet,
    max_payout = max_payout,
    betting_fraction = betting_fraction
  )

  return(
    list(
      results = results,
      payout = results[length(results)],
      toss_results = toss_results
    )
  )
}

#' Calculate the payout after each round of the die game
#' @param toss_results A sequence of integers,
#'  representing the sides of each die roll
#' @param starting_capital Initial amount of money
#' @param payoffs a list of the payoffs, one for each side of the die
#' @param min_bet Minimum bet
#' @param max_payout Maximum payout
#' @param betting_fraction Fraction of funds to bet
#' @return A list of the payout after each round
#' @export
#'
#' @examples
#' calc_score_for_die(1:6, 100, c(2, 0.5, 1, 1, 1, 1), 1, 1000, 0.5)
calc_score_for_die <- function(
    toss_results,
    starting_capital = 100,
    payoffs = c(2, 0.5),
    min_bet = 0,
    max_payout = Inf,
    betting_fraction = 1) {
  if (starting_capital < 0) {
    stop("starting_capital must be positive")
  }
  if (any(payoffs < 0)) {
    stop("all payoffs must be positive")
  }

  if (min_bet < 0) {
    stop("min_bet must be positive")
  }
  # betting_fraction must be between 0 and 1 for now, could add leverage later
  if (betting_fraction < 0 || betting_fraction > 1) {
    stop("betting_fraction must be between 0 and 1")
  }

  expected_num_values <- length(toss_results) + 1

  results <- c(starting_capital)
  for (res in toss_results) {
    current_funds <- results[length(results)]
    new_bet <- current_funds * betting_fraction

    payout_is_maxed <- current_funds >= max_payout
    bet_is_too_small <- new_bet < min_bet

    if (payout_is_maxed || bet_is_too_small) {
      # fill in the rest with current value
      if (length(results) < expected_num_values) {
        results <- c(results, rep(
          results[length(results)],
          expected_num_values - length(results)
        ))
      }

      break
    }

    reserve <- current_funds - new_bet

    current_factor <- payoffs[res]

    results <- c(results, reserve + (new_bet * current_factor))

    results[length(results)] <- min(results[length(results)], max_payout)
  }

  return(results)
}
