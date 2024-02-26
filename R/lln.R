#' Visualize the behavior of the Law of Large Numbers
#' with a given function compared to a normal distribution.
#'
#' @param animate Whether the returned plot should animate the draws
#' @param n The number of draws to be made
#' @param theoretical_mean The theoretical mean of the function
#' @param random_function The function from which samples are to be drawn
#' @param ... Additional arguments to pass to the random function
#' @return a plot or animated plot
#'
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' draw_lln_with_func_facet(
#'   animate = FALSE, n = 400,
#'   theoretical_mean = 2.5, rgamma, shape = 10, scale = 0.25
#' )
#' draw_lln_with_func_facet(
#'   animate = FALSE, n = 400,
#'   theoretical_mean = 2.5, stats::rgamma, shape = 10, scale = 0.25
#' )
#' draw_lln_with_func_facet(
#'   animate = FALSE, n = 200,
#'   theoretical_mean = 2.5, stats::rcauchy, location = 2.5, scale = 2
#' )
#' draw_lln_with_func_facet(
#'   animate = FALSE, n = 200,
#'   theoretical_mean = 2.5, stats::rcauchy, location = 2.5, scale = 2
#' )
#' draw_lln_with_func_facet(
#'   animate = TRUE, n = 200,
#'   theoretical_mean = 2.5, stats::rcauchy, location = 2.5, scale = 2
#' )
draw_lln_with_func_facet <- function(
    animate = FALSE,
    n = 1000, theoretical_mean,
    random_function, ...) {
  provided_function_name <- ifelse(is.call(random_function),
    deparse(random_function[[1]]),
    deparse(substitute(random_function))
  )
  # print(provided_function_name)

  # build a dataframe of draws, value and average
  # iteratively populate the dataframe.
  #  the draws will increase from 1 to n,
  #  the value will be a random draw from a normal (0,1)
  # and the average will be the
  # average of the elements from 1 to the current draw
  df <- data.frame(
    draw = 1:n,
    value = random_function(n, ...),
    average = NA, mean = theoretical_mean,
    type = provided_function_name, lower = 0, upper = 0
  )
  for (i in 1:n) {
    df$average[i] <- mean(df$value[1:i])
    variance <- stats::var(df$value[1:i])
    interval <- 1.96 * sqrt(variance / i)
    df$lower[i] <- mean(df$value[1:i]) - interval
    df$upper[i] <- mean(df$value[1:i]) + interval
  }
  # df |> head()

  reference_distribution_label <- "Normal(0,1)"
  df2 <- data.frame(
    draw = 1:n,
    value = stats::rnorm(n, 0, 1),
    average = NA, mean = 0,
    type = reference_distribution_label,
    lower = 0,
    upper = 0
  )
  for (i in 1:n) {
    df2$average[i] <- mean(df2$value[1:i])
    variance <- stats::var(df2$value[1:i])
    interval <- 1.96 * sqrt(variance / i)
    df2$lower[i] <- mean(df2$value[1:i]) - interval
    df2$upper[i] <- mean(df2$value[1:i]) + interval
  }
  # df2 |> head()

  # combine df1 and df2
  big_df <- rbind(df, df2)
  # print(big_df |> head())

  dummy2 <- data.frame(
    type = c(provided_function_name, reference_distribution_label),
    Z = c(theoretical_mean, 0)
  )

  # Plot
  result <- big_df |>
    ggplot2::ggplot(ggplot2::aes(x = .data$draw, y = .data$average, group = 1)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      fill = "blue", alpha = 0.2
    ) +
    # scale_color_viridis(discrete = TRUE) +
    ggplot2::ggtitle(paste("Draws from", provided_function_name)) +
    # theme_ipsum() +
    ggplot2::ylab("Sample Mean") +
    ggplot2::geom_hline(
      data = dummy2, linetype = "dashed", color = "red",
      ggplot2::aes(yintercept = .data$Z)
    ) +
    ggplot2::facet_wrap(~ .data$type, ncol = 1, scales = "free")

  if (animate) {
    result <- result + ggplot2::geom_point()
    result <- result + gganimate::transition_reveal(.data$draw)
    result <- gganimate::animate(result,
      duration = 5, fps = 20, width = 400, height = 400
    )
    # anim_save("output.gif")
  }

  return(result)
}
