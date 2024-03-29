#' Create a side-by-side plot of time series and histogram
#'
#' @param df A data frame with columns `round`, `value`, and `index`
#' @param use_log Whether to use a log scale
#' @param quantiles A vector of quantiles to show as lines
#'
#' @return A ggplot2 object
#'
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' create_side_by_side_plot(data.frame(
#'   round = 1:100,
#'   value = rnorm(100),
#'   index = "A"
#' ))
#'
create_side_by_side_plot <- function(df, use_log = TRUE, quantiles = c()) {
  if (use_log) {
    df <- df |>
      dplyr::mutate(value = log(.data$value))
  }

  arranged <- df |>
    dplyr::group_by(.data$index) |>
    dplyr::summarise(last_value = dplyr::last(.data$value)) |>
    dplyr::arrange(.data$last_value)

  special_indices <- c()

  for (quantile in quantiles) {
    special_index <- arranged |>
      dplyr::slice(ceiling(quantiles * nrow(arranged))) |>
      dplyr::pull(.data$index)

    special_indices <- c(special_indices, special_index)
  }

  # add boolean column to df indicating if line the 95th percentile line
  df <- df |>
    dplyr::mutate(
      is_special = .data$index %in% special_indices,
      opacity = ifelse(.data$is_special, 1, 0.2),
      color = ifelse(.data$is_special, "black", "blue"),
      size = ifelse(.data$is_special, 1, 1)
    )

  hist_df <- df |>
    dplyr::group_by(.data$index) |>
    dplyr::summarise(last_value = dplyr::last(.data$value))

  # get geometric mean of hist_df$last_value
  # careful here to handle 0s not sure
  # geometric_mean <- exp(mean(log(hist_df$last_value), na.rm = TRUE))
  # print(geometric_mean)

  median <- median(hist_df$last_value)
  mean <- mean(hist_df$last_value)

  print(paste("median", median))
  print(paste("mean", mean))

  min_val <- min(df$value)
  max_val <- max(df$value)

  time_series_plot <- df |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data$round,
        y = .data$value,
        group = .data$index
      ),
    ) +
    ggplot2::geom_line(ggplot2::aes(alpha = .data$opacity)) +
    ggplot2::theme(
      legend.position = "none",
      # panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(0, -1, 0, 0, "cm")
    ) +
    ggplot2::geom_vline(
      xintercept = max(df$round), color = "black", linewidth = 1
    ) +
    ggplot2::ylab(ifelse(use_log, "Log Value", "Value"))



  hist_plot <- hist_df |>
    ggplot2::ggplot(ggplot2::aes(.data$last_value)) +
    ggplot2::geom_histogram(binwidth = 0.5, fill = "blue", alpha = 0.4) +
    ggplot2::theme(
      legend.position = "none",
      # panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank()
    ) +
    ggplot2::xlab(NULL) +
    ggplot2::scale_x_continuous(
      limits = c(min_val, max_val),
      position = "top"
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::geom_vline(
      xintercept = mean, color = "red", linewidth = 1, linetype = "dashed"
    ) +
    ggplot2::geom_vline(
      xintercept = median, color = "black", linewidth = 1, linetype = "solid"
    ) +
    ggplot2::coord_flip()

  # show hist plot next to time series plot with 1 row of 2 cols
  egg::ggarrange(time_series_plot, hist_plot,
    ncol = 2, nrow = 1,
    widths = c(2, 1),
    labels = c("-  Outcome Paths", "-  Density of Outcomes")
  )
}
