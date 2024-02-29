#' Create a side-by-side plot of time series and histogram
#'
#' @param df A data frame with columns `round`, `value`, and `index`
#' @param use_log Whether to use a log scale
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
create_side_by_side_plot <- function(df, use_log = TRUE) {
  # handle use_log
  # handle different column names
  time_series_plot <- df |> ggplot2::ggplot(
    ggplot2::aes(
      x = .data$round,
      y = log(.data$value),
      color = .data$index,
      group = .data$index
    )
  ) +
    ggplot2::geom_line(alpha = 0.1) +
    ggplot2::theme(
      legend.position = "none",
      panel.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(0, -0.7, 0, 0, "cm")
    )

  hist_plot <- df |>
    dplyr::group_by(.data$index) |>
    dplyr::summarise(last_value = dplyr::last(.data$value)) |>
    ggplot2::ggplot(ggplot2::aes(log(.data$last_value))) +
    ggplot2::geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
    # geom_line(stat = "density", color = "blue") +
    # move y axis to the right
    # scale_x_continuous(position = "left") +
    # remove background and gridlines
    ggplot2::theme(
      legend.position = "none",
      # panel.background = element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      # axis.line = element_line(color = "black"),
      # panel.border = element_rect(
      #   colour = "black", fill = NA, linewidth = 5
      # ),
      # plot.margin = margin(0, 0, 0, 0, "cm")
    ) +
    ggplot2::xlab(NULL) +
    ggplot2::scale_x_continuous(breaks = NULL) +
    ggplot2::ylab(NULL) +
    ggplot2::scale_y_continuous(breaks = NULL) +
    # move x axis label to top
    # theme(axis.title.x = element_text(margin = margin(t = 10))) +
    ggplot2::coord_flip()

  # show hist plot next to time series plot with 1 row of 2 cols
  egg::ggarrange(time_series_plot, hist_plot,
    ncol = 2, nrow = 1,
    widths = c(2, 1),
    labels = c("Individual Outcomes", "Density of Outcomes")
  )
}
