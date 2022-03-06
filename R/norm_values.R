#' @export
pretty_plot <- function() {
  ggplot2::ggplot(data = analyzeTCS::normal_values,
                  ggplot2::aes(x = frequency, y = sensitivity_for_age)) +
  ggplot2::stat_summary(fun.min = function (x) quantile(x, .05),
                        fun.max = function (x) quantile(x, .95),
                        ggplot2::aes(group = virtual_age),
                        geom = "ribbon", color = "red",
                        fill = "red",
                        alpha = .2) +
  ggplot2::stat_summary(fun = median,
                        fun.min = function (x) quantile(x, .05),
                        fun.max = function (x) quantile(x, .95),
                        geom = "line", color = "red") +
    ggplot2::scale_x_log10(limits = c(1,20)) +
    ggplot2::scale_y_log10(limits = c(1, 500)) +
    ggplot2::facet_grid(stimulus_type ~ cut(virtual_age, breaks = seq(0, 100, 10),
                                              right = FALSE)) +
    theme_bw()
}
