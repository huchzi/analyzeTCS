#' @export
show_plot <- function (t) {
  ggplot(t, aes(x = frequency, y = sensitivity, color = stimulus_type,
                group = date_of_exam)) +
    geom_point() +
    geom_line() +
    scale_x_log10() +
    scale_y_log10() +
    scale_color_manual(values = c("L-cone" = "red",
                                  "M-cone" = "green",
                                  "S-cone" = "blue",
                                  "Rod" = "orange"))
}
