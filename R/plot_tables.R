#' @export
plot_tables <- function(tb, plot_type) {

  tb[, ac_sensitivity := adjust_for_age(sensitivity, age_exam, 40)]
  tb[, ac_log_sens := log10(ac_sensitivity)]
  tb[, type := as.character(factor(stimulus_type, levels = c("L-cone", "M-cone", "S-cone", "Rod"), labels = c("L", "M", "S", "R")))]

  setkey(tb, type, frequency)

  tb <- norm_data$norm_value_dB[tb]

  tb[, deviation := 10 * log10(ac_sensitivity) - norm_value_dB]
  tb[, deviation_plot := as.character(round(deviation,2))]
  tb[used_threshold == "gamut", deviation_plot := "-"]
  tb[, probability := pnorm(deviation, mean = 0, sd = 2.8)]
  tb[, probability_labels := cut(probability, breaks = c(-.1, .005, .01, .05, 1), labels = c("***", "**", "*", "."))]

  if (plot_type == 1)
    kable_minimal(
      kbl(
        dcast(tb, type ~ frequency, value.var = "deviation_plot"),
        col.names = c("Photoreceptor", paste0(c(1, 2, 4, 6, 8, 10, 12, 16, 20), "Hz")),
        caption = "Deviation [dB]",
        digits = 2
      )
    )
  else if (plot_type == 2)
    kable_minimal(
      kbl(
        dcast(tb, type ~ frequency, value.var = "probability"),
        col.names = c("Photoreceptor", paste0(c(1, 2, 4, 6, 8, 10, 12, 16, 20), "Hz")),
        caption = "Probability",
        digits = 2
      )
    )

  else if (plot_type == 3)
    kable_minimal(
      kbl(
        dcast(tb, type ~ frequency, value.var = "probability_labels"),
        col.names = c("Photoreceptor", paste0(c(1, 2, 4, 6, 8, 10, 12, 16, 20), "Hz")),
        caption = "Probability labels",
        digits = 2
      )
    )
}
