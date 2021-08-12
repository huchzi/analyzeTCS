merge_norm_values <- function(dataTab, 
                              norm_values = "normal_median",
                              age_field = "age_exam") {
  
  stopifnot(is.data.table(dataTab))
  stopifnot(nrow(dataTab) > 0)
  
  stopifnot(exists(norm_values))
  norm_median <- get(norm_values)
  stopifnot(is.data.table(norm_median))
  
  stopifnot(age_field %in% colnames(dataTab))
  
  result <- merge(dataTab, 
                  norm_median, 
                  by = c("stimulus_type", "frequency"))
  result[, log_norm45 := log10(norm45)]
  result[, log_norm := log_norm45 - .01 * (34 - 45)]
  result[, defect := log_sensitivity - log_norm]
  
  result
  
}
