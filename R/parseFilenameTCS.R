#' @export
parseFilenameTCS <- function(filename)
{
  filename <- normalizePath(filename, winslash = "/", mustWork = FALSE)
  filename_info <- strsplit(filename, split = "/")[[1]]
  filename_info <- filename_info[length(filename_info)]
  filename_info <- strsplit(substr(filename_info, 1, nchar(filename_info) - 4), split = "_")[[1]]

  patid <- as.integer(filename_info[1])
  eye <- factor(filename_info[length(filename_info) - 2],
                levels = c("OD", "OS"))
  date_time <- strptime(paste(filename_info[length(filename_info) - 1],
                              filename_info[length(filename_info)]),
                        "%Y-%m-%d %H%M%S")
  date_of_exam <- as.character(date_time, format = "%Y-%m-%d")
  time_of_exam <- as.character(date_time, format = "%H:%M:%S")

  return(data.frame(patid, eye, date_of_exam, time_of_exam))
}
