#' parse a TCS file
#'
#' @param filename name of the tcs file
#' @return list of properties
#'
parseTCS <- function(filename)
{
  filename <- normalizePath(filename, winslash = "/", mustWork = TRUE)

  tcs_data <- readLines(filename)

  # parse header

  parse_header_line <- function(line, type) {
    element_list <-     scan(text = line,
                             sep = ";",
                             dec = ",",
                             quiet = TRUE,
                             what = list("", NULL, type, type, type, type, NULL, type, type, type, type),
                             fill = TRUE)
    sapply(element_list[c(3:6, 8:11)], c)
  }

  active <-
    parse_header_line(tcs_data[grepl("^Signal aktiv", tcs_data)],logical(0))
  waveform <-
    parse_header_line(tcs_data[grepl("^Signalform", tcs_data)], character(0))
  luminance <-
    parse_header_line(tcs_data[grepl("^Helligkeit", tcs_data)], double(0))
  frequency <-
    parse_header_line(tcs_data[grepl("^Frequenz;", tcs_data)],integer(0))
  phase <-
    parse_header_line(tcs_data[grepl("^Phase", tcs_data)], integer(0))
  initial_contrast_1 <-
    parse_header_line(tcs_data[grepl("^Kontrast SC1", tcs_data)], double(0))
  initial_contrast_2 <-
    parse_header_line(tcs_data[grepl("^Kontrast SC2", tcs_data)], double(0))
  stepsize_contrast_1 <-
    parse_header_line(tcs_data[grepl("^Delta Kontrast SC1", tcs_data)], double(0))
  stepsize_contrast_2 <-
    parse_header_line(tcs_data[grepl("^Delta Kontrast SC2", tcs_data)], double(0))
  if (any(grepl("^Frequenz Envelope", tcs_data)))
    envelope <- parse_header_line(tcs_data[grepl("^Frequenz Envelope", tcs_data)], double(0))[1]
  else
    envelope <- 0
  if (any(grepl("^Pausiere Envelope", tcs_data)))
    pause <- parse_header_line(tcs_data[grepl("^Pausiere Envelope", tcs_data)], logical(0))[1]
  else
    pause <- FALSE

  header <-
    data.frame(field = rep(c("inner", "outer"), each = 4),
               led = c("red", "green", "blue", "cyan"),
               active,
               waveform = factor(waveform,
                                 levels = c("Sinus"),
                                 labels = c("sinus")),
               luminance,
               frequency,
               phase,
               initial_contrast_1,
               stepsize_contrast_1,
               initial_contrast_2,
               stepsize_contrast_2,
               envelope,
               pause)

  # parse responses

  parse_response_line <- function(line) {
    element_list <-     scan(text = line,
                             sep = ";",
                             dec = ",",
                             quiet = TRUE,
                             what = list(character(0),
                                         character(0),
                                         double(0),
                                         double(0),
                                         double(0),
                                         double(0),
                                         NULL,
                                         double(0),
                                         double(0),
                                         double(0),
                                         double(0))
                             )
    sapply(element_list[c(2:6, 8:11)], c)
  }

  staircase_1 <-
    parse_response_line(tcs_data[grepl("^Down:;", tcs_data)])
  staircase_2 <-
    parse_response_line(tcs_data[grepl("^Up:;", tcs_data)])

  responses_1 <-
    data.frame(staircase_1[, 1] == "gesehen",
               staircase_1[, -1])
  responses_2 <-
    data.frame(staircase_2[, 1] == "gesehen",
               staircase_2[, -1])

  names(responses_1) <- c("response",
                          "inner_red",
                          "inner_green",
                          "inner_blue",
                          "inner_cyan",
                          "outer_red",
                          "outer_green",
                          "outer_blue",
                          "outer_cyan"
  )
  names(responses_2) <- names(responses_1)

  # parse thresholds

  parse_treshold_line <- function(line) {

    if (identical(line, character(0))) return(c(NA, NA, NA, NA))

    element_list <-     scan(text = line,
                             sep = ";",
                             dec = ",",
                             quiet = TRUE,
                             what = list(character(0),
                                         NULL,
                                         double(0),
                                         double(0),
                                         double(0),
                                         double(0))
    )

    sapply(element_list[3:6], c)
  }

  threshold_1 <-
    parse_treshold_line(tcs_data[grepl("^Down: Schwelle erreicht!", tcs_data)])
  threshold_2 <-
    parse_treshold_line(tcs_data[grepl("^Up: Schwelle erreicht!", tcs_data)])
  thresholds <- data.frame(rbind(threshold_1, threshold_2))
  names(thresholds) <- c("red", "green", "blue", "cyan")

  # parse gamut / false positives

  parse_gamut_lines <- function(line_numbers) {

    if(identical(line_numbers, integer(0))) return(NULL)

    gamut_line <-     scan(text = tcs_data[line_numbers],
                           sep = ":",
                           dec = ",",
                           quiet = TRUE,
                           what = list(character(0),
                                       integer(0))
    )
    staircase_line <- scan(text = tcs_data[line_numbers + 1],
                           sep = ";",
                           dec = ",",
                           quiet = TRUE,
                           what = list(character(0),
                                       character(0),
                                       double(0),
                                       double(0),
                                       double(0),
                                       double(0),
                                       NULL,
                                       double(0),
                                       double(0),
                                       double(0),
                                       double(0))
    )

    contrasts <- sapply(staircase_line[c(3:6, 8:11)], cbind)

    data.frame(n = as.integer(gamut_line[[2]]),
               staircase = factor(staircase_line[[1]],
                                  levels = c("Down:", "Up:"),
                                  labels = c("staircase_1", "staircase_2")),
               contrast = as.double(apply(contrasts, 1, max)))

  }

  gamut <-
    parse_gamut_lines(grep("^Versuch .*berschreitung", tcs_data))

  false_positives <-
    parse_gamut_lines(grep("^Versuch Unterschreitung", tcs_data))

  # return result

  list(header = header,
       responses_1 = responses_1,
       responses_2 = responses_2,
       thresholds = thresholds,
       gamut = gamut,
       false_positives = false_positives)

}
