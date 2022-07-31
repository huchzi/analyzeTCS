#' @export
getThreshold <- function(which_staircases, test_field, parsedTCS)
{
  stopifnot(which_staircases %in% c("both", "first", "second", "gamut", "none"))
  stopifnot(test_field %in% c("inner", "outer"))

  if(which_staircases == "both" & nrow(parsedTCS$thresholds) < 2) stop ("Less than two thresholds available.")
  if(which_staircases %in% c("first", "second") & nrow(parsedTCS$thresholds) < 1) stop ("No staircase available.")

  if (which_staircases == "both") return(apply(parsedTCS$thresholds, 2, mean))
  if (which_staircases == "first") return(unlist(parsedTCS$thresholds[1, ]))
  if (which_staircases == "second") return(unlist(parsedTCS$thresholds[2, ]))

  if (which_staircases == "none") {
    contrasts <- rep(NA, 4)
    names(contrasts) <- c("red", "green", "blue", "cyan")
    return(contrasts)
  }

  if (which_staircases == "gamut")
  {
    contrasts <- sapply(parsedTCS$responses_1[, -1], as.numeric)
    contrasts <- contrasts[, grepl(paste0("^", test_field), colnames(contrasts))]
    contrasts <- apply(contrasts, 2, max)
    contrast_names <- sub(paste0(test_field, "_"), "", names(contrasts))
    if (max(contrasts) > 0) contrasts <- 100 * contrasts / max(contrasts) else contrasts <- rep(NA, 4)
    names(contrasts) <- contrast_names
    contrasts
  }
}
