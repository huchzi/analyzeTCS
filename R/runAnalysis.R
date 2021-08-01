#' @export
runAnalysis <- function() {

  appDir <- system.file("shiny-examples", "analyzeTCS", "app.R", package = "analyzeTCS")

  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `analyzeTCS`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
