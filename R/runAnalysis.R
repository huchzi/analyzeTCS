#' @export
runAnalysis <- function() {
  appDir <- system.file("shiny-examples", "analyzeTCS", "app.R", package = "analyzeTCS")
  print(appDir)
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
