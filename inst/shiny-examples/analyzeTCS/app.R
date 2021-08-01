library(shiny)
library(shinydashboard)
library(flickerbox)
library(ggplot2)

source("server.R")
source("ui.R")

# Run the application
shinyApp(
    ui = ui,
    server = server
)
