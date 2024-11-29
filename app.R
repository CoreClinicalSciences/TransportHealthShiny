library(shiny)
library(bslib)
library(shinydashboard)
library(reactlog)

# Define UI and server logic in separate files
source("ui.R")
source("server.R")

reactlog_enable()

shinyApp(ui, server)

reactlogShow()
