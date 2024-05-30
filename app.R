library(shiny)
library(shinydashboard)

# Define UI and server logic in separate files
source("ui.R")
source("server.R")

shinyApp(ui, server)