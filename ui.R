library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Transport Health"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("User Manual"),
      menuItem("DAG", tabName = "dag"),
      menuItem("Data", tabName = "data"),
      menuItem("Model", tabName = "model"),
      menuItem("Results", tabName = "results"),
      menuItem("Technical Overview")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dag",
              h2("DAG Tab Content")),
      
      tabItem(tabName = "data",
              fileInput("fileInput", "Upload CSV File", accept = c(".csv")),
              selectInput("method", "Select Method", 
                          choices = c("IOPW", "G-Computation", "MAIC", "NMI")),
              uiOutput("dataUI")),
      
      tabItem(tabName = "model",
              uiOutput("modelUI")),
      
      tabItem(tabName = "results",
              uiOutput("resultsUI"))
    )
  )
)
