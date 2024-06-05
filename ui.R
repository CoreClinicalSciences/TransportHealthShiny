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
      # We are forcing users to provide study and target data separately. The package supports both separated and merged datasets.
      tabItem(tabName = "data",
              fileInput("studyDataInput", "Upload Study Data (CSV File)", accept = c(".csv")),
              fileInput("targetDataInput", "Upload Target Data (CSV File)", accept = c(".csv")),
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
