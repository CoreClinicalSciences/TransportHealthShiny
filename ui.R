library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(tags$li(
                    class = "dropdown",
                    tags$img(src = "CCSLogoCropped.png", height = 50)
                  ),
                  title = p("TransportHealth", style = "color: #FFFFFF; font = Lato")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Get Started", tabName = "home"),
      menuItem("DAG", tabName = "dag"),
      menuItem("Data & Model", tabName = "dataModel"),
      menuItem("Results", tabName = "results")
    )
  ),
  dashboardBody(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    tabItems(
      tabItem(tabName = "home",
              uiOutput("getStarted")),
      tabItem(tabName = "dag",
              h2("DAG Tab Content")),
      # We are forcing users to provide study and target data separately. The package supports both separated and merged datasets.
      tabItem(tabName = "dataModel",
              fluidRow(
                column(width = 3,
                       fluidPage(selectInput("method", "Select Method", 
                                              choices = c("", "IOPW", "G-Computation", "TADA", "NMI")),
                                uiOutput("dataUI"))),
                column(width = 8, uiOutput("modelUI"))
                      )
              ),
      
      tabItem(tabName = "results",
              uiOutput("resultsUI"))
    )
  )
)
