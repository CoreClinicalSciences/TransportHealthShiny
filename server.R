library(shiny)
library(shinydashboard)

server <- function(input, output, session) {
  
  studyData <- reactive({
    req(input$studyDataInput)
    read.csv(input$studyDataInput$datapath)
  })
  
  targetData <- reactive({
    req(input$targetDataInput)
    read.csv(input$targetDataInput$datapath)
  })
  
  observeEvent(input$method, {
    if(input$method == "IOPW") {
      output$modelUI <- renderUI({
        #Get shared variable names between the data sets
          req(studyData(), targetData())
          commonVars <- intersect(names(studyData()), names(targetData()))
          
          ### Could add an error message here if the intersection is empty ###
        
        fluidPage(
          h3("Model Inputs"),
          selectInput("propensityModel", "Specify Variables in Propensity Model", choices = names(studyData()), multiple = TRUE),
          selectInput("participationModel", "Specify Variables in Participation Model", choices = commonVars, multiple = TRUE),
          selectInput("msmModel", "Specify Variables in MSM", choices = names(studyData()), multiple = TRUE)
        )
      })
      
      output$resultsUI <- renderUI({
        fluidPage(
          h3("Results"),
          tableOutput("resultsTable"),
          fluidRow(
            column(6, plotOutput("propensityHistOutput")),
            column(6, plotOutput("participationHistOutput"))
          ),
          fluidRow(
            column(6, plotOutput("propensitySMDOutput")),
            column(6, plotOutput("participationSMDOutput"))
          )
        )
      })
      
      output$resultsTable <- renderTable({
        head(studyData())  # Placeholder for actual results table
      })
      
      output$propensityHistOutput <- renderPlot({
        hist(rnorm(100), main = "Propensity Histogram")  # Placeholder plot
      })
      
      output$participationHistOutput <- renderPlot({
        hist(rnorm(100), main = "Participation Histogram")  # Placeholder plot
      })
      
      output$propensitySMDOutput <- renderPlot({
        plot(rnorm(100), main = "Propensity SMD")  # Placeholder plot
      })
      
      output$participationSMDOutput <- renderPlot({
        plot(rnorm(100), main = "Participation SMD")  # Placeholder plot
      })
    } else {
      output$modelUI <- renderUI({ NULL }) # Placeholder for the other methods' UI
      output$resultsUI <- renderUI({ NULL }) # Placeholder for the other methods' UI
    }
  })
  
}
