library(shiny)
library(shinydashboard)
library(TransportHealthR)

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
          selectInput("response", "Specify the response variable", choices = names(studyData()), multiple = FALSE),
          selectInput("treatment", "Specify the treatment variable", choices = names(studyData()), multiple = FALSE),
          selectInput("propensityModel", "Specify Variables in Propensity Model", choices = names(studyData()), multiple = TRUE),
          selectInput("participationModel", "Specify Variables in Participation Model", choices = commonVars, multiple = TRUE),
          selectInput("msmModel", "Specify Variables in MSM", choices = names(studyData()), multiple = TRUE),
          selectInput("responseType", "Specify the type of the response variable and regression model to be fit",
                      choices = c("Continuous - Normal", "Binary - Logistic", "Survival - AFT", "Survival - Cox PH"))
        )
      })
      
      resultIP <- reactive({
      if (!is.null(input$msmModel)) 
        msmFormulaString <- paste0(input$response, " ~ ", paste(input$msmModel, collapse = " + "))
      else msmFormulaString <- paste0(input$response, " ~ 1")
      
      if (!is.null(input$propensityModel)) 
        propensityFormulaString <- paste0(input$treatment, " ~ ", paste(input$propensityModel, collapse = " + "))
      else propensityFormulaString <- paste0(input$treatment, " ~ 1")
      
      if (!is.null(input$participationModel))
        participationFormulaString <- paste0("participation ~ ", paste(input$participationModel, collapse = " + "))
      else participationFormulaString <- "participation ~ 1"
      
      familyList <- list(stats::gaussian, stats::binomial, "survreg", "coxph")
      names(familyList) <- c("Continuous - Normal", "Binary - Logistic", "Survival - AFT", "Survival - Cox PH")
      
      transportIP(msmFormula = as.formula(msmFormulaString),
                              propensityScoreModel = as.formula(propensityFormulaString),
                              participationModel = as.formula(participationFormulaString),
                              family = familyList[[input$responseType]],
                              data = list(studyData = studyData(), targetData = targetData()),
                              transport = T)
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
        summary(resultIP())$msmSummary$coefficients # Placeholder for actual results table
      }, rownames = T)
      
      output$propensityHistOutput <- renderPlot({
        plot(resultIP(), type = "propensityHist")  # Placeholder plot
      })
      
      output$participationHistOutput <- renderPlot({
        plot(resultIP(), type = "participationHist")  # Placeholder plot
      })
      
      output$propensitySMDOutput <- renderPlot({
        plot(resultIP(), type = "propensitySMD")  # Placeholder plot
      })
      
      output$participationSMDOutput <- renderPlot({
        plot(resultIP(), type = "participationSMD")  # Placeholder plot
      })
    } else {
      output$modelUI <- renderUI({ NULL }) # Placeholder for the other methods' UI
      output$resultsUI <- renderUI({ NULL }) # Placeholder for the other methods' UI
    }
  })
  
}
