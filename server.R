library(shiny)
library(shinydashboard)
library(TransportHealthR)
library(quarto)
library(flextable)

server <- function(input, output, session) {
  
  output$getStarted <- renderUI({suppressWarnings(includeHTML("GetStarted.html"))})
  
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
      output$dataUI <- renderUI({
        fluidPage(
          fileInput("studyDataInput", "Upload Study Data (CSV File)", accept = c(".csv")),
          fileInput("targetDataInput", "Upload Target Data (CSV File)", accept = c(".csv")))
      })
      output$modelUI <- renderUI({
        #Get shared variable names between the data sets
          req(studyData(), targetData())
          commonVars <- intersect(names(studyData()), names(targetData()))
          
          ### Could add an error message here if the intersection is empty ###
        
        fluidRow(
          column(width = 4,
            fluidPage(selectInput("response", "Specify the response variable", choices = names(studyData()), multiple = FALSE),
                      selectInput("treatment", "Specify the treatment variable", choices = names(studyData()), multiple = FALSE)
            )
          ),
          column(width = 4,
            fluidPage(selectInput("propensityModel", "Specify Variables in Propensity Model", choices = names(studyData()), multiple = TRUE),
                      selectInput("participationModel", "Specify Variables in Participation Model", choices = commonVars, multiple = TRUE),
                      selectInput("msmModel", "Specify Variables in MSM", choices = names(studyData()), multiple = TRUE),
                      selectInput("responseType", "Specify the type of the response variable and regression model to be fit",
                                  choices = c("Continuous - Normal", "Binary - Logistic", "Ordinal - Logistic", "Survival - AFT", "Survival - Cox PH"))
                      )
          )
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
          h3(tags$b("Marginal structural model coefficient estimates")),
          column(12, align = "center", tableOutput("resultsTable")),
          column(12, align= "center", plotOutput("resultsPlot")),
          h3(tags$b("Mirrored histograms")),
          fluidRow(
            column(6, fluidPage(plotOutput("propensityHistOutput"), span(p("Propensity", align = "center")))),
            column(6, fluidPage(plotOutput("participationHistOutput"), span(p("Participation", align = "center"))))
          ),
          h3(tags$b("Standardized mean differences plots")),
          fluidRow(
            column(6, fluidPage(plotOutput("propensitySMDOutput"), span(p("Propensity", align = "center")))),
            column(6, fluidPage(plotOutput("participationSMDOutput"), span(p("Participation", align = "center"))))
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
      
      output$resultsPlot <- renderPlot({
        plot(resultIP(), type = "msm")
      })
    } else {
      output$modelUI <- renderUI({ NULL }) # Placeholder for the other methods' UI
      output$resultsUI <- renderUI({ NULL }) # Placeholder for the other methods' UI
    }
  })
  
}
