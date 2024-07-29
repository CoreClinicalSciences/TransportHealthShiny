library(shiny)
library(shinydashboard)
library(TransportHealthR)
library(quarto)
library(flextable)
library(DT)

source("CCSTheme.R")

server <- function(input, output, session) {
  
  # Get Started page
  output$getStarted <- renderUI({suppressWarnings(includeHTML("GetStarted.html"))})
  
  observeEvent(input$method, {
    # START: Module 1 IOPW functionality ----------------------------------------------------------
    if(input$method == "IOPW") {
      # Data input UI
      output$dataUI <- renderUI({
        fluidPage(
          fileInput("studyDataInput", "Upload source data (CSV File)", accept = c(".csv")),
          fileInput("targetDataInput", "Upload target data (CSV File)", accept = c(".csv")))
      })
      
      # Read in data
      studyData <- reactive({
        req(input$studyDataInput)
        read.csv(input$studyDataInput$datapath)
      })
      
      targetData <- reactive({
        req(input$targetDataInput)
        read.csv(input$targetDataInput$datapath)
      })
      
      # Model specification UI
      output$modelUI <- renderUI({
        #Get shared variable names between the data sets
          req(studyData(), targetData())
          commonVars <- intersect(names(studyData()), names(targetData()))
          
          ### Could add an error message here if the intersection is empty ###
        
        fluidRow(
          column(width = 4,
            fluidPage(selectInput("response", "Response variable", choices = names(studyData()), multiple = FALSE),
                      selectInput("treatment", "Treatment variable", choices = names(studyData()), multiple = FALSE)
            )
          ),
          column(width = 4,
            fluidPage(selectInput("propensityModel", "Propensity model", choices = names(studyData()), multiple = TRUE),
                      selectInput("participationModel", "Participation model", choices = commonVars, multiple = TRUE),
                      selectInput("msmModel", "MSM", choices = names(studyData()), multiple = TRUE),
                      selectInput("responseType", "Type of the response variable and regression model",
                                  choices = c("Continuous - Normal", "Binary - Logistic", "Ordinal - Logistic", "Survival - AFT", "Survival - Cox PH"))
                      )
          )
        )
      })
      
      # Read in model then fit it
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
      
      # Convert treatment variable to factor in study data for plotting compatibility
      studyDataTrtmtFactor <- studyData()
      
      studyDataTrtmtFactor[[input$treatment]] <- as.factor(studyDataTrtmtFactor[[input$treatment]])
      
      transportIP(msmFormula = as.formula(msmFormulaString),
                              propensityScoreModel = as.formula(propensityFormulaString),
                              participationModel = as.formula(participationFormulaString),
                              family = familyList[[input$responseType]],
                              data = list(studyData = studyDataTrtmtFactor, targetData = targetData()),
                              transport = T)
      })
      
      # Display analysis results
      output$resultsUI <- renderUI({
        fluidPage(
          h3(tags$b("Marginal structural model coefficient estimates")),
          column(12, align = "center", DTOutput("resultsTable")),
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
      
      # Put analysis results in right place for display
      output$resultsTable <- renderDT({
        datatable(summary(resultIP())$msmSummary$coefficients,
                  escape = F,
                  options = list(paging = F,
                                 searching = F,
                                 info = F,
                                 autowidth = F),
                  class = "display",
                  rownames = T)
      })
      
      output$propensityHistOutput <- renderPlot({
        plot(resultIP(), type = "propensityHist") + CCS_theme(scale_type = "fill") + xlab("Propensity score")
      })
      
      output$participationHistOutput <- renderPlot({
        plot(resultIP(), type = "participationHist") + CCS_theme(scale_type = "fill") + xlab("Participation score")
      })
      
      output$propensitySMDOutput <- renderPlot({
        plot(resultIP(), type = "propensitySMD") + CCS_theme(scale_type = "color") + xlab("SMD") + ylab("Variable")
      })
      
      output$participationSMDOutput <- renderPlot({
        plot(resultIP(), type = "participationSMD") + CCS_theme(scale_type = "color") + xlab("SMD") + ylab("Variable")
      })
      
      output$resultsPlot <- renderPlot({
        plot(resultIP(), type = "msm") + CCS_theme(scale_type = "color")
      })
    } 
    # END: Module 1 IOPW functionality -------------------------------------------------
    
    else {
      output$modelUI <- renderUI({ NULL }) # Placeholder for the other methods' UI
      output$resultsUI <- renderUI({ NULL }) # Placeholder for the other methods' UI
    }
  })
  
}
