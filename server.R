library(shiny)
library(shinydashboard)
library(TransportHealthR)
library(quarto)
library(flextable)
library(DT)
library(modelsummary)

source("CCSTheme.R")

server <- function(input, output, session) {
  familyList <- list(stats::gaussian, stats::binomial, "survreg", "coxph")
  names(familyList) <- c("Continuous - Normal", "Binary - Logistic", "Survival - AFT", "Survival - Cox PH")
  
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
          column(12, align = "center", plotOutput("resultsPlot")),
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
    # START: Module 2 G-computation functionality --------------------------------------
    else if (input$method == "G-Computation") {
      output$dataUI <- renderUI({
        fluidPage(
          selectInput("inputType", "What data are you inputting?",
                      choices = c("", "Source", "Target + Prepared model"))
      )})
      
      observeEvent(input$inputType, {
        if (input$inputType == "Source") {
          output$dataUI <- renderUI({
            fluidPage(
              selectInput("inputType", "What data are you inputting?",
                          choices = c("Source", "", "Target + Prepared model")),
              fileInput("studyDataInput", "Upload data", accept = ".csv")
            )})
          
          output$modelUI <- renderUI({
            #Get variable names from source data
            req(studyData())
            availableVars <- names(studyData())
            
            ### Could add an error message here if the intersection is empty ###
            
            fluidRow(
              column(width = 4,
                     fluidPage(selectInput("response", "Response variable", choices = names(studyData()), multiple = FALSE),
                               selectInput("treatment", "Treatment variable", choices = names(studyData()), multiple = FALSE),
                               selectInput("covariates", "Covariates", choices = names(studyData()), multiple = TRUE),
                               selectInput("effectModifiers", "Effect modifiers", choices = names(studyData()), multiple = TRUE),
                               selectInput("responseType", "Type of the response variable and regression model",
                                           choices = c("Continuous - Normal", "Binary - Logistic", "Ordinal - Logistic", "Survival - AFT", "Survival - Cox PH")),
                               checkboxInput("wipe", "Erase source data - SEs may be invalid if erased")
                     )
              ),
              column(width = 4,
                     downloadButton("preparedModelDownload", "Download prepared model")
                     )
              )}
            )
            
          output$preparedModelDownload <- downloadHandler(filename = "preparedModel.rds",
                                                            content = function (file) {saveRDS(preparedModel(), file)})
        }
        else if (input$inputType == "Target + Prepared model") {
          output$dataUI <- renderUI({
            fluidPage(
              selectInput("inputType", "What data are you inputting?",
                          choices = c("Target + Prepared model", "", "Source")),
              fileInput("targetDataInput", "Upload data", accept = ".csv"),
              fileInput("preparedModelInput", "Upload prepared model", accept = ".rds")
            )})
          
          output$modelUI <- renderUI({
            fluidPage(selectInput("effectType", "Choose the type of the average treatment effect to be calculated",
                      choices = c("Mean/risk difference", "Relative risk", "Odds ratio", "Hazard ratio")))
          })
        }
        })
      
      studyData <- reactive({
        req(input$studyDataInput)
        read.csv(input$studyDataInput$datapath)
      })
      
      preparedModel <- reactive({
        outcomeFormula <- paste0(input$response, " ~ ", input$treatment)
        baseTerms <- union(input$covariates, input$effectModifiers)
        outcomeFormula <- paste0(outcomeFormula, " + ", paste(baseTerms, collapse = " + "))
        outcomeFormula <- paste0(outcomeFormula, " + ", paste(paste0(input$treatment, ":", input$effectModifiers), collapse = " + "))
        
        data <- studyData()
        data[[input$treatment]] <- factor(data[[input$treatment]])
        
        transportGCPreparedModel(outcomeModel = as.formula(outcomeFormula),
                                 response = input$response,
                                 treatment = input$treatment,
                                 family = familyList[[input$responseType]],
                                 studyData = data,
                                 wipe = input$wipe)
      })
      
      targetData <- reactive({
        req(input$targetDataInput)
        read.csv(input$targetDataInput$datapath)
      })
      
      uploadedPreparedModel <- reactive({
        req(input$preparedModelInput)
        readRDS(input$preparedModelInput$datapath)
      })
      
      outcomeTypes <- c("meanDiff", "rr", "or", "hr")
      names(outcomeTypes) <- c("Mean/risk difference", "Relative risk", "Odds ratio", "Hazard ratio")
      
      resultGC <- reactive({
        transportGC(effectType = outcomeTypes[[input$effectType]],
                    preparedModel = uploadedPreparedModel(),
                    targetData = targetData())
      })
      
      output$resultsUI <- renderUI({
        fluidPage(selectInput("resultType", "What results would you like to view?",
                  choices = c("", "Prepared model", "Transported effect")))
      })
      
      observeEvent(input$resultType, {
        if (input$resultType == "Prepared model") {
          output$preparedModelSummary <- renderDT({
            datatable(summary(preparedModel()$outcomeModel)$coefficients,
                      escape = F,
                      options = list(paging = F,
                                     searching = F,
                                     info = F,
                                     autowidth = F),
                      class = "display",
                      rownames = T)
          })
          
          output$preparedModelCoefPlot <- renderPlot(modelplot(preparedModel()$outcomeModel) + CCS_theme(scale_type = "color"))
          
          # Decide what to do for residuals with polr
          #output$preparedModelResidPlot <- ggplot()
          
          output$resultsUI <- renderUI({
            fluidPage(selectInput("resultType", "What results would you like to view?",
                      choices = c("Prepared model", "", "Transported effect")),
                      h3(tags$b("Coefficient estimates of outcome model")),
                      column(12, align = "center", DTOutput("preparedModelSummary")),
                      h3(tags$b("Coefficient plot of outcome model")),
                      column(12, align = "center", plotOutput("preparedModelCoefPlot"))
                      )
          })
        } else if (input$resultType == "Transported effect") {
          output$effect <- renderText(paste0("Transported average treatment effect estimate: ", resultGC()$effect))
          output$se <- renderText(paste0("Standard error: ", sqrt(resultGC()$var)))
          output$effectType <- renderText(paste0("Effect type: ", input$effectType))
          
          output$atePlot <- renderPlot(plot(resultGC()) + CCS_theme(scale_type = "color"))
          
          output$resultsUI <- renderUI({
            fluidPage(selectInput("resultType", "What results would you like to view?",
                                  choices = c("Transported effect", "Prepared model", "")),
                      h3(tags$b("Coefficient estimates of outcome model")),
                      textOutput("effect"),
                      textOutput("se"),
                      textOutput("effectType"),
                      h3(tags$b("Coefficient plot of outcome model")),
                      column(12, align = "center", plotOutput("atePlot"))
            )
          })
        }
      })
    }
    # END: Module 2 G-computation functionality -----------------------------------
    else {
      output$modelUI <- renderUI({ NULL }) # Placeholder for the other methods' UI
      output$resultsUI <- renderUI({ NULL }) # Placeholder for the other methods' UI
    }
  })
  
}
