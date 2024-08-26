library(shiny)
#library(shinydashboard)
library(TransportHealth)
library(quarto)
library(flextable)
library(DT)
library(bslib)
library(ggplot2)
library(thematic)

source("CCSTheme.R")
#thematic::thematic_shiny(font = "auto")

server <- function(input, output, session) {
  #bslib::bs_themer()
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
      
      #creating accordion for results panel
      
      accordion_results <- accordion(
        accordion_panel(
          "MSM Coefficients",
          "This tab shows estimates of the coefficients in the specified marginal structural model and their estimated standard errors in table form and plot form. Conclusions should be drawn from the estimates in this table."
        ),
        accordion_panel(
          "SMD Plots",
          "This tab shows standardized mean differences (SMD) plots, or Love plots, for covariates between treatment groups (propensity) and effect modifiers between source and target data (participation). This is used to assess if covariate balance has been adequately achieved after weighting to determine if the propensity and participation models are correctly specified."
        ),
        accordion_panel(
          "Mirrored Histograms",
          "This tab shows mirrored histograms of the probability of being in the treatment group between treatment groups (propensity) and of the probability of being in the source dataset between the source and target data (participation). This is used to assess positivity.")
      )
      
      # Display analysis results
      output$resultsUI <- renderUI({
        layout_sidebar(
          sidebar = sidebar(
            title = "Understanding Your Results",
            accordion_results
          ),
          card(
            min_height="400px",
            card_header(
              class = "bg-secondary",
              "Marginal Structural Model (MSM) Coefficient Estimates"),    
            navset_card_tab(
              nav_panel(
                "Table",
                DTOutput("resultsTable")
              ),
              nav_panel(
                "Plot",
                plotOutput("resultsPlot")
              )
            )),
          card(
            min_height = "475px",
            card_header(
              class = "bg-secondary",
              "Standardized Mean Difference (SMD) Plots"),
            layout_column_wrap(
              min_width = "400px",
              min_height = "400px",
              card(
                "Propensity",
                plotOutput("propensitySMDOutput")
              ),
              card (
                "Participation",
                plotOutput("participationSMDOutput")
              )
            )
          ),
          card(
            min_height = "475px",
            card_header(
              class = "bg-secondary",
              "Mirrored Histograms"),
            layout_column_wrap(
              min_width = "400px",
              min_height = "400px",
              card(
                "Propensity Histogram",
                plotOutput("propensityHistOutput")
              ),
              card (
                "Participation Histogram",
                plotOutput("participationHistOutput")
              ))))
      })
      
      # Put analysis results in right place for display
      output$resultsTable <- renderDT({
        # Extract coefficients and round them to 2 decimal places
        data <- summary(resultIP())$msmSummary$coefficients
        
        # Render the DataTable with rounded data
        datatable(data,
                  escape = F,
                  options = list(paging = F,
                                 searching = F,
                                 info = F,
                                 width = '100%'),  # Set table width to 100%
                  class = "display",
                  rownames = T,
                  fillContainer = TRUE) %>%
          formatRound(columns = c("Estimate", "Std. Error", "t value", "Pr(>|t|)"), digits = 2)  # Specify numeric columns by name
      })
      
      output$propensityHistOutput <- renderPlot({
        plot(resultIP(), type = "propensityHist") + CCS_theme(scale_type = "fill") + xlab("Propensity score") + theme_minimal(base_family="Roboto")
      })
      
      output$participationHistOutput <- renderPlot({
        plot(resultIP(), type = "participationHist") + CCS_theme(scale_type = "fill") + xlab("Participation score") + theme_minimal(base_family="Roboto")
      })
      
      output$propensitySMDOutput <- renderPlot({
        plot(resultIP(), type = "propensitySMD") + CCS_theme(scale_type = "color") + xlab("SMD") + ylab("Variable") + theme_minimal(base_family="Roboto")
      })
      
      output$participationSMDOutput <- renderPlot({
        plot(resultIP(), type = "participationSMD") + CCS_theme(scale_type = "color") + xlab("SMD") + ylab("Variable") + theme_minimal(base_family="Roboto")
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
