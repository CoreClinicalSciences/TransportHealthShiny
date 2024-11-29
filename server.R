library(shiny)
#library(shinydashboard)
library(TransportHealth)
library(quarto)
library(flextable)
library(DT)
library(bslib)
library(ggplot2)
library(thematic)
library(modelsummary)
library(bsicons)

source("CCSTheme.R")
#thematic::thematic_shiny(font = "auto")

server <- function(input, output, session) {

  #bslib::bs_themer()

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
                               checkboxInput("wipe", span("Erase source data - SEs may be invalid if erased",
                                                          tooltip(bs_icon("info-circle"), "Ticking this box will cause the data used to fit the outcome model to be erased from the prepared model object. This means that it will not be possible to resample the source data, thus disabling bootstrapping. Since the app relies on bootstrapping to correctly calculate standard errors, the standard errors of the transported effect estimate may be misleading if this box is ticked.", placement = "bottom")))
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
      
      accordion_results <- accordion(
        accordion_panel(
          "Result Types",
          "Choose \"Prepared model\" to view summary information about the fitted outcome model. Choose \"Transported effect\" to view the transported effect estimate."
        )
      )
      
      output$resultsUI <- renderUI({
      layout_sidebar(
        sidebar = sidebar(
          title = "Understanding Your Results",
          accordion_results
        ),
        card(
          min_height="150px",
          card_header(
            class = "bg-secondary",
            "Result Type"),
          selectInput("resultType", "What results would you like to view?",
                                choices = c("", "Prepared model", "Transported effect"))))
      })
      
      observeEvent(input$resultType, {
        if (input$resultType == "Prepared model") {
          accordion_results <- accordion(
            accordion_panel(
              "Outcome Model Summary",
              "This table displays summary information about the fitted outcome model, including the coefficient estimates and standard errors."
            ),
            accordion_panel(
              "Outcome Model Coefficient Plot",
              "This plot displays the estimates and confidence intervals for the coefficients in the outcome model."
            )
          )
          # if (is.null(uploadedPreparedModel())) {
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
          # } else {
          #   output$preparedModelSummary <- renderDT({
          #     datatable(summary(uploadedPreparedModel()$outcomeModel)$coefficients,
          #               escape = F,
          #               options = list(paging = F,
          #                              searching = F,
          #                              info = F,
          #                              autowidth = F),
          #               class = "display",
          #               rownames = T)
          #   })
          #   
          #   output$preparedModelCoefPlot <- renderPlot(modelplot(uploadedPreparedModel()$outcomeModel) + CCS_theme(scale_type = "color"))
          # }
          
          # Decide what to do for residuals with polr
          #output$preparedModelResidPlot <- ggplot()
          
          output$resultsUI <- renderUI({
            layout_sidebar(
              sidebar = sidebar(
                title = "Understanding Your Results",
                accordion_results
              ),
              card(
                min_height="250px",
                card_header(
                  class = "bg-secondary",
                  "Result Type"),
                selectInput("resultType", "What results would you like to view?",
                            choices = c("Prepared model", "", "Transported effect"))),
              card(
                  min_height="400px",
                  card_header(
                    class = "bg-secondary",
                    "Outcome Model Summary"),
                  DTOutput("preparedModelSummary")),
              card(
                min_height="400px",
                card_header(
                  class = "bg-secondary",
                  "Outcome Model Coefficient Plot"),
                plotOutput("preparedModelCoefPlot")))
          })
        } else if (input$resultType == "Transported effect") {
          accordion_results <- accordion(
            accordion_panel(
              "Transported Effect Estimate",
              "This tab displays the transported effect estimate and its standard error."
            ),
            accordion_panel(
              "Plot of Transported Effect Estimate",
              "This plot displays the transported effect estimate and the associated confidence interval."
            )
          )
          
          output$effect <- renderText(paste0("Transported average treatment effect estimate: ", resultGC()$effect))
          output$se <- renderText(paste0("Standard error: ", sqrt(resultGC()$var)))
          output$effectType <- renderText(paste0("Effect type: ", input$effectType))
          
          output$atePlot <- renderPlot(plot(resultGC()) + CCS_theme(scale_type = "color"))
          
          output$resultsUI <- renderUI({
            layout_sidebar(
              sidebar = sidebar(
                title = "Understanding Your Results",
                accordion_results
              ),
              card(
                min_height="250px",
                card_header(
                  class = "bg-secondary",
                  "Result Type"),
                selectInput("resultType", "What results would you like to view?",
                            choices = c("Transported effect", "Prepared model", ""))),
                card(
                  min_height="250px",
                  card_header(
                    class = "bg-secondary",
                    "Transported Effect Estimate"),
                  p(textOutput("effect"),
                            textOutput("se"),
                            textOutput("effectType"))
                  ),
                card(
                  min_height="400px",
                  card_header(
                    class = "bg-secondary",
                    "Plot of Transported Effect Estimate"),
                  plotOutput("atePlot")))
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
