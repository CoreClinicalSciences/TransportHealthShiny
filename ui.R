library(shiny)
library(bslib)
library(DT)

# Define Theme ------------------------------------------------------------

app_theme = bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#000000",
  primary = "#462A79",
  secondary = "#BDDD21",
  success = "#423F85",
  info = "#39548C",
  warning = "#218E8B",
  danger = "#61CA5F",
  base_font = font_google("Roboto"),
  heading_font = font_google("Lato"),
  "navbar-bg" = "lightgrey"
)



# Create objects for UI ---------------------------------------------------


#generate single object containing both elements of model specification for sidebar_layout
methodSelectionUI <- tagList(
  selectInput("method", "Select Method", 
              choices = c("", "IOPW", "G-Computation", "TADA", "NMI")),
  uiOutput("dataUI")
)

#creating accordion for results panel

accordion_results <- accordion(
  accordion_panel(
    "MSM Coefficients",
    "this is what these mean"
  ),
  accordion_panel(
    "SMD Plots",
    "this is what these mean"
  ),
  accordion_panel(
  "Mirrored Histograms",
  "this is what those mean")
)



# UI ----------------------------------------------------------------------

ui <- page_navbar(
  theme = app_theme,
  title = "TransportHealth",
  #bg = "white",
  #inverse = FALSE,

  #Intro Nav Panel  
  nav_panel(title = "Get Started", id = "home", p(
    uiOutput("getStarted")
  )),

  #Model specification panel
  nav_panel(
    title = "Data & Model", 
    id = "dataModel", 
    layout_sidebar(
      sidebar = methodSelectionUI,
        uiOutput("modelUI")
      #)
    )
  ),
  
  
  #results panel
  nav_panel(title = "Results", id = "results", p(
    
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
      )))))))