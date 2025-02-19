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
              choices = c("", "IOPW", "G-Computation", "TADA", "Interpolated G-Computation")),
  uiOutput("dataUI")
)



# UI ----------------------------------------------------------------------

ui <- page_navbar(
  theme = app_theme,
  title = div(
    class = "navbar-title-container",
    style = "width: 100%; position: relative; top: 15px",
    span("TransportHealth v1.0.0"),
    tags$img(src = "CCSlogo.png", height = "35px", style = "margin-right: 10px; margin-left: 10px;")
  ),
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
  nav_panel(title = "Results", id = "results", p(uiOutput("resultsUI")))

)