if(!nzchar(system.file(package = "shiny"))) install.packages("shiny")
if(!nzchar(system.file(package = "bslib"))) install.packages("bslib")
if(!nzchar(system.file(package = "bsicons"))) install.packages("bsicons")
if(!nzchar(system.file(package = "shinyWidgets"))) install.packages("shinyWidgets")
if(!nzchar(system.file(package = "bslib"))) install.packages("bslib")
if(!nzchar(system.file(package = "dplyr"))) install.packages("dplyr")
if(!nzchar(system.file(package = "DT"))) install.packages("DT")
if(!nzchar(system.file(package = "openxlsx"))) install.packages("openxlsx")
if(!nzchar(system.file(package = "ggplot2"))) install.packages("ggplot2")

library(shiny)
library(bslib)
library(bsicons)
library(shinyWidgets)
library(dplyr)
library(DT)
library(openxlsx)
library(ggplot2)
# library(shinythemes)
# thematic::thematic_shiny(font = "auto")

options(warn = -1)
options(shiny.maxRequestSize = 30 * 1024 ^ 2)


# Define your primary color
my_primary_color <-  "#DF6919"  # This is a blue-violet color
theme <- bs_theme()



# Source the module files
source("OralDisintTabletModule.R")
source("ReazDegradForzataModule.R")

# Main application UI
ui <- page_navbar(
  
  theme = bs_theme_update(theme, preset = "superhero"),
 
# Add custom CSS to ensure sliders maintain the primary color
tags$head(
  tags$style(HTML(paste0("
    .irs-bar, .irs-bar-edge, .irs-single, .irs-from, .irs-to {
      background: ", my_primary_color, " !important;
      border-color: ", my_primary_color, " !important;
    }
    .irs-handle {
      border-color: ", my_primary_color, " !important;
       background: ", my_primary_color, " !important;
    }

    /* Additional styling for sidebar elements */
    .sidebar, .sidebar-content, .sidebar .form-group, .sidebar .control-label {
      --bs-primary: ", my_primary_color, " !important;
      --bs-primary-rgb: ", paste(as.vector(col2rgb(my_primary_color)), collapse = ","), " !important;
    }
  ")))
),

  title = "Simulators",

nav_panel(
  title = "About",
  layout_columns(
    # Prima riga: due card affiancate
    layout_columns(
      col_width = 6,
      card(
        card_header("About Simulators"),
        p("This application integrates two separate Shiny apps into a single interface using Shiny modules."),
        p("Use the navigation bar above to switch between the different applications.")
      ),
      card(
        card_header("How to Use"),
        p("Navigate between the different simulators using the tabs at the top of the page."),
        p("Each simulator has its own inputs and outputs that operate independently.")
      )
    ),
    # Seconda riga: altre due card affiancate
    layout_columns(
      col_width = 6,
      card(
        card_header("Features"),
        p("These simulators provide interactive visualizations for complex statistical concepts."),
        p("They are designed to be educational and user-friendly.")
      ),
      card(
        card_header("Contact"),
        p("For questions or feedback about these simulators, please contact us."),
        p("Email: example@example.com")
      )
    )
  ),
  
  card_footer(
    style = "padding: 8px; margin-top: auto; display: flex; justify-content: space-between;", 
    # Elemento a sinistra
    span(""),
    # Elemento al centro
    span(style = "text-align: center;", "Version 1.1"),
    # Elemento a destra
    span("")
  )
  ),
  
  
  nav_spacer(),
  
  
  nav_panel(
    title = "OralDisintTablet",
   
    OralDisintTablet_ui("odt")
  ),
  
  nav_panel(
    title = "ForcDegradReact",
    ReazDegradForzataModule_ui("rdf")
  )

  

  
)

# Main application server logic
server <- function(input, output, session) {
  # Call the module server functions with their respective IDs
  
  # per personalizzare il tema
  # bs_themer()
  
  OralDisintTablet_server("odt")
  ReazDegradForzataModule_server("rdf")
}

# Run the application
shinyApp(ui = ui, server = server)