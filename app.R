library(shiny)
library(bslib)

# Source the module files
source("OralDisintTabletModule.R")
source("ReazDegradForzataModule.R")

# Main application UI
ui <- page_navbar(
  title = "Simulatori",
  # Remove the theme setting or make it null to use default styling
  # theme = bs_theme(bootswatch = "flatly"),
  
  # nav_panel(
  #   title = "App 1",
  #   OralDisintTablet_ui("app1")
  # ),
  # 
  # nav_panel(
  #   title = "App 2",
  #   module2_ui("app2")
  # ),
  
  nav_panel(
    title = "About",
    card(
      card_header("About Simulators"),
      p("This application integrates two separate Shiny apps into a single interface using Shiny modules."),
      p("Use the navigation bar above to switch between the different applications.")
    )
  ),
  
  
  nav_spacer(),
  
  # nav_panel(
  #   title = "About",
  #   card(
  #     card_header("About This Application"),
  #     p("This application integrates two separate Shiny apps into a single interface using Shiny modules."),
  #     p("Use the navigation bar above to switch between the different applications.")
  #   )
  # )
  
  nav_panel(
    title = "OralDisintTablet",
    OralDisintTablet_ui("odt")
  ),
  
  nav_panel(
    title = "ReazDegradForzata",
    ReazDegradForzataModule_ui("rdf")
  ),
  
  # 
  card(
    footer = TRUE,
    " Version 1.0",
    # HTML((paste(" "," ","Giorgio Marrubini","email: giorgio.marrubini@unipv.it",
    #             " ",
    #             'Camillo Melzi','email: camillomelzi@gmail.com',sep="<br/>"))),
    class = "text-center mt-4"
  )
  
  

  
)

# Main application server logic
server <- function(input, output, session) {
  # Call the module server functions with their respective IDs
  OralDisintTablet_Server("odt")
  ReazDegradForzataModule_Server("rdf")
}

# Run the application
shinyApp(ui = ui, server = server)