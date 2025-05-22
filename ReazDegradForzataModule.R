ReazDegradForzataModule_ui <- function(id) {
  ns <- NS(id)
  
  page_sidebar(
    title = "Forced Degradation Reaction",
    sidebar = sidebar(
      # Factor 1: Temperature
      card(
        sliderInput(ns("x1"), "Reaction Temperature", 
                    value = 65, min = 30, max = 100, step = 1)
      ),
      
      # Factor 2: Acid concentration
      card(
        sliderInput(ns("x2"), "Acid concentration (N)", 
                    min = 0.1, max = 5.9, value = 3, step = 0.1)
      ),
      
      # Factor 3: Acid
      card(
        sliderTextInput(
          inputId = ns("x3"),
          label = "Type of acid (HCl, H2SO4):",
          choices = c("HCl", "H2SO4"),
          selected = "HCl"
        )
      ),
      
      # Factor 4: API concentration
      card(
        sliderInput(ns("x4"), "API concentration (mg/mL):", 
                    min = 0.1, max = 19.9, value = 10, step = 0.1)
      ),
      
      # Factor 5: Reaction time (h)
      card(
        sliderInput(ns("x5"), "Reaction time (h):", 
                    min = 0.25, max = 24.75, value = 12.5, step = 0.1)
      ),
      
      # Factor 6: Reaction volume (mL)
      card(
        sliderInput(ns("x6"), "Reaction volume (mL):", 
                    min = 5, max = 15, value = 10, step = 1)
      ),
      
      # Factor 7: Heating mode (plate, microwave)
      card(
        sliderTextInput(
          inputId = ns("x7"),
          label = "Heating mode (plate, microwave):",
          choices = c("p", "mw"),
          selected = "p"
        )
      ),
      
      # Factor 8: Mechanical stirring
      card(
        sliderTextInput(
          inputId = ns("x8"),
          label = "Mechanical stirring:",
          choices = c("no", "yes"),
          selected = "no"
        )
      ),
      
      # Factor 9: Reaction quenching (liquid nitrogen)
      card(
        sliderTextInput(
          inputId = ns("x9"),
          label = "Reaction quenching (liquid nitrogen):",
          choices = c("no", "yes"),
          selected = "no"
        )
      ),
      
      # Generate data button
      actionButton(ns("generate"), "Generate Data", class = "btn-primary btn-lg w-100 mt-3")
    ),
    
    card(
      min_height = "230px",
      card_header("Study Purpose"),
      div(
        style = "white-space: pre-wrap; word-wrap: break-word; overflow-wrap: break-word; max-width: 100%;",
        "Identify the reaction conditions that degrade the API between 5 and 20% - that is, that leave the API unchanged in % between 95 and 80%. Optimal degradation percentage range [...]

Lowest possible reaction time and temperature, no workup of the product solution (no mw, no liquid nitrogen).
High temperature, high acid concentration: low %, MB < 97%
Low temperature and low acid concentration: long times, API degrades linearly, MB almost always ok and constant."
      )
    ),
    
    card(
      card_header("Responses"),
      min_height = "200px",
      layout_columns(
        value_box(
          title = "Residual API amount (%, w/w)",
          value = textOutput(ns("response1")),
          showcase = bsicons::bs_icon("clipboard-data"),
          # theme = "warning"
          style = "background-color:#7777F9; color: white;" # Blue, white text
        ),
        value_box(
          title = "Mass balance (%, w/w)",
          value = textOutput(ns("response2")),
          showcase = bsicons::bs_icon("clipboard-data"),
          # theme = "danger"
          style = "background-color:#E55DBC; color: white;" # Red/violet, white text
          
        )
      )
    ),
    
    card(
      min_height = "500px",
      card_header(
        "Data History",
        downloadButton(ns("downloadData"), "Save to Excel", 
                       class = "btn-primary float-end")
      ),
      DTOutput(ns("history_table"))
    )
  )
}


ReazDegradForzataModule_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    # Reactive value to store history
    history_data <- reactiveVal(
      data.frame(
        Exp = numeric(0),
        
        Temperature = numeric(0),
        Acid_concentration = numeric(0),
        Acid = character(0),
        API_concentration = numeric(0),
        Time = numeric(0),
        Volume = numeric(0),
        Heating = character(0),
        Stirring = character(0),
        Quenching = character(0),
        
        API_amount = numeric(0),
        Mass_balance = numeric(0)
      )
    )
    
    
    
    
    # model generation -------------------------------------------------
    
    # Generate responses when button is clicked
    observeEvent(input$generate, {
      # set.seed(seed())
      # Add some randomness but make responses depend on factors
      # Each factor contributes to the responses in different ways
      
      
      # effects for the variables encoded, def non-encoded variables
      x1 <- (input$x1 - 65)/35 
      x2 <- (input$x2 - 3)/2.9
      x3 <- switch(input$x3,
                   "HCl" = -1,
                   "H2SO4" = 1)
      x4 <- (input$x4 - 10)/9.9
      x5 <- (input$x5 - 12.5)/12.25
      x6 <- (input$x6 - 10)/5
      x7 <- switch(input$x7,
                   "p" = -1,
                   "mw" = 1)
      x8 <- switch(input$x8,
                   "no" = -1,
                   "yes" = 1)
      x9 <- switch(input$x9,
                   "no" = -1,
                   "yes" = 1) 
      
      
      # Factor effects for Response 1 
      x1_effect1 <- -2.9 * x1
      x2_effect1 <- -3.2 * x2
      x3_effect1 <- -1.07 * x3
      x4_effect1 <- -0.25 * x4
      x5_effect1 <- -1.025 * x5
      x6_effect1 <- 0.075 * x6
      x7_effect1 <- 0.045 * x7
      x8_effect1 <- -0.0475 * x8
      x9_effect1 <- 0.35 * x9
      
      x11_effect1 <- 0 * x1^2
      x22_effect1 <- 0 * x2^2
      x44_effect1 <- 0 * x4^2
      x55_effect1 <- 0 * x5^2
      x66_effect1 <- 0 * x6^2
      
      x12_effect1 <- -1.8 * x1*x2
      x13_effect1 <- 0 * x1*x3
      x14_effect1 <- 0.05 * x1*x4
      x15_effect1 <- 0.85 * x1*x5
      x16_effect1 <- 0.075 * x1*x6
      x17_effect1 <- 0 * x1*x7
      x18_effect1 <- 0 * x1*x8
      x19_effect1 <- 0 * x1*x9
      
      x23_effect1 <- 0 * x2*x3
      x24_effect1 <- 0 * x2*x4
      x25_effect1 <- 0 * x2*x5
      x26_effect1 <- 0 * x2*x6
      x27_effect1 <- 0 * x2*x7
      x28_effect1 <- 0 * x2*x8
      x29_effect1 <- 0 * x2*x9
      
      x34_effect1 <- 0 * x3*x4
      x35_effect1 <- 0 * x3*x5
      x36_effect1 <- 0 * x3*x6
      x37_effect1 <- 0 * x3*x7
      x38_effect1 <- 0 * x3*x8
      x39_effect1 <- 0 * x3*x9
      
      x45_effect1 <- 0 * x4*x5
      x46_effect1 <- 0 * x4*x6
      x47_effect1 <- 0 * x4*x7
      x48_effect1 <- 0 * x4*x8
      x49_effect1 <- 0 * x4*x9
      
      x56_effect1 <- 0 * x5*x6
      x57_effect1 <- 1 * x5*x7
      x58_effect1 <- 0 * x5*x8
      x59_effect1 <- 0 * x5*x9
      
      x67_effect1 <- 0 * x6*x7
      x68_effect1 <- 0 * x6*x8
      x69_effect1 <- 0 * x6*x9
      
      x78_effect1 <- 0 * x7*x8
      x79_effect1 <- 0 * x7*x9
      
      x89_effect1 <- 0 * x8*x9
      
      
      # Factor effects for Response 2 
      x1_effect2 <- -3.2 * x1
      x2_effect2 <- -5.5 * x2
      x3_effect2 <- -3.25 * x3
      x4_effect2 <- 0.05 * x4
      x5_effect2 <- -2.5 * x5
      x6_effect2 <- 2.8 * x6
      x7_effect2 <- 0.375 * x7
      x8_effect2 <- 0.025 * x8
      x9_effect2 <- 5.375 * x9
      
      x11_effect2 <- 0 * x1^2
      x22_effect2 <- 0 * x2^2
      x44_effect2 <- 0 * x4^2
      x55_effect2 <- 0 * x5^2
      x66_effect2 <- 0 * x6^2
      
      x12_effect2 <- -0.665 * x1*x2
      x13_effect2 <- 0 * x1*x3
      x14_effect2 <- 0.56 * x1*x4
      x15_effect2 <- 0.07 * x1*x5
      x16_effect2 <- 0.03 * x1*x6
      x17_effect2 <- 0 * x1*x7
      x18_effect2 <- 0 * x1*x8
      x19_effect2 <- 0 * x1*x9
      
      x23_effect2 <- 0 * x2*x3
      x24_effect2 <- 0 * x2*x4
      x25_effect2 <- 0 * x2*x5
      x26_effect2 <- 0 * x2*x6
      x27_effect2 <- 0 * x2*x7
      x28_effect2 <- 0 * x2*x8
      x29_effect2 <- 0 * x2*x9
      
      x34_effect2 <- 0 * x3*x4
      x35_effect2 <- 0 * x3*x5
      x36_effect2 <- 0 * x3*x6
      x37_effect2 <- 0 * x3*x7
      x38_effect2 <- 0 * x3*x8
      x39_effect2 <- 0 * x3*x9
      
      x45_effect2 <- 0 * x4*x5
      x46_effect2 <- 0 * x4*x6
      x47_effect2 <- 0 * x4*x7
      x48_effect2 <- 0 * x4*x8
      x49_effect2 <- 0 * x4*x9
      
      x56_effect2 <- 0 * x5*x6
      x57_effect2 <- 0 * x5*x7
      x58_effect2 <- 0 * x5*x8
      x59_effect2 <- 0 * x5*x9
      
      x67_effect2 <- 0 * x6*x7
      x68_effect2 <- 0 * x6*x8
      x69_effect2 <- 0 * x6*x9
      
      x78_effect2 <- 0 * x7*x8
      x79_effect2 <- 0 * x7*x9
      
      x89_effect2 <- 0 * x8*x9
      
      
      #
      # Calculate responses with some random noise
      r1 <- 85.3 + x1_effect1 + x2_effect1 + x3_effect1 + x4_effect1 + x5_effect1 + x6_effect1 + x7_effect1 + x8_effect1 + x9_effect1 +
        x11_effect1 + x22_effect1 + x44_effect1 + x55_effect1 + x66_effect1 +
        x12_effect1 + x13_effect1 + x14_effect1 + x15_effect1 + x16_effect1 + x17_effect1 + x18_effect1 + x19_effect1 +
        x23_effect1 + x24_effect1 + x25_effect1 + x26_effect1 + x27_effect1 + x28_effect1 + x29_effect1 +
        x34_effect1 + x35_effect1 + x36_effect1 + x37_effect1 + x38_effect1 + x39_effect1 +
        x45_effect1 + x46_effect1 + x47_effect1 + x48_effect1 + x49_effect1 +
        x56_effect1 + x57_effect1 + x58_effect1 + x59_effect1 +
        x67_effect1 + x68_effect1 + x69_effect1 +
        x78_effect1 + x79_effect1 +
        x89_effect1 +
        rnorm(1, 0, 1)
      r2 <- 83.5 + x1_effect2 + x2_effect2 + x3_effect2 + x4_effect2 + x5_effect2 + x6_effect2 + x7_effect2 + x8_effect2 + x9_effect2 +
        x11_effect2 + x22_effect2 + x44_effect2 + x55_effect2 + x66_effect2 +
        x12_effect2 + x13_effect2 + x14_effect2 + x15_effect2 + x16_effect2 + x17_effect2 + x18_effect2 + x19_effect2 +
        x23_effect2 + x24_effect2 + x25_effect2 + x26_effect2 + x27_effect2 + x28_effect2 + x29_effect2 +
        x34_effect2 + x35_effect2 + x36_effect2 + x37_effect2 + x38_effect2 + x39_effect2 +
        x45_effect2 + x46_effect2 + x47_effect2 + x48_effect2 + x49_effect2 +
        x56_effect2 + x57_effect2 + x58_effect2 + x59_effect2 +
        x67_effect2 + x68_effect2 + x69_effect2 +
        x78_effect2 + x79_effect2 +
        x89_effect2 +
        rnorm(1, 0, 1)
      
      # Ensure responses are in reasonable ranges
      r1 <- max(0, min(100, r1))
      r2 <- max(0, min(100, r2))
      # r3 <- max(0, min(100, r3))
      
      # Update displayed responses
      output$response1 <- renderText({
        paste0(format(round(r1, 1), nsmall = 1), "%")
      })
      
      output$response2 <- renderText({
        paste0(format(round(r2, 1), nsmall = 1), "%")
      })
      
      # Add to history
      current_data <- history_data()
      
      # Calculate experiment number
      exp_number <- 1
      if (!is.null(current_data) && nrow(current_data) > 0) {
        exp_number <- max(current_data$Exp) + 1
      }
      
      new_row <- data.frame(
        Exp = exp_number,  # Add the experiment counter
        
        Temperature = input$x1,
        Acid_concentration = input$x2,
        Acid = input$x3,
        API_concentration = input$x4,
        Time = input$x5,
        Volume = input$x6,
        Heating = input$x7,
        Stirring = input$x8,
        Quenching = input$x9,
        
        API_amount = format(round(r1, 1), nsmall = 1),
        Mass_balance = format(round(r2, 1), nsmall = 1)
      )
      
      history_data(rbind(new_row, current_data))
    })
    
    # Render history table
    output$history_table <- renderDT({
      req(nrow(history_data()) > 0)
      datatable(history_data(), 
                options = list(pageLength = 10, 
                               autoWidth = TRUE,
                               scrollX = TRUE,
                               order = list(
                                 list(0, 'desc')),  # Sort by column Exp (index 0) in descending order
                               columnDefs = list(
                                 list(className = 'dt-center', targets = "_all")
                               )),
                rownames = FALSE)%>%
        formatStyle(
          columns = 11,  
          # backgroundColor = bslib::bs_get_variables(bslib::bs_theme(), "primary"),
          backgroundColor = "#7777F9",
          color = "white",
          fontWeight = "bold"
        )%>%
        formatStyle(
          columns = 12,  
          # backgroundColor = bslib::bs_get_variables(bslib::bs_theme(), "success"),
          backgroundColor = "#E55DBC",
          color = "white",
          fontWeight = "bold"
        )
    })
    
    # Download handler for Excel export
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Degrad_Simulation_", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".xlsx", sep = "")
      },
      content = function(file) {
        # Only proceed if there's data to download
        req(nrow(history_data()) > 0)
        
        # Create a workbook
        wb <- createWorkbook()
        
        # Add a worksheet
        addWorksheet(wb, "Exp_data")
        
        # Write the data to the worksheet
        writeData(wb, "Exp_data", history_data())
        
        # Style the header row
        headerStyle <- createStyle(
          fontSize = 12, 
          fontColour = "#FFFFFF", 
          halign = "center", 
          fgFill = "#4472C4", 
          border = "TopBottom", 
          borderColour = "#000000"
        )
        
        # Orange style for responses header
        headerRisposteStyle <- createStyle(
          fontColour = "#FFFFFF",   # white text for better readability
          fgFill = "#E26B0A",       # orange
          halign = "center",
          textDecoration = "bold"
        )
        
        addStyle(wb, "Exp_data", headerStyle, rows = 1, cols = 1:ncol(history_data()))
        
        # Apply the "responses" style to the last 4 headers
        addStyle(wb, "Exp_data", headerRisposteStyle, rows = 1, cols = (ncol(history_data())-1):ncol(history_data()))
        
        # Auto-adjust column widths
        setColWidths(wb, "Exp_data", cols = 1:ncol(history_data()), widths = "auto")
        
        # Save the workbook
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    
    })
  }
  
  
  
 