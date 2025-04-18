library(shiny)
library(shinyWidgets)
library(bslib)
library(dplyr)
library(DT)
library(openxlsx)  # Added for Excel export

# Set a seed for reproducibility
# set.seed(as.integer(Sys.time()))

ui <- page_sidebar(
  title = "Reazione di degradazione forzata",
  sidebar = sidebar(
    # Factor 1: Temperature
    card(
      sliderInput("x1", "Temperatura di reazione", 
                   value = 65, min = 30, max = 100, step = 1)
    ),
    
    # Factor 2: Concentrazione acido
    card(
      sliderInput("x2", "Concentrazione dell'acido (N)", 
                  min = 0.1, max = 5.9, value = 3, step = 0.1)
    ),
    
    # Factor 3: Acido
    card(
      sliderTextInput(
        inputId = "x3",
        label = "Tipo di acido (HCl, H2SO4):",
        choices = c("HCl", "H2SO4"),
        selected = "HCl"
      )
    ),
    
    # Factor 4: Concentrazione API
    card(
      sliderInput("x4", "Concentrazione API (mg/mL):", 
                  min = 0.1, max = 19.9, value = 10, step = 0.1)
    ),
    
    # Factor 5: Tempo di reazione (h)
    card(
      sliderInput("x5", "Tempo di reazione (h):", 
                  min = 0.25, max = 24.75, value = 12.5, step = 0.1)
    ),
    
    # Factor 6: Volume di reazione (mL)
    card(
      sliderInput("x6", "Volume di reazione (mL):", 
                  min = 5, max = 15, value = 10, step = 1)
    ),
    
    # Factor 7: Modalità di riscaldamento (piastra, microonde)
    card(
      sliderTextInput(
        inputId = "x7",
        label = "Modalità di riscaldamento (piastra, microonde):",
        choices = c("p", "mw"),
        selected = "p"
      )
    ),
    
    # Factor 8: Agitazione meccanica
    card(
      sliderTextInput(
        inputId = "x8",
        label = "Agitazione meccanica:",
        choices = c("no", "si"),
        selected = "no"
      )
    ),
    
    # Factor 9: Quenching della reazione (azoto liquido)
    card(
      sliderTextInput(
        inputId = "x9",
        label = "Quenching della reazione (azoto liquido):",
        choices = c("no", "si"),
        selected = "no"
      )
    ),
    
    # Generate data button
    actionButton("generate", "Genera Dati", class = "btn-primary btn-lg w-100 mt-3")
  ),

card(
  min_height = "230px",
  card_header("Scopo dello studio"),
  div(
    style = "white-space: pre-wrap; word-wrap: break-word; overflow-wrap: break-word; max-width: 100%;",
    "Individuare le condizioni di reazione che degradino l'API tra il 5 e il 20% - ossia che lascino immodificato l'API in % compresa tra il 95 e l'80%. Range percentuale ottimale di degradazione 10-15%. Il bilancio di massa inoltre deve risultare compreso strettamente tra 97 e 103%.
    
Tempo di reazione e temperatura più basse possibili, nessun workup della soluzione dei prodotti (no mw, no azoto liq.)
Alta temperatura alta concentrazione di acido % bassa e MB < 97% 
Bassa temperatura e bassa concentrazione di acido: tempi lunghi, API che si degrada in modo lineare, MB costante ok quasi sempre."
  )
),
    
    card(
      card_header("Risposte"),
      min_height = "200px",
      layout_columns(
        value_box(
          title = "Quantità residua di API (%, p/p)",
          value = textOutput("response1"),
          showcase = bsicons::bs_icon("clipboard-data"),
          theme = "primary"
        ),
        value_box(
          title = "Bilancio di massa (%, p/p)",
          value = textOutput("response2"),
          showcase = bsicons::bs_icon("clipboard-data"),
          theme = "success"
        )
      )
    ),
    
    card(
      min_height = "500px",
      card_header(
        "Storico dei Dati",
        downloadButton("downloadData", "Salva in Excel", 
                       class = "btn-success float-end")
      ),
      DTOutput("history_table")
    )
)

server <- function(input, output, session) {
  # Reactive value to store history
  history_data <- reactiveVal(
    data.frame(
      Exp = numeric(0),
      
      Temperatura = numeric(0),
      Concentrazione_acido = numeric(0),
      Acido = character(0),
      Concentrazione_API = numeric(0),
      Tempo = numeric(0),
      Volume = numeric(0),
      Riscaldamento = character(0),
      Agitazione = character(0),
      Quenching = character(0),
      
      Qta_API = numeric(0),
      Bilancio_massa = numeric(0)
    )
  )
  
  
  

# da qui generare modello -------------------------------------------------
  
  # Generate responses when button is clicked
  observeEvent(input$generate, {
    # set.seed(seed())
    # Add some randomness but make responses depend on factors
    # Each factor contributes to the responses in different ways
    
    
    # effetti per le variabili codificati, def variabili non codificate
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
                 "si" = 1)
    x9 <- switch(input$x9,
                 "no" = -1,
                 "si" = 1) 
    
    
    # Factor effects for Response 1 
    x1_effect1 <- 1 * x1
    x2_effect1 <- 1 * x2
    x3_effect1 <- 1 * x3
    x4_effect1 <- 1 * x4
    x5_effect1 <- 1 * x5
    x6_effect1 <- 0 * x6
    x7_effect1 <- 1 * x7
    x8_effect1 <- 0 * x8
    x9_effect1 <- 0 * x9
    
    x11_effect1 <- 1 * x1^2
    x22_effect1 <- 1 * x2^2
    x44_effect1 <- 1 * x4^2
    x55_effect1 <- 1 * x5^2
    x66_effect1 <- 0 * x6^2

    x12_effect1 <- 1 * x1*x2
    x13_effect1 <- 1 * x1*x3
    x14_effect1 <- 1 * x1*x4
    x15_effect1 <- 1 * x1*x5
    x16_effect1 <- 0 * x1*x6
    x17_effect1 <- 1 * x1*x7
    x18_effect1 <- 0 * x1*x8
    x19_effect1 <- 0 * x1*x9
    
    x23_effect1 <- 1 * x2*x3
    x24_effect1 <- 1 * x2*x4
    x25_effect1 <- 1 * x2*x5
    x26_effect1 <- 0 * x2*x6
    x27_effect1 <- 1 * x2*x7
    x28_effect1 <- 0 * x2*x8
    x29_effect1 <- 0 * x2*x9
    
    x34_effect1 <- 1 * x3*x4
    x35_effect1 <- 1 * x3*x5
    x36_effect1 <- 0 * x3*x6
    x37_effect1 <- 1 * x3*x7
    x38_effect1 <- 0 * x3*x8
    x39_effect1 <- 0 * x3*x9
    
    x45_effect1 <- 1 * x4*x5
    x46_effect1 <- 0 * x4*x6
    x47_effect1 <- 1 * x4*x7
    x48_effect1 <- 0 * x4*x8
    x49_effect1 <- 0 * x4*x9
    
    x56_effect1 <- 0 * x5*x6
    x57_effect1 <- 1 * x5*x7
    x58_effect1 <- 0 * x5*x8
    x59_effect1 <- 0 * x5*x9
    
    x67_effect1 <- 1 * x6*x7
    x68_effect1 <- 0 * x6*x8
    x69_effect1 <- 0 * x6*x9
    
    x78_effect1 <- 0 * x7*x8
    x79_effect1 <- 0 * x7*x9
    
    x89_effect1 <- 0 * x8*x9
                     
    
    # Factor effects for Response 2 
    x1_effect2 <- 1 * x1
    x2_effect2 <- 1 * x2
    x3_effect2 <- 1 * x3
    x4_effect2 <- 1 * x4
    x5_effect2 <- 1 * x5
    x6_effect2 <- 0 * x6
    x7_effect2 <- 1 * x7
    x8_effect2 <- 0 * x8
    x9_effect2 <- 0 * x9
    
    x11_effect2 <- 1 * x1^2
    x22_effect2 <- 1 * x2^2
    x44_effect2 <- 1 * x4^2
    x55_effect2 <- 1 * x5^2
    x66_effect2 <- 0 * x6^2
    
    x12_effect2 <- 1 * x1*x2
    x13_effect2 <- 1 * x1*x3
    x14_effect2 <- 1 * x1*x4
    x15_effect2 <- 1 * x1*x5
    x16_effect2 <- 0 * x1*x6
    x17_effect2 <- 1 * x1*x7
    x18_effect2 <- 0 * x1*x8
    x19_effect2 <- 0 * x1*x9
    
    x23_effect2 <- 1 * x2*x3
    x24_effect2 <- 1 * x2*x4
    x25_effect2 <- 1 * x2*x5
    x26_effect2 <- 0 * x2*x6
    x27_effect2 <- 1 * x2*x7
    x28_effect2 <- 0 * x2*x8
    x29_effect2 <- 0 * x2*x9
    
    x34_effect2 <- 1 * x3*x4
    x35_effect2 <- 1 * x3*x5
    x36_effect2 <- 0 * x3*x6
    x37_effect2 <- 1 * x3*x7
    x38_effect2 <- 0 * x3*x8
    x39_effect2 <- 0 * x3*x9
    
    x45_effect2 <- 1 * x4*x5
    x46_effect2 <- 0 * x4*x6
    x47_effect2 <- 1 * x4*x7
    x48_effect2 <- 0 * x4*x8
    x49_effect2 <- 0 * x4*x9
    
    x56_effect2 <- 0 * x5*x6
    x57_effect2 <- 1 * x5*x7
    x58_effect2 <- 0 * x5*x8
    x59_effect2 <- 0 * x5*x9
    
    x67_effect2 <- 1 * x6*x7
    x68_effect2 <- 0 * x6*x8
    x69_effect2 <- 0 * x6*x9
    
    x78_effect2 <- 0 * x7*x8
    x79_effect2 <- 0 * x7*x9
    
    x89_effect2 <- 0 * x8*x9
    
   
    #
    # Calculate responses with some random noise
    r1 <- 0 + x1_effect1 + x2_effect1 + x3_effect1 + x4_effect1 + x5_effect1 + x6_effect1 + x7_effect1 + x8_effect1 + x9_effect1 +
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
    r2 <- 0 + x1_effect2 + x2_effect2 + x3_effect2 + x4_effect2 + x5_effect2 + x6_effect2 + x7_effect2 + x8_effect2 + x9_effect2 +
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
      paste0(round(r1, 1), "%")
    })
    
    output$response2 <- renderText({
      paste0(round(r2, 1), "%")
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

      Temperatura = input$x1,
      Conc_acido = input$x2,
      Acido = input$x3,
      Conc_API = input$x4,
      Tempo = input$x5,
      Volume = input$x6,
      Riscaldamento = input$x7,
      Agitazione = input$x8,
      Quenching = input$x9,

      Qta_API = round(r1, 1),
      Bilancio_massa = round(r2, 1)
    )

    history_data(rbind(new_row, current_data))
  })
  
  # Render history table
  output$history_table <- renderDT({
    req(nrow(history_data()) > 0)
    datatable(history_data(), 
              options = list(pageLength = 5, 
                             autoWidth = TRUE,
                             scrollX = TRUE,
                             order = list(list(0, 'asc')),  # Ordina per la colonna Exp (indice 0) in ordine crescente
                             columnDefs = list(
                               list(className = 'dt-center', targets = "_all")
                             )),
              rownames = FALSE)%>%
      formatStyle(
        columns = 11,  
        backgroundColor = bslib::bs_get_variables(bslib::bs_theme(), "primary"),
        color = "white",
        fontWeight = "bold"
      )%>%
      formatStyle(
        columns = 12,  
        backgroundColor = bslib::bs_get_variables(bslib::bs_theme(), "success"),
        color = "white",
        fontWeight = "bold"
      )
  })
  
  # Download handler for Excel export
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dati-sperimentali-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      # Only proceed if there's data to download
      req(nrow(history_data()) > 0)
      
      # Create a workbook
      wb <- createWorkbook()
      
      # Add a worksheet
      addWorksheet(wb, "Dati Sperimentali")
      
      # Write the data to the worksheet
      writeData(wb, "Dati Sperimentali", history_data())
      
      # Style the header row
      headerStyle <- createStyle(
        fontSize = 12, 
        fontColour = "#FFFFFF", 
        halign = "center", 
        fgFill = "#4472C4", 
        border = "TopBottom", 
        borderColour = "#000000"
      )
      addStyle(wb, "Dati Sperimentali", headerStyle, rows = 1, cols = 1:ncol(history_data()))
      
      # Auto-adjust column widths
      setColWidths(wb, "Dati Sperimentali", cols = 1:ncol(history_data()), widths = "auto")
      
      # Save the workbook
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)