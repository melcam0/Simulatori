# ui Module
OralDisintTablet_ui <- function(id) {
  ns <- NS(id)
  
  page_sidebar(
    title = "OralDisintTablet",
    sidebar = sidebar(
      card(
        sliderInput(ns("x1"), "Binder (%, p/p)", 
                    min = 2, max = 5, value = 3.5, step = 0.1)
      ),
      card(
        sliderInput(ns("x2"), "Disintegrant level (%, p/p)", 
                    min = 2, max = 5, value = 3.5, step = 0.1)
      ),
      card(
        sliderInput(ns("x3"), "Compaction force (MPa):", 
                    min = 10, max = 20, value = 15, step = 1)
      ),
      card(
        sliderTextInput(
          inputId = ns("x4"),
          label = "Binder type (A, B):",
          choices = c("A", "B"),
          selected = "A"
        )
      ),
      card(
        sliderTextInput(
          inputId = ns("x5"),
          label = "Disintegrant type (A, B):",
          choices = c("A", "B"),
          selected = "A"
        )
      ),
      actionButton(ns("generate"), "Data generator", class = "btn-primary btn-lg w-100 mt-3")
    ),
    card(
      min_height = "250px",
      card_header("Factors"),
      div(
        style = "white-space: pre-wrap; word-wrap: break-word; overflow-wrap: break-word; max-width: 100%;",
        "Disintegrant excipients in tablets facilitate rapid breakdown and dissolution, ensuring proper drug absorption. Examples include starch and its derivatives, cellulose-based excipients, and superdisintegrants like croscarmellose sodium and crospovidone

Binder excipients in tablets hold the active pharmaceutical ingredients (APIs) and other excipients together, ensuring the tablet's integrity and mechanical strength. They improve the cohesion and plasticity of the powder mixture, enabling the manufacture of robust and uniform tablets. Binders also play a role in controlling the release of the active ingredient and maintaining the tablet's shape and size
The most common are starch, microcrystalline cellulose (MCC), polyvinylpyrrolidone (PVP), and gelatin. 
Starch: A common and versatile binder, often used as a paste or in dry form.
Gelatin: Derived from animal collagen, it provides good binding strength and is often used in film-coated tablets.
Acacia: A natural gum that can be used as a binder, offering good binding properties and biodegradability.
Synthetic/Semi-synthetic Binders: 
Microcrystalline Cellulose (MCC): A widely used binder, known for its good compressibility and binding properties.
Polyvinylpyrrolidone (PVP): Offers excellent binding and can also act as a solubilizer and stabilizer.
Hydroxypropyl Cellulose (HPC): A cellulose derivative with good water solubility and binding capabilities.
Hydroxypropyl Methylcellulose (HPMC): Another cellulose derivative, often used for its film-forming properties and ability to control release.
Polyethylene Glycol (PEG): A polymer used as a binder and can also act as a plasticizer.
Sodium Carboxymethyl Cellulose (NaCMC): A cellulose derivative that provides good binding and can also act as a disintegrant.
"
      )
    ),
    
    card(
      min_height = "250px",
      card_header("Responses"),
      div(
        style = "white-space: pre-wrap; word-wrap: break-word; overflow-wrap: break-word; max-width: 100%;",
        "The ideal hardness for a standard oral tablet typically falls within the range of 40 to 100 Newtons (N). This range ensures the tablet is strong enough to withstand handling and shipping while still disintegrating properly for drug release.
While there's no single ideal value, this range is a general guideline for most oral solid dosage forms.
Some medications may require different hardness levels based on factors like the active ingredient, delivery mechanism, or specific handling requirements.
For example, a study on orally disintegrating tablets (ODTs) aimed for a hardness of 100 N, ensuring easy handling and compatibility with conventional packaging. Other studies have explored hardness ranges between 30 and 140 N for ODTs. From research studies and various pharmacological-based studies, it is recommended that an immediate-release tablet hardness mainly limits from 30 to 60N.

The ideal disintegration time for an oral tablet, especially an Orally Disintegrating Tablet (ODT), is typically within 3 minutes. However, for improved patient compliance, a disintegration time of less than 30 seconds is often preferred.
European Pharmacopoeia (Ph. Eur.): defines ODTs as tablets that should disintegrate within 3 minutes.
Food and Drug Administration (FDA): guidance suggests a disintegration time of up to 30 seconds for ODTs.
Patient Compliance: Shorter disintegration times (e.g., less than 30 seconds) can enhance patient compliance, especially in cases where swallowing might be difficult or where rapid absorption is desired.

Standard Tablet Disintegration: For standard tablets, the European Medicines Agency (EMA) specifies a general disintegration time of 30 minutes for plain tablets and 60 minutes for coated tablets, unless otherwise stated in the individual monograph.
"
      )
    ),
    card(
      card_header("Responses"),
      min_height = "200px",
      layout_columns(
        value_box(
          title = "Disintegration time (min)",
          value = textOutput(ns("response1")),
          showcase = bsicons::bs_icon("clipboard-data"),
          theme = "primary"
        ),
        value_box(
          title = "Hardness (N)",
          value = textOutput(ns("response2")),
          showcase = bsicons::bs_icon("clipboard-data"),
          theme = "success"
        )
      )
    ),
    card(
      min_height = "500px",
      card_header(
        "Historical Data",
        downloadButton(ns("downloadData"), "Save to Excel", 
                       class = "btn-success float-end")
      ),
      DTOutput(ns("history_table"))
    )
  )
}

# Server Module
OralDisintTablet_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive value to store history
    history_data <- reactiveVal(
      data.frame(
        Exp = numeric(0),
        
        Binder = numeric(0),
        Disintegrant_level = numeric(0),
        Compaction_force = numeric(0),
        Binder_type = character(0),
        Disintegrant_type = character(0),
        
        Dis_time = numeric(0),
        Hardness = numeric(0)
      )
    )
    
    
    
    
    # da qui generare modello -------------------------------------------------
    
    # Generate responses when button is clicked
    observeEvent(input$generate, {
      # set.seed(seed())
      # Add some randomness but make responses depend on factors
      # Each factor contributes to the responses in different ways
      
      # effetti per le variabili codificati, def variabili non codificate
      x1 <- (input$x1 - 3.5)/1.5 
      x2 <- (input$x2 - 3.5)/1.5 
      x3 <- (input$x3 - 15)/5
      x4 <- switch(input$x4,
                   "A" = -1,
                   "B" = 1)
      x5 <- switch(input$x5,
                   "A" = -1,
                   "B" = 1)
      
      
      # Factor effects for Response 1 
      x1_effect1 <- 0.49 * x1
      x2_effect1 <- -3.2 * x2 
      x3_effect1 <- 1.97 * x3
      x4_effect1 <- 0 * x4
      x5_effect1 <- 0 * x5
      
      x12_effect1 <- 0.05 * x1*x2
      x13_effect1 <- 0.025 * x1*x3
      x23_effect1 <- -0.975 * x2*x3
      x34_effect1 <- 0 * x3*x4
      
      x11_effect1 <- 0.425 * x1^2
      x22_effect1 <- 0.475 * x2^2
      x33_effect1 <- 0.025 * x3^2
      
      
      # Factor effects for Response 2 
      x1_effect2 <- 33.2 * x1
      x2_effect2 <- 0.5 * x2
      x3_effect2 <- 34.25 * x3
      x4_effect2 <- 1 * x4
      x5_effect2 <- 1 * x5
      
      x12_effect2 <- 1 * x1*x2
      x13_effect2 <- 18.5 * x1*x3
      x23_effect2 <- 3 * x2*x3
      x34_effect2 <- 1 * x3*x4
      
      x11_effect2 <- 0.385 * x1^2
      x22_effect2 <- 6.875 * x2^2
      x33_effect2 <- -8.375 * x3^2
      
      
      # Calculate responses with some random noise
      r1 <- 5.625 + x1_effect1 + x2_effect1 + x3_effect1 + x4_effect1 + x5_effect1+
        x12_effect1 +   x13_effect1 +   x23_effect1 +  x34_effect1 +   
        x11_effect1 +    x22_effect1 +  x33_effect1 +
        rnorm(1, 0, 0.6)
      
      r2 <- 104.625 + x1_effect2 + x2_effect2 + x3_effect2 + x4_effect2 + x5_effect2+
        x12_effect2 +   x13_effect2 +   x23_effect2 +  x34_effect2 +   
        x11_effect2 +    x22_effect2 +  x33_effect2 +
        rnorm(1, 0, 1.5)
      
      
      
      # Update displayed responses
      output$response1 <- renderText({
        round(r1, 1)
      })
      
      output$response2 <- renderText({
        round(r2, 0)
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
        
        Binder  = input$x1,
        Disintegrant_level  = input$x2,
        Compaction_force  = input$x3,
        Binder_type  = input$x4,
        Disintegrant_type  = input$x5,
        
        Dis_time  = round(r1, 1),
        Hardness  = round(r2, 0)
        # Stabilità = round(r3, 1),
        # Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
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
          columns = 7,  
          backgroundColor = bslib::bs_get_variables(bslib::bs_theme(), "primary"),
          color = "white",
          fontWeight = "bold"
        )%>%
        formatStyle(
          columns = 8,  
          backgroundColor = bslib::bs_get_variables(bslib::bs_theme(), "success"),
          color = "white",
          fontWeight = "bold"
        )
    })
    
    # Download handler for Excel export
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("Exp_data-", format(Sys.time(), "%Y-%m-%d-%H%M%S"), ".xlsx", sep = "")
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
        addStyle(wb, "Exp_data", headerStyle, rows = 1, cols = 1:ncol(history_data()))
        
        # Auto-adjust column widths
        setColWidths(wb, "Exp_data", cols = 1:ncol(history_data()), widths = "auto")
        
        # Save the workbook
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    
    })
  }