library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)
library(openxlsx)

# Function to simulate HPLC chromatogram
simulate_chromatogram <- function(compounds, flow_rate, column_length, 
                                  column_diameter, particle_size, 
                                  mobile_phase_composition, 
                                  detection_wavelength, 
                                  injection_volume, 
                                  noise_level,
                                  ionic_strength,
                                  ph,
                                  temperature) {
  
  # Simulation parameters
  time_points <- seq(0, 15, by = 0.01)
  baseline <- 0.01 + noise_level * rnorm(length(time_points), mean = 0, sd = 0.5)
  
  peaks <- matrix(0, nrow = length(time_points), ncol = length(compounds))
  
  RTs <- numeric(length(compounds))
  Areas <- numeric(length(compounds))
  Widths <- numeric(length(compounds))
  Heights <- numeric(length(compounds))
  
  # Calculate retention times based on parameters
  for (i in seq_along(compounds)) {
    # Retention time depends on:
    # - flow rate (higher = faster elution)
    # - column length (longer = slower elution)
    # - mobile phase (higher organic = faster elution)
    # - compound properties
    # - ionic strength (higher = can affect selectivity)
    # - pH (affects ionization state)
    # - temperature (higher = generally faster elution)
    
    # Base retention time for each compound
    base_rt <- compounds[[i]]$rt
    # Adjust for flow rate
    rt_adjusted <- base_rt * (1 / flow_rate) * 1.5
    # Adjust for column length
    rt_adjusted <- rt_adjusted * (column_length / 150)
    # Adjust for mobile phase composition
    rt_adjusted <- rt_adjusted * (1 - (mobile_phase_composition / 100) * compounds[[i]]$solvent_sensitivity)
    # Adjust for ionic strength (higher ionic strength can affect retention of charged compounds)
    ionic_factor <- 1 + (ionic_strength / 50) * compounds[[i]]$ionic_sensitivity
    rt_adjusted <- rt_adjusted * ionic_factor
    # Adjust for pH (affects ionization state of compounds)
    ph_difference <- abs(ph - compounds[[i]]$optimal_ph)
    ph_factor <- 1 + (ph_difference / 2) * compounds[[i]]$ph_sensitivity
    rt_adjusted <- rt_adjusted * ph_factor
    # Adjust for temperature (van't Hoff relationship - higher temp usually means faster elution)
    # Using a simplified model where a 10°C increase causes ~10-20% decrease in retention
    temp_factor <- (1 - (temperature - 30) * 0.015)  # 1.5% change per degree from reference temp of 30°C
    rt_adjusted <- rt_adjusted * temp_factor
    
    # Aggiungo rumore a RT
    rt_adjusted <- rt_adjusted*(1 + rnorm(1,0,0.01))
    RTs[i] <- rt_adjusted 
    
    
    
    # Adjust peak width based on column properties
    peak_width <- 0.1 * (particle_size / 3) * (column_diameter / 4.6)
    # Temperature also affects peak width (higher temp = sharper peaks generally)
    peak_width <- peak_width * (1 - (temperature - 30) * 0.01)
    peak_width <- peak_width * (1 + rnorm(1,0,0.05))
    Widths[i] <- peak_width
    
    # Adjust peak height based on concentration and detection properties
    peak_height <- compounds[[i]]$concentration * (injection_volume / 10) * 
      compounds[[i]]$response_factors[detection_wavelength]
    peak_height <- peak_height * (1 + rnorm(1,0,0.02))
    Heights[i] <- peak_height
    
    # Generate peak
    peaks[, i] <- peak_height * exp(-0.5 * ((time_points - rt_adjusted) / peak_width)^2)
    
    
    # Calculate peak area - numerical integration (approximately the sum of all points times the time step)
    Areas[i] <- sum(peaks[, i]) * (time_points[2] - time_points[1])
  }
  
  # Combine all peaks and add to baseline
  signal <- baseline + rowSums(peaks)
  
  # Return data frame with time and signal
  list(
    data = data.frame(
      Time = time_points,
      Signal = signal
    ),
    RTs = RTs,
    Heights = Heights,
    Widths = Widths,
    Areas = Areas
  )
}

# Define compound database with additional properties
compounds_db <- list(
  list(
    name = "Caffeine", 
    rt = 3.5, 
    concentration = 1.0,
    solvent_sensitivity = 0.7,
    response_factors = c("210 nm" = 0.6, "254 nm" = 1.0, "280 nm" = 0.8),
    ionic_sensitivity = 0.3,
    ph_sensitivity = 0.2,
    optimal_ph = 6.0
  ),
  list(
    name = "Paracetamol", 
    rt = 2.0, 
    concentration = 1.0,
    solvent_sensitivity = 0.5,
    response_factors = c("210 nm" = 0.8, "254 nm" = 0.6, "280 nm" = 0.3),
    ionic_sensitivity = 0.2,
    ph_sensitivity = 0.3,
    optimal_ph = 5.5
  ),
  list(
    name = "Aspirin", 
    rt = 4.2, 
    concentration = 1.0,
    solvent_sensitivity = 0.6,
    response_factors = c("210 nm" = 0.9, "254 nm" = 0.5, "280 nm" = 0.2),
    ionic_sensitivity = 0.5,
    ph_sensitivity = 0.6,
    optimal_ph = 6.5
  ),
  list(
    name = "Ibuprofen", 
    rt = 7.8, 
    concentration = 1.0,
    solvent_sensitivity = 0.8,
    response_factors = c("210 nm" = 0.7, "254 nm" = 0.4, "280 nm" = 0.3),
    ionic_sensitivity = 0.6,
    ph_sensitivity = 0.7,
    optimal_ph = 5.2
  ),
  list(
    name = "Naproxen", 
    rt = 6.5, 
    concentration = 1.0,
    solvent_sensitivity = 0.75,
    response_factors = c("210 nm" = 0.8, "254 nm" = 0.7, "280 nm" = 0.5),
    ionic_sensitivity = 0.55,
    ph_sensitivity = 0.65,
    optimal_ph = 5.8
  )
)




ui <- page_navbar(
  title = "HPLC Simulator",
  nav_spacer(),
  nav_panel(
    title = "Simulator",
    page_sidebar(
      sidebar = sidebar(
        h4("Sample"),
        checkboxGroupInput("compounds", "Compounds",
                           choices = sapply(compounds_db, function(c) c$name),
                           selected = "Caffeine"),
        
        sliderInput("noise_level", "Noise Level", 
                    min = 0, max = 0.5, value = 0.01, step = 0.01),
        
        h4("Instrument Parameters"),
        numericInput("flow_rate", "Flow Rate (mL/min)", 
                     min = 0.1, max = 5, value = 1, step = 0.1),
        
        numericInput("injection_volume", "Injection Volume (μL)", 
                     min = 1, max = 100, value = 10, step = 1),
        
        selectInput("detection_wavelength", "Detection Wavelength", 
                    choices = c("210 nm", "254 nm", "280 nm"),
                    selected = "254 nm"),
        
        h4("Column Properties"),
        sliderInput("column_length", "Column Length (mm)", 
                    min = 50, max = 250, value = 150, step = 10),
        
        selectInput("column_diameter", "Column Diameter (mm)", 
                    choices = c(2.1, 3.0, 4.6), selected = 4.6),
        
        selectInput("particle_size", "Particle Size (μm)", 
                    choices = c(1.7, 3, 5), selected = 3),
        
        h4("Mobile Phase"),
        sliderInput("mobile_phase_composition", "Organic Phase (%)", 
                    min = 0, max = 100, value = 50, step = 5),
        
        sliderInput("ionic_strength", "Ionic Strength (mM)", 
                    min = 0, max = 50, value = 20, step = 1),
        
        sliderInput("ph", "pH", 
                    min = 5, max = 7, value = 6, step = 0.1),
        
        sliderInput("temperature", "Temperature (°C)", 
                    min = 20, max = 50, value = 30, step = 1),
        
        actionButton("generate", "Data generator", class = "btn-primary btn-lg w-100 mt-3")
        
  
  
      ),
      
      # layout_columns(
        card(
          min_height = "375px",
          card_header("Simulated Chromatogram"),
          # plotOutput("chromatogram_plot", height = "400px")
          # style = "height: 500px; overflow-y: auto;"
          
          
          conditionalPanel(
            condition = "input.generate == 0",
            div(
              style = "color: #d9534f; font-weight: bold; border: 2px dashed #d9534f; padding: 15px; text-align: center; margin: 10px 0;",
              "Press the 'Data generator' button to start the simulation."
            )
          ),
          conditionalPanel(
            condition = "input.generate > 0",
            plotOutput("chromatogram_plot")
          )
        ),
        
        card(
          min_height = "375px",
          card_header("Peak Information"),
          # tableOutput("peak_info")
          conditionalPanel(
            condition = "input.generate == 0",
            div(
              style = "color: #d9534f; font-weight: bold; border: 2px dashed #d9534f; padding: 15px; text-align: center; margin: 10px 0;",
              "Press the 'Data generator' button to start the simulation."
            )
          ),
          conditionalPanel(
            condition = "input.generate > 0",
            tableOutput("peak_info")
          )
        )
      # )
      
      # card(
      #   height = 200,
      #   card_header("Simulator Information"),
      #   p("This HPLC simulator allows you to explore how different parameters affect chromatographic separation."),
      #   p("The simulator models the effects of flow rate, column dimensions, mobile phase composition, detector settings, ionic strength, pH, and temperature on the resulting chromatogram."),
      #   p("Note: This is a simplified model for educational purposes and doesn't account for all factors that would affect a real HPLC separation.")
      # )
      )
    ),
  
  
  
  
  
    nav_panel(
      title = "Historical data",
      card(
        card_header(
          "Historical Data"
          ),
        # height = 400,
        DTOutput("history_peak_info")
        # tableOutput("history_peak_info")
      ),
      layout_columns(
        downloadButton("downloadData", "Save historical data to Excel", 
                       class = "btn-success float-end"),
        actionButton(inputId = "reset_button", label = "Reset historical data")
      )
      ),
  
  
  nav_panel(
    title = "About",
    card(
      height = 200,
      card_header("Simulator Information"),
      p("This HPLC simulator allows you to explore how different parameters affect chromatographic separation."),
      p("The simulator models the effects of flow rate, column dimensions, mobile phase composition, detector settings, ionic strength, pH, and temperature on the resulting chromatogram."),
      p("Note: This is a simplified model for educational purposes and doesn't account for all factors that would affect a real HPLC separation.")
    )
  )
  
  
  
  )


server <- function(input, output, session) {
  
  # Reactive to store selected compounds with their properties
  selected_compounds <- eventReactive(input$generate,{
    compounds_db[match(input$compounds, sapply(compounds_db, function(c) c$name))]
  })
  
  # Simulate chromatogram when inputs change
  chromatogram_data <- eventReactive(input$generate,{
    req(length(input$compounds) > 0)

    simulate_chromatogram(
      compounds = selected_compounds(),
      flow_rate = input$flow_rate,
      column_length = input$column_length,
      column_diameter = as.numeric(input$column_diameter),
      particle_size = as.numeric(input$particle_size),
      mobile_phase_composition = input$mobile_phase_composition,
      detection_wavelength = input$detection_wavelength,
      injection_volume = input$injection_volume,
      noise_level = input$noise_level,
      ionic_strength = input$ionic_strength,
      ph = input$ph,
      temperature = input$temperature
    )
  })
  
  # Plot chromatogram
  output$chromatogram_plot <- renderPlot({
    data <- chromatogram_data()$data
    
    # Use ggplot with updated aesthetic names
    ggplot(data, aes(x = Time, y = Signal)) +
      geom_line(linewidth = 0.8, color = "blue") +
      labs(
        x = "Retention Time (min)",
        y = "Detector Response (AU)",
        title = "HPLC Chromatogram"
      ) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
  })
  
  # Generate peak information table
  output$peak_info <- renderTable({
    
    req(length(input$compounds) > 0)
    
    compounds <- selected_compounds()
    
    chromatogram_results <- chromatogram_data()
    RTs <-  chromatogram_results$RTs
    Heights  <- chromatogram_results$Heights
    Widths <- chromatogram_results$Widths
    Areas <- chromatogram_results$Areas
    
    # Calculate adjusted retention times
    result <- data.frame(
      Compound = sapply(compounds, function(c) c$name),
      stringsAsFactors = FALSE
    )
    
    result$RT <- sprintf("%.3f",round(RTs,3))
    result$Height <- sprintf("%.3f",round(Heights,3))
    
    # Add peak width
    result$Width <- sprintf("%.3f",round(Widths, 3))
    
    # Add peak area
    result$Area <- sprintf("%.3f",round(Areas, 3))
    
    result
  })

  history_data <- reactiveVal(
    data.frame(
      Exp = numeric(0),

      Compound = character(0),
      
      Flow = numeric(0),
      Vol = numeric(0),
      
      
      Wavelength = numeric(0),
      C_Length = numeric(0),
      C_Diameter = numeric(0),
      Size = numeric(0),
      Phase = numeric(0),
      Ionic_Str = numeric(0),
      pH = numeric(0),
      Temp = numeric(0),
      
      RT = numeric(0),
      Height = numeric(0),
      Width = numeric(0),
      Area = character(0)
    )
  )
  
  observeEvent(input$generate, {
    req(length(input$compounds) > 0)
    
    compounds <- selected_compounds()
    
    chromatogram_results <- chromatogram_data()
    RTs <-  chromatogram_results$RTs
    Heights  <- chromatogram_results$Heights
    Widths <- chromatogram_results$Widths
    Areas <- chromatogram_results$Areas
    
    # Calculate adjusted retention times
    result <- data.frame(
      Compound = sapply(compounds, function(c) c$name),
      stringsAsFactors = FALSE
    )

    current_data <- history_data()
    
    # Calculate experiment number
    exp_number <- 1
    if (!is.null(current_data) && nrow(current_data) > 0) {
      exp_number <- max(current_data$Exp) + 1
    }
    result$Exp = exp_number
    
    
    result$Flow <- input$flow_rate
    result$Vol <- input$injection_volume
    
    result$Wavelength <- input$detection_wavelength
    result$C_Length <- input$column_length
    result$C_Diameter <- input$column_diameter
    result$Size <- input$particle_size
    result$Phase <- input$mobile_phase_composition
    result$Ionic_Str <- input$ionic_strength
    result$pH <- input$ph
    result$Temp <- input$temperature
      
    
    result$RT <- sprintf("%.3f",round(RTs,3))
    result$Height <- sprintf("%.3f",round(Heights,3))
    
    # Add peak width
    result$Width <- sprintf("%.3f",round(Widths, 3))
    
    # Add peak area
    result$Area <- sprintf("%.3f",round(Areas, 3))

    result <- result[, c(2, 1, 3:ncol(result))]
    history_data(rbind(current_data,result))
  })

  # Render history table
  output$history_peak_info  <- renderDT({
    req(nrow(history_data()) > 0)
    datatable(history_data(),
              options = list(pageLength = 10,
                             autoWidth = TRUE,
                             scrollX = TRUE,
                             order = list(
                               list(0, 'desc')),  # Ordina per la colonna Exp (indice 0) in ordine discendente
                             columnDefs = list(
                               list(className = 'dt-center', targets = "_all")
                             )),
              rownames = FALSE)%>%
    #   formatStyle(
    #     columns = 7,
    #     backgroundColor = bslib::bs_get_variables(bslib::bs_theme(), "primary"),
    #     color = "white",
    #     fontWeight = "bold"
    #   )%>%
      formatStyle(
        columns = 13:16,
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
  
  observeEvent(input$reset_button, {
    # Restablece el reactiveVal a su valor inicial
    history_data(data.frame(
      Exp = numeric(0),
      
      Compound = character(0),
      
      Flow = numeric(0),
      Vol = numeric(0),
      
      
      Wavelength = numeric(0),
      C_Length = numeric(0),
      C_Diameter = numeric(0),
      Size = numeric(0),
      Phase = numeric(0),
      Ionic_Str = numeric(0),
      pH = numeric(0),
      Temp = numeric(0),
      
      RT = numeric(0),
      Height = numeric(0),
      Width = numeric(0),
      Area = character(0)
    ))
    # showNotification("Historical data has been reset", type = "message")
  })
}

# Run the app
shinyApp(ui, server)