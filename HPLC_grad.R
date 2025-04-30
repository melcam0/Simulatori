library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)
library(openxlsx)

# Function to simulate HPLC chromatogram with gradient support
simulate_chromatogram <- function(compounds, flow_rate, column_length, 
                                  column_diameter, particle_size, 
                                  mobile_phase_composition, 
                                  detection_wavelength, 
                                  injection_volume, 
                                  noise_level,
                                  ionic_strength,
                                  ph,
                                  temperature,
                                  gradient_mode = "isocratic", 
                                  gradient_start = NA, 
                                  gradient_end = NA, 
                                  gradient_time = NA) {
  
  # Simulation parameters
  time_points <- seq(0, 15, by = 0.01)
  baseline <- 0.01 + noise_level * rnorm(length(time_points), mean = 0, sd = 0.5)
  
  peaks <- matrix(0, nrow = length(time_points), ncol = length(compounds))
  
  RTs <- numeric(length(compounds))
  Areas <- numeric(length(compounds))
  Widths <- numeric(length(compounds))
  Heights <- numeric(length(compounds))
  
  # Calculate mobile phase composition at each time point for gradient elution
  mobile_phase_at_time <- if (gradient_mode == "isocratic") {
    rep(mobile_phase_composition, length(time_points))
  } else {
    # Linear gradient calculation
    phase_values <- numeric(length(time_points))
    for (i in seq_along(time_points)) {
      t <- time_points[i]
      if (t <= 0) {
        phase_values[i] <- gradient_start
      } else if (t >= gradient_time) {
        phase_values[i] <- gradient_end
      } else {
        # Linear interpolation
        phase_values[i] <- gradient_start + (gradient_end - gradient_start) * (t / gradient_time)
      }
    }
    phase_values
  }
  
  # Calculate retention times based on parameters
  for (i in seq_along(compounds)) {
    if (gradient_mode == "isocratic") {
      # Base retention time for each compound
      base_rt <- compounds[[i]]$rt
      # Adjust for flow rate
      rt_adjusted <- base_rt * (1 / flow_rate) * 1.5
      # Adjust for column length
      rt_adjusted <- rt_adjusted * (column_length / 150)
      # Adjust for mobile phase composition
      rt_adjusted <- rt_adjusted * (1 - (mobile_phase_composition / 100) * compounds[[i]]$solvent_sensitivity)
      # Adjust for ionic strength
      ionic_factor <- 1 + (ionic_strength / 50) * compounds[[i]]$ionic_sensitivity
      rt_adjusted <- rt_adjusted * ionic_factor
      # Adjust for pH
      ph_difference <- abs(ph - compounds[[i]]$optimal_ph)
      ph_factor <- 1 + (ph_difference / 2) * compounds[[i]]$ph_sensitivity
      rt_adjusted <- rt_adjusted * ph_factor
      # Adjust for temperature
      temp_factor <- (1 - (temperature - 30) * 0.015)
      rt_adjusted <- rt_adjusted * temp_factor
      
      # Add noise to RT
      rt_adjusted <- rt_adjusted * (1 + rnorm(1, 0, 0.01))
      RTs[i] <- rt_adjusted
    } else {
      # For gradient elution, use a more complex model
      # This is a simplified model based on LSS (Linear Solvent Strength) theory
      
      # k' = k0 * e^(-S*φ) where k0 is the retention factor in pure water, S is compound-specific, φ is organic content
      
      # Estimate when the compound will elute in the gradient
      k0 <- compounds[[i]]$rt * 5  # Approximation of k0 based on isocratic rt
      S <- compounds[[i]]$solvent_sensitivity * 10  # Scale solvent sensitivity
      
      # Simplified gradient elution time calculation
      b <- (gradient_end - gradient_start) / gradient_time  # Gradient slope in % per minute
      
      # Gradient retention time calculation
      t0 <- 0.5 * (column_length / 150) * (1 / flow_rate)  # Approximate void time
      gradient_rt <- t0 + (log(k0) / (S * b * flow_rate))
      
      # Apply other factors that would still affect retention in gradient
      gradient_rt <- gradient_rt * (column_length / 150)  # Column length effect
      gradient_rt <- gradient_rt * (1 + (ionic_strength / 50) * compounds[[i]]$ionic_sensitivity)  # Ionic strength
      
      # pH effect
      ph_difference <- abs(ph - compounds[[i]]$optimal_ph)
      ph_factor <- 1 + (ph_difference / 2) * compounds[[i]]$ph_sensitivity
      gradient_rt <- gradient_rt * ph_factor
      
      # Temperature effect
      temp_factor <- (1 - (temperature - 30) * 0.015)
      gradient_rt <- gradient_rt * temp_factor
      
      # Add noise to RT
      gradient_rt <- gradient_rt * (1 + rnorm(1, 0, 0.01))
      
      # Ensure the RT is within our simulation time frame
      gradient_rt <- max(0.5, min(14.5, gradient_rt))
      
      RTs[i] <- gradient_rt
    }
    
    # Adjust peak width
    if (gradient_mode == "isocratic") {
      peak_width <- 0.1 * (particle_size / 3) * (column_diameter / 4.6)
    } else {
      # In gradient mode, peaks are typically sharper
      peak_width <- 0.07 * (particle_size / 3) * (column_diameter / 4.6)
    }
    
    # Temperature also affects peak width
    peak_width <- peak_width * (1 - (temperature - 30) * 0.01)
    peak_width <- peak_width * (1 + rnorm(1, 0, 0.05))
    Widths[i] <- peak_width
    
    # Adjust peak height based on concentration and detection properties
    peak_height <- compounds[[i]]$concentration * (injection_volume / 10) * 
      compounds[[i]]$response_factors[detection_wavelength]
    peak_height <- peak_height * (1 + rnorm(1, 0, 0.02))
    Heights[i] <- peak_height
    
    # Generate peak
    peaks[, i] <- peak_height * exp(-0.5 * ((time_points - RTs[i]) / peak_width)^2)
    
    # Calculate peak area - numerical integration
    Areas[i] <- sum(peaks[, i]) * (time_points[2] - time_points[1])
  }
  
  # Combine all peaks and add to baseline
  signal <- baseline + rowSums(peaks)
  
  # Return data frame with time and signal
  list(
    data = data.frame(
      Time = time_points,
      Signal = signal,
      MobilePhase = mobile_phase_at_time  # Add mobile phase composition at each time point
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
        # Elution mode selection
        selectInput("gradient_mode", "Elution Mode", 
                    choices = c("isocratic", "gradient"), 
                    selected = "isocratic"),
        
        # Show different controls based on elution mode
        conditionalPanel(
          condition = "input.gradient_mode == 'isocratic'",
          sliderInput("mobile_phase_composition", "Organic Phase (%)", 
                      min = 0, max = 100, value = 50, step = 5)
        ),
        
        conditionalPanel(
          condition = "input.gradient_mode == 'gradient'",
          sliderInput("gradient_start", "Initial Organic Phase (%)", 
                      min = 0, max = 95, value = 5, step = 5),
          sliderInput("gradient_end", "Final Organic Phase (%)", 
                      min = 5, max = 100, value = 95, step = 5),
          sliderInput("gradient_time", "Gradient Time (min)", 
                      min = 1, max = 15, value = 10, step = 1)
        ),
        
        sliderInput("ionic_strength", "Ionic Strength (mM)", 
                    min = 0, max = 50, value = 20, step = 1),
        
        sliderInput("ph", "pH", 
                    min = 5, max = 7, value = 6, step = 0.1),
        
        sliderInput("temperature", "Temperature (°C)", 
                    min = 20, max = 50, value = 30, step = 1),
        
        actionButton("generate", "Data generator", class = "btn-primary btn-lg w-100 mt-3")
      ),
      
      layout_columns(
        card(
          min_height = "375px",
          card_header("Simulated Chromatogram"),
          conditionalPanel(
            condition = "input.generate == 0",
            div(
              style = "color: #d9534f; font-weight: bold; border: 2px dashed #d9534f; padding: 15px; text-align: center; margin: 10px 0;",
              "Press the 'Data generator' button to start the simulation."
            )
          ),
          conditionalPanel(
            condition = "input.generate > 0",
            plotOutput("chromatogram_plot"),
            conditionalPanel(
              condition = "input.gradient_mode == 'gradient'",
              plotOutput("gradient_plot", height = "130px")
            )
          )
        ),
        
        card(
          min_height = "375px",
          card_header("Peak Information"),
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
      )
      
      # card(
      #   height = 200,
      #   card_header("Simulator Information"),
      #   p("This HPLC simulator allows you to explore how different parameters affect chromatographic separation."),
      #   p("The simulator models the effects of flow rate, column dimensions, mobile phase composition, detector settings, ionic strength, pH, and temperature on the resulting chromatogram."),
      #   p("You can now simulate both isocratic and gradient elution methods to observe how they affect peak resolution and retention time."),
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
      DTOutput("history_peak_info")
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
  selected_compounds <- eventReactive(input$generate, {
    compounds_db[match(input$compounds, sapply(compounds_db, function(c) c$name))]
  })
  
  # Initialize reactive value for storing historical data
  history_data <- reactiveVal(
    data.frame(
      Exp = numeric(0),
      Compound = character(0),
      Elution_Mode = character(0),
      Flow = numeric(0),
      Vol = numeric(0),
      Wavelength = character(0),
      C_Length = numeric(0),
      C_Diameter = numeric(0),
      Size = numeric(0),
      Phase = numeric(0),      # For isocratic
      Grad_Start = numeric(0), # For gradient
      Grad_End = numeric(0),   # For gradient
      Grad_Time = numeric(0),  # For gradient
      Ionic_Str = numeric(0),
      pH = numeric(0),
      Temp = numeric(0),
      RT = character(0),
      Height = character(0),
      Width = character(0),
      Area = character(0)
    )
  )
  
  # Counter for experiment number
  experiment_counter <- reactiveVal(0)
  
  # Simulate chromatogram when inputs change
  chromatogram_data <- eventReactive(input$generate, {
    req(length(input$compounds) > 0)
    
    # Get mobile phase parameters based on elution mode
    mobile_phase_composition <- if(input$gradient_mode == "isocratic") {
      input$mobile_phase_composition
    } else {
      NA  # Not used directly for gradient mode
    }
    
    # Additional gradient parameters
    gradient_start <- if(input$gradient_mode == "gradient") input$gradient_start else NA
    gradient_end <- if(input$gradient_mode == "gradient") input$gradient_end else NA
    gradient_time <- if(input$gradient_mode == "gradient") input$gradient_time else NA
    
    # Increment experiment counter
    experiment_counter(experiment_counter() + 1)
    
    simulate_chromatogram(
      compounds = selected_compounds(),
      flow_rate = input$flow_rate,
      column_length = input$column_length,
      column_diameter = as.numeric(input$column_diameter),
      particle_size = as.numeric(input$particle_size),
      mobile_phase_composition = mobile_phase_composition,
      detection_wavelength = input$detection_wavelength,
      injection_volume = input$injection_volume,
      noise_level = input$noise_level,
      ionic_strength = input$ionic_strength,
      ph = input$ph,
      temperature = input$temperature,
      gradient_mode = input$gradient_mode,
      gradient_start = gradient_start,
      gradient_end = gradient_end,
      gradient_time = gradient_time
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
  
  # Plot gradient profile for gradient mode
  output$gradient_plot <- renderPlot({
    req(input$gradient_mode == "gradient")
    data <- chromatogram_data()$data
    
    ggplot(data, aes(x = Time, y = MobilePhase)) +
      geom_line(linewidth = 1.2, color = "green4") +
      labs(
        x = "Time (min)",
        y = "% Organic",
        title = "Gradient Profile"
      ) +
      ylim(0, 100) +
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)
      )
  })
  
  # Generate peak information table
  output$peak_info <- renderTable({
    req(length(input$compounds) > 0)
    
    compounds <- selected_compounds()
    
    chromatogram_results <- chromatogram_data()
    RTs <- chromatogram_results$RTs
    Heights <- chromatogram_results$Heights
    Widths <- chromatogram_results$Widths
    Areas <- chromatogram_results$Areas
    
    # Calculate adjusted retention times
    result <- data.frame(
      Compound = sapply(compounds, function(c) c$name),
      stringsAsFactors = FALSE
    )
    
    result$RT <- sprintf("%.3f", round(RTs, 3))
    result$Height <- sprintf("%.3f", round(Heights, 3))
    
    # Add peak width
    result$Width <- sprintf("%.3f", round(Widths, 3))
    
    # Add peak area
    result$Area <- sprintf("%.3f", round(Areas, 3))
    
    result
  })
  
  # Update historical data when a new simulation is run
  observeEvent(input$generate, {
    req(length(input$compounds) > 0)
    
    compounds <- selected_compounds()
    
    chromatogram_results <- chromatogram_data()
    RTs <- chromatogram_results$RTs
    Heights <- chromatogram_results$Heights
    Widths <- chromatogram_results$Widths
    Areas <- chromatogram_results$Areas
    
    # Create new data for this experiment
    new_data <- data.frame()
    
    for (i in seq_along(compounds)) {
      # Use appropriate mobile phase parameters based on elution mode
      phase_val <- NA
      grad_start_val <- NA
      grad_end_val <- NA
      grad_time_val <- NA
      
      if (input$gradient_mode == "isocratic") {
        phase_val <- input$mobile_phase_composition
      } else {
        grad_start_val <- input$gradient_start
        grad_end_val <- input$gradient_end
        grad_time_val <- input$gradient_time
      }
      
      # Add one row for each compound in this experiment
      new_row <- data.frame(
        Exp = experiment_counter(),
        Compound = compounds[[i]]$name,
        Elution_Mode = input$gradient_mode,
        Flow = input$flow_rate,
        Vol = input$injection_volume,
        Wavelength = input$detection_wavelength,
        C_Length = input$column_length,
        C_Diameter = as.numeric(input$column_diameter),
        Size = as.numeric(input$particle_size),
        Phase = phase_val,
        Grad_Start = grad_start_val,
        Grad_End = grad_end_val,
        Grad_Time = grad_time_val,
        Ionic_Str = input$ionic_strength,
        pH = input$ph,
        Temp = input$temperature,
        RT = sprintf("%.3f", round(RTs[i], 3)),
        Height = sprintf("%.3f", round(Heights[i], 3)),
        Width = sprintf("%.3f", round(Widths[i], 3)),
        Area = sprintf("%.3f", round(Areas[i], 3)),
        stringsAsFactors = FALSE
      )
      
      # Combine with existing new data
      new_data <- rbind(new_data, new_row)
    }
    
    # Combine with historical data
    history_data(rbind(history_data(), new_data))
  })
  
  # Display historical data
  output$history_peak_info <- renderDT({
    datatable(history_data(), 
              options = list(pageLength = 15, 
                             autoWidth = TRUE,
                             scrollX = TRUE),
              rownames = FALSE)
  })
  
  # Download historical data as Excel
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("HPLC_Simulation_Data_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx", sep = "")
    },
    content = function(file) {
      # Create a new workbook
      wb <- createWorkbook()
      
      # Add a worksheet
      addWorksheet(wb, "Simulation Data")
      
      # Write the data to the worksheet
      writeData(wb, "Simulation Data", history_data())
      
      # Apply some styling
      headerStyle <- createStyle(
        fontColour = "#FFFFFF", 
        fgFill = "#4F81BD",
        halign = "center",
        textDecoration = "bold"
      )
      
      # Apply style to header row
      addStyle(wb, "Simulation Data", headerStyle, rows = 1, cols = 1:ncol(history_data()))
      
      # Save the workbook
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # Reset historical data
  observeEvent(input$reset_button, {
    history_data(data.frame(
      Exp = numeric(0),
      Compound = character(0),
      Elution_Mode = character(0),
      Flow = numeric(0),
      Vol = numeric(0),
      Wavelength = character(0),
      C_Length = numeric(0),
      C_Diameter = numeric(0),
      Size = numeric(0),
      Phase = numeric(0),      # For isocratic
      Grad_Start = numeric(0), # For gradient
      Grad_End = numeric(0),   # For gradient
      Grad_Time = numeric(0),  # For gradient
      Ionic_Str = numeric(0),
      pH = numeric(0),
      Temp = numeric(0),
      RT = character(0),
      Height = character(0),
      Width = character(0),
      Area = character(0)
    ))
    
    experiment_counter(0)
    
    showNotification("Historical data has been reset", type = "message")
  })
}



# Run the app
shinyApp(ui, server)                                  