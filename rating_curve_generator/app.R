packages <- c("ggplot2", "readxl", "shiny", "segmented")
for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

ui <- fluidPage(
  titlePanel("Rating Curve Generator"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload .xlsx File", accept = c(".xlsx")),
      numericInput("zero_flow_stage", "Stage at Zero Flow (a)", value = 0, step = 0.01),
      numericInput("max_discharge", "Maximum Discharge (ft^3/s)", value = 3000, min = 0.01),
      sliderInput("segments", "Number of Breakpoints",
                  min = 0, max = 2, value = 2, step = 1),
      actionButton("process", "Process Data")
    ),

    mainPanel(
      wellPanel(
        h3("Rating Curve Generator"),
        p("This app creates discharge rating curves from stage and discharge measurements."),
        tags$ul(
          tags$li("Navigate to Rating Review (RRT)"),
          tags$li("Open the field measurements tab"),
          tags$li("Check each measurement in the Use column to be included in the rating curve generator"),
          tags$li("Select Export Table in RRT and upload the downloaded .xlsx file to this script."),
          tags$li("*Do not edit the .xlsx file prior to import*"),
          tags$li("Set the number of segments for the Segmented Power Law model and enter the GZF and max discharge for the rating curve")
        ),
        p("The segmented model provides flexibility to handle different behaviors across the rating curve's range."),
        p("Note: This app is not yet configured to compute offset values."),
        p("For questions contact Rebecca Baggott rbaggott@usgs.gov.")
      ),
      conditionalPanel(
        condition = "input.process > 0 && output.hasBreakpoints == true",
        wellPanel(
          h4("Segment Breakpoints:"),
          tableOutput("breakpointsTable")
        )
      ),
      tableOutput("ratingTable"),
      plotOutput("ratingPlot"),
      h4("Model Fit Statistics:"),
      verbatimTextOutput("modelStats"),
      wellPanel(
        h4("Understanding the Statistics:"),
        p(strong("R-squared:"), "Indicates the proportion of variance in discharge explained by the model."),
        p(strong("Adjusted R-squared:"), "Adjusted for the number of predictors in the model."),
        p(strong("Root Mean Square Error (RMSE):"), "Measures the average deviation between observed and predicted values."),
        p(strong("Number of observations:"), "The total number of data points used in creating the model.")
      )
    )
  )
)

server <- function(input, output, session) {

  # Add output to indicate whether breakpoints exist
  output$hasBreakpoints <- reactive({
    req(data_reactive())
    model_data <- data_reactive()
    !is.null(model_data$model$psi)
  })

  # Make breakpoints available as a table
  output$breakpointsTable <- renderTable({
    req(data_reactive())
    model_data <- data_reactive()

    if(is.null(model_data$model$psi)) {
      return(NULL)
    }

    segmented_model <- model_data$model
    zero_flow_stage <- model_data$zero_flow_stage

    # Get breakpoints in original scale
    breakpoints <- 10^segmented_model$psi[,2] - zero_flow_stage

    # Calculate discharge at each breakpoint
    breakpoint_discharges <- predict_discharge(model_data, breakpoints)

    # Prepare table with segment number, breakpoint stage, and discharge
    result_df <- data.frame(
      Segment = paste("Segment", 1:(length(breakpoints) + 1)),
      Stage_Range = c(
        paste0("< ", round(breakpoints[1], 2), " ft"),
        sapply(1:(length(breakpoints)-1), function(i) {
          paste0(round(breakpoints[i], 2), " to ", round(breakpoints[i+1], 2), " ft")
        }),
        paste0("> ", round(breakpoints[length(breakpoints)], 2), " ft")
      ),
      Breakpoint_Stage = c(NA, round(breakpoints, 2)),
      Discharge_at_Breakpoint = c(NA, round(breakpoint_discharges, 2))
    )

    return(result_df)
  })

  data_reactive <- eventReactive(input$process, {
    req(input$file)

    tryCatch({
      field_measurements_export <- read_excel(input$file$datapath)

      # Check if the "Use" column exists
      if (!"Use" %in% names(field_measurements_export)) {
        showNotification("Warning: 'Use' column not found in the file. Using all data points.", type = "warning")
      } else {
        # Filter to keep only rows where Use is exactly TRUE
        field_measurements_export <- field_measurements_export[field_measurements_export$Use == TRUE, ]

        # Check if any rows remain after filtering
        if (nrow(field_measurements_export) == 0) {
          showNotification("Error: No data points with 'Use' set to TRUE were found.", type = "error")
          return(NULL)
        }
      }

      if (!all(c("Gage height (ft)", "Discharge (ft^3/s)") %in% names(field_measurements_export))) {
        showNotification("Error: Required columns 'Gage height (ft)' and 'Discharge (ft^3/s)' are missing.", type = "error")
        return(NULL)
      }

      gage_height <- field_measurements_export$`Gage height (ft)`
      discharge <- field_measurements_export$`Discharge (ft^3/s)`

      # Check for missing or invalid data
      if (is.null(gage_height) || is.null(discharge)) {
        showNotification("Error: File doesn't contain required columns. Please check format.", type = "error")
        return(NULL)
      }

      if (any(is.na(gage_height)) || any(is.na(discharge))) {
        showNotification("Warning: Data contains missing values. They will be removed.", type = "warning")
        valid_rows <- !is.na(gage_height) & !is.na(discharge)
        gage_height <- gage_height[valid_rows]
        discharge <- discharge[valid_rows]
      }

      # Check for zero or negative values
      if (any(discharge <= 0)) {
        showNotification("Warning: Zero or negative discharge values found. Using a small positive value.", type = "warning")
        discharge[discharge <= 0] <- 0.0001  # Replace with a small positive value
      }

      data <- data.frame(gage_height, discharge)

      # Apply stage at zero flow (datum correction)
      zero_flow_stage <- input$zero_flow_stage
      data$adjusted_stage <- data$gage_height + zero_flow_stage

      # Check for non-positive adjusted stage values
      if (any(data$adjusted_stage <= 0)) {
        showNotification("Error: Adjusted stage values are zero or negative. Please check your Stage at Zero Flow value.", type = "error")
        return(NULL)
      }

      # Log transform data
      data$log_adjusted_stage <- log10(data$adjusted_stage)
      data$log_discharge <- log10(data$discharge)

      # Always use segmented model
      initial_model <- lm(log_discharge ~ log_adjusted_stage, data = data)

      # Try to fit segmented model
      tryCatch({
        # Use the number of breakpoints from the slider input
        breakpoints_count <- input$segments

        # Use linear model if there are too few data points
        if (nrow(data) < (breakpoints_count + 2)) {  # Need sufficient points for the selected number of breakpoints
          showNotification("Not enough data points for segmentation. Using linear model.", type = "warning")
          model <- initial_model

          result <- list(
            data = data,
            model = model,
            zero_flow_stage = zero_flow_stage
          )
        } else {
          # Starting points for segmentation based on the number of breakpoints
          psi <- quantile(data$log_adjusted_stage, probs = seq(0, 1, length.out = breakpoints_count + 2)[-c(1, breakpoints_count + 2)])

          segmented_model <- tryCatch({
            segmented(initial_model, seg.Z = ~log_adjusted_stage, psi = psi)
          }, error = function(e) {
            showNotification(paste("Segmentation error:", e$message, ". Using linear model."), type = "warning")
            return(initial_model)
          })

          result <- list(
            data = data,
            model = segmented_model,
            zero_flow_stage = zero_flow_stage
          )
        }
      }, error = function(e) {
        # Fallback to linear model if segmentation fails
        showNotification(paste("Segmentation failed:", e$message, ". Using linear model instead."), type = "warning")
        model <- initial_model

        result <- list(
          data = data,
          model = model,
          zero_flow_stage = zero_flow_stage
        )
      })

      return(result)
    }, error = function(e) {
      showNotification(paste("Error processing file:", e$message), type = "error")
      return(NULL)
    })
  })

  # Modified predict_discharge function to use smoothing instead of enforcing monotonicity
  predict_discharge <- function(model_data, stage_values) {
    if (is.null(model_data)) return(NULL)

    # Remove missing stage values
    stage_values <- stage_values[!is.na(stage_values)]

    zero_flow_stage <- model_data$zero_flow_stage
    adjusted_stage <- stage_values + zero_flow_stage

    # Safety check for log transformation
    if (any(adjusted_stage <= 0)) {
      adjusted_stage[adjusted_stage <= 0] <- 0.0001
    }

    log_adjusted_stage <- log10(adjusted_stage)

    # Get log discharge predictions
    log_discharge <- predict(model_data$model, newdata = data.frame(log_adjusted_stage = log_adjusted_stage))
    
    # Convert to discharge
    discharge <- 10^(log_discharge)
    
    # Sort values to ensure proper smoothing
    sorted_indices <- order(stage_values)
    stage_sorted <- stage_values[sorted_indices]
    discharge_sorted <- discharge[sorted_indices]
    
    # Only apply smoothing if we have enough points
    if (length(discharge_sorted) > 5) {
      # Apply smoothing while preserving the overall relationship
      tryCatch({
        # Use loess for smoothing with a small span to preserve breakpoints
        smooth_fit <- loess(discharge_sorted ~ stage_sorted, span = 0.2, degree = 1)
        smoothed_discharge <- predict(smooth_fit, stage_sorted)
        
        # Check for non-monotonicity in smoothed results
        is_monotonic <- all(diff(smoothed_discharge) >= 0)
        
        # Only use smoothed values if they maintain monotonicity
        if (is_monotonic) {
          discharge_sorted <- smoothed_discharge
        } else {
          # Fall back to spline smoothing with monotonic constraint
          require(splines)
          x_range <- range(stage_sorted)
          knots <- seq(x_range[1], x_range[2], length.out = min(10, length(stage_sorted)/4))
          spline_fit <- smooth.spline(stage_sorted, discharge_sorted, df = min(10, length(stage_sorted)/3))
          smoothed_discharge <- predict(spline_fit, stage_sorted)$y
          
          # Ensure monotonicity if splines didn't preserve it
          if (any(diff(smoothed_discharge) <= 0)) {
            # Use cummax to enforce monotonicity while preserving the smooth shape
            smoothed_discharge <- cummax(smoothed_discharge)
          }
          
          discharge_sorted <- smoothed_discharge
        }
      }, error = function(e) {
        # If smoothing fails, ensure monotonicity directly
        for (i in 2:length(discharge_sorted)) {
          if (discharge_sorted[i] <= discharge_sorted[i-1]) {
            discharge_sorted[i] <- discharge_sorted[i-1] * 1.001
          }
        }
      })
    }
    
    # Put back in original order
    discharge[sorted_indices] <- discharge_sorted
    
    return(discharge)
  }

  rating_table_reactive <- reactive({
    req(data_reactive())

    model_data <- data_reactive()
    max_discharge <- input$max_discharge
    zero_flow_stage <- model_data$zero_flow_stage

    # Generate a sequence of gage heights based on the data range
    data <- model_data$data
    min_gage <- min(data$gage_height, na.rm = TRUE)
    max_gage <- max(data$gage_height, na.rm = TRUE) * 1.2  # Extend slightly beyond observed data

    # Add points to cover zero flow and max discharge
    very_low_stage <- max(0.01, min_gage * 0.5)
    highest_observed_discharge <- max(data$discharge, na.rm = TRUE)
    highest_observed_stage <- max(data$gage_height, na.rm = TRUE)
    extrapolation_factor <- (max_discharge / highest_observed_discharge) ^ 0.5
    estimated_max_stage <- highest_observed_stage * extrapolation_factor

    log_spacing <- exp(seq(log(very_low_stage + 0.001), log(max_gage + 0.001), length.out = 30)) - 0.001
    low_end_points <- seq(very_low_stage, min_gage + (max_gage - min_gage) * 0.2, length.out = 10)
    high_end_points <- seq(max_gage, estimated_max_stage, length.out = 10)

    gage_height_table <- sort(unique(c(low_end_points, log_spacing, high_end_points)))

    # Calculate discharge using the appropriate model
    predicted_discharge_table <- predict_discharge(model_data, gage_height_table)

    # Remove rows with missing values
    valid_indices <- !is.na(gage_height_table) & !is.na(predicted_discharge_table)
    gage_height_table <- gage_height_table[valid_indices]
    predicted_discharge_table <- predicted_discharge_table[valid_indices]

    result_df <- data.frame(
      Gage_Height_ft = round(gage_height_table, 2),
      Discharge_ft3s = round(predicted_discharge_table, 2)
    )

    result_df <- result_df[result_df$Discharge_ft3s <= max_discharge, ]
    result_df <- result_df[result_df$Discharge_ft3s >= 0.01, ]

    result_df <- result_df[order(result_df$Gage_Height_ft), ]

    return(result_df)
  })

  output$ratingTable <- renderTable({
    rating_table_reactive()
  })

  output$ratingPlot <- renderPlot({
    req(data_reactive())
    model_data <- data_reactive()
    if(is.null(model_data)) return(NULL)

    data <- model_data$data
    max_discharge <- input$max_discharge

    # Generate a sequence of gage heights based on the data range
    min_gage <- min(data$gage_height)
    max_gage <- max(data$gage_height) * 1.2  # Extend slightly beyond observed data

    # Create non-uniform spacing with more points at lower values
    # Use logarithmic spacing to concentrate points at the low end
    log_spacing <- exp(seq(log(min_gage + 0.001), log(max_gage + 0.001), length.out = 500)) - 0.001

    # Add extra points at the low end for smoother curve representation
    low_end_points <- seq(min_gage, min_gage + (max_gage - min_gage) * 0.2, length.out = 100)

    # Combine and sort all points, then remove duplicates
    gage_height_seq <- sort(unique(c(low_end_points, log_spacing)))

    # Add extra points near segment breakpoints for smoother transitions
    if (!is.null(model_data$model$psi)) {
      breakpoints <- 10^model_data$model$psi[,2] - model_data$zero_flow_stage

      for (bp in breakpoints) {
        # Add more points closely spaced around each breakpoint for better smoothing
        extra_points <- seq(bp - 0.05, bp + 0.05, length.out = 40)
        gage_height_seq <- sort(unique(c(gage_height_seq, extra_points)))
      }
    }

    predicted_discharge <- predict_discharge(model_data, gage_height_seq)

    # Remove missing values
    valid_indices <- !is.na(gage_height_seq) & !is.na(predicted_discharge)
    gage_height_seq <- gage_height_seq[valid_indices]
    predicted_discharge <- predicted_discharge[valid_indices]

    predicted_data <- data.frame(
      gage_height = gage_height_seq,
      discharge = predicted_discharge
    )

    # Filter out values above max_discharge
    valid_indices <- which(predicted_discharge <= max_discharge)
    predicted_data <- data.frame(
      gage_height = gage_height_seq[valid_indices],
      discharge = predicted_discharge[valid_indices]
    )

    # Calculate reasonable x-axis limits based on data
    min_x <- max(0.01, min(data$discharge) * 0.5)
    max_x <- min(max_discharge, max(data$discharge) * 2)

    p <- ggplot(data, aes(x = discharge, y = gage_height)) +
      geom_point(size = 3) +
      geom_line(data = predicted_data, aes(x = discharge, y = gage_height), color = "blue", linewidth = 1) +
      scale_x_log10(limits = c(min_x, max_x)) +
      # Use regular scale for y if range is small, log scale if range is large
      {if(max(data$gage_height)/min(data$gage_height) > 10) scale_y_log10() else scale_y_continuous()} +
      labs(title = "Rating Curve", x = "Discharge (ft^3/s)", y = "Gage Height (ft)") +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold")
      )

    # Add segment breakpoint lines with labels
    segmented_model <- model_data$model
    zero_flow_stage <- model_data$zero_flow_stage

    # Get breakpoints (in original gage height units)
    if (inherits(segmented_model, "segmented") && !is.null(segmented_model$psi)) {
      breakpoints <- 10^segmented_model$psi[,2] - zero_flow_stage

      # For each breakpoint, add a horizontal line and a label
      for (bp in breakpoints) {
        # Calculate the discharge at this breakpoint for label placement
        bp_discharge <- predict_discharge(model_data, bp)[1]

        # Add horizontal line at breakpoint stage height
        p <- p + geom_hline(yintercept = bp,
                           color = "red", linetype = "dashed", alpha = 0.7) +
                geom_point(data = data.frame(discharge = bp_discharge, gage_height = bp),
                          color = "red", size = 4, shape = 16) +
                # Add label showing both stage height and discharge values with black text for better legibility
                annotate("text", x = bp_discharge, y = bp * 1.05,
                        label = paste0("Stage: ", round(bp, 2), " ft\nQ: ", round(bp_discharge, 1), " cfs"),
                        color = "black", size = 3, fontface = "bold", hjust = 0)
      }

      # Add a legend for the breakpoints (changed to black text)
      p <- p + annotate("text", x = min(data$discharge) * 2, y = max(data$gage_height),
                       label = "Segment Breakpoints", color = "black")
    }

    p
  })

  # Update model stats to reflect only the model fit (no offsets)
  output$modelStats <- renderText({
    req(data_reactive())
    model_data <- data_reactive()
    data <- model_data$data

    # For segmented models, calculate manually
    model <- model_data$model

    # Safely get fitted values
    fitted_values <- tryCatch({
      fitted(model)
    }, error = function(e) {
      # If fitted() fails, try predict directly
      predict(model, newdata = data.frame(log_adjusted_stage = data$log_adjusted_stage))
    })

    observed_values <- data$log_discharge

    residuals <- observed_values - fitted_values
    SST <- sum((observed_values - mean(observed_values))^2)
    SSE <- sum(residuals^2)
    r_squared <- 1 - (SSE/SST)

    # Adjusted R-squared
    n <- length(observed_values)
    p <- length(coef(model))
    adj_r_squared <- 1 - (1-r_squared)*((n-1)/(n-p-1))

    rmse <- sqrt(mean(residuals^2))

    segments_text <- if(!inherits(model, "segmented") || is.null(model$psi))
      "Single segment" else paste(nrow(model$psi) + 1, "segments")

    paste0("Model: ", segments_text, " Power Law",
           "\nR-squared: ", round(r_squared, 4),
           "\nAdjusted R-squared: ", round(adj_r_squared, 4),
           "\nRoot Mean Square Error (log scale): ", round(rmse, 4),
           "\nNumber of observations: ", nrow(data))
  })
}

shinyApp(ui = ui, server = server)