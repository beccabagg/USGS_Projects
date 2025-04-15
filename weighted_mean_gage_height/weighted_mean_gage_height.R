# List of required packages
required_packages <- c("shiny", "DT", "dplyr", "lubridate", "shinyjs")

# Loop through each package to install (if needed) and load
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing package:", pkg))
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Weighted Mean Gage Height Calculator"),

  sidebarLayout(
    sidebarPanel(width = 4,
                 h4("Step 1: Enter Time-Discharge Data"),
                 helpText("Copy data from Excel/CSV (2 columns: Time, Discharge)"),
                 # Make the textarea larger and ensure it's visible
                 tags$textarea(id = "paste_area", rows = 10, cols = 30,
                               placeholder = "Paste your time and discharge data here...",
                               style = "width:100%; border:1px solid #ccc; padding:5px;"),
                 br(),
                 actionButton("load_pasted", "Load Time-Discharge Data",
                              style = "margin-top:10px; margin-bottom:10px;"),
                 hr(),
                 h4("Step 2: Enter Gage Heights"),
                 helpText("After loading time-discharge data, enter gage heights for:"),
                 helpText("- The 15-minute interval before the start time"),
                 helpText("- The exact start time"),
                 helpText("- Every 15-minute interval between start and end"),
                 helpText("- The exact end time"),
                 helpText("- The 15-minute interval after the end time"),
                 actionButton("generate_gage_inputs", "Generate Gage Height Inputs"),
                 hr(),
                 h4("Step 3: Calculate Results"),
                 actionButton("calculate", "Calculate Weighted Means"),
                 br(), br(),
                 downloadButton("downloadData", "Download Results")
    ),

    mainPanel(width = 8,
      tabsetPanel(
        tabPanel("Time-Discharge Data",
                h4("Time and Discharge Pairs:"),
                DTOutput("time_discharge_table")),
        tabPanel("Gage Heights",
                h4("Gage Height Input Form:"),
                fluidRow(
                  column(4,
                        h5("Set Time Range for Gage Heights"),
                        textInput("gage_start_time", "Start Time (military format)",
                                placeholder = "e.g., 1430"),
                        textInput("gage_end_time", "End Time (military format)",
                                placeholder = "e.g., 1645")
                  ),
                  column(8,
                        br(),
                        helpText("Enter custom start/end times for gage heights, or leave blank to use discharge time range"),
                        actionButton("generate_gage_inputs", "Generate Gage Height Inputs",
                                  style = "margin-top:10px;")
                  )
                ),
                hr(),
                uiOutput("gage_height_inputs"),
                br(),
                actionButton("save_gage_heights", "Save Gage Heights")),
        tabPanel("Time-Weighted Results",
                h4("Time-Weighted Mean Gage Height:"),
                verbatimTextOutput("weighted_mean_gage"),
                h4("Calculation Details:"),
                helpText("This calculation weights each gage height by the time duration it represents."),
                verbatimTextOutput("calculation_details")),
        tabPanel("Volume-Weighted Results",
                h4("Volume-Weighted Mean Gage Height:"),
                verbatimTextOutput("volume_weighted_mean_gage"),
                h4("Calculation Details:"),
                helpText("This calculation weights each gage height by the volume of water (cumulative discharge) that passed through during its period."),
                verbatimTextOutput("volume_calculation_details")),
        tabPanel("Combined Results",
                h4("Average of Time and Volume-Weighted Results:"),
                verbatimTextOutput("combined_mean_gage"),
                h4("Calculation Details:"),
                verbatimTextOutput("combined_calculation_details"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive values to store the data
  values <- reactiveValues(
    time_discharge = data.frame(
      Time = character(0),
      Discharge = numeric(0),
      stringsAsFactors = FALSE
    ),
    gage_heights = NULL,
    gage_times = NULL,
    gage_times_decimal = NULL,
    weighted_mean_gage = NULL,
    volume_weighted_mean_gage = NULL,
    calculation_details = NULL,
    volume_calculation_details = NULL
  )

  # Process pasted time-discharge data
  observeEvent(input$load_pasted, {
    pasted_text <- input$paste_area
    if (pasted_text == "") {
      showNotification("No data pasted", type = "warning")
      return()
    }

    # Process the pasted text
    tryCatch({
      # Split by newline and then by tab or comma
      lines <- strsplit(pasted_text, "[\r\n]+")[[1]]
      lines <- lines[lines != ""] # Remove empty lines

      processed_data <- lapply(lines, function(line) {
        # Try different delimiters (tab, comma, semicolon)
        values <- strsplit(line, "[\t,;]+")[[1]]
        values <- trimws(values) # Trim whitespace

        if (length(values) < 2) {
          showNotification("Each line should have at least 2 values", type = "error")
          return(NULL)
        }

        time_val <- values[1]
        discharge_val <- as.numeric(values[2])

        if (is.na(discharge_val)) {
          showNotification("Non-numeric discharge value found", type = "error")
          return(NULL)
        }

        # Clean up the time format - remove any non-numeric characters
        time_val <- gsub("[^0-9]", "", time_val)

        # Validate military time format
        if (!grepl("^[0-9]{3,4}$", time_val)) {
          showNotification("Invalid time format. Use military time (e.g., 1400)", type = "error")
          return(NULL)
        }

        return(list(Time = time_val, Discharge = discharge_val))
      })

      # Check for NULL values (errors)
      if (any(sapply(processed_data, is.null))) {
        return()
      }

      # Convert to data frame
      new_data <- data.frame(
        Time = sapply(processed_data, function(x) x$Time),
        Discharge = sapply(processed_data, function(x) x$Discharge),
        stringsAsFactors = FALSE
      )

      # Validate the number of rows
      n_rows <- nrow(new_data)
      if (n_rows < 2) {
        showNotification("At least 2 data points are required", type = "error")
        return()
      }

      # Update the data
      values$time_discharge <- new_data

      # Reset gage heights when new time-discharge data is loaded
      values$gage_heights <- NULL
      values$gage_times <- NULL
      values$gage_times_decimal <- NULL

      # Show a success message with the data that was loaded
      showNotification(paste("Successfully loaded", n_rows, "time-discharge pairs"), type = "message")

    }, error = function(e) {
      showNotification(paste("Error processing data:", e$message), type = "error")
    })
  })

  # Display time-discharge table
  output$time_discharge_table <- renderDT({
    if(nrow(values$time_discharge) == 0) {
      return(data.frame(Message = "No data loaded yet. Paste data and click 'Load Time-Discharge Data'"))
    }

    DT::datatable(values$time_discharge,
                  editable = TRUE,
                  options = list(
                    pageLength = 30,
                    lengthMenu = c(10, 15, 20, 25, 30)
                  ))
  })

  # Update time_discharge data when table is edited
  observeEvent(input$time_discharge_table_cell_edit, {
    info <- input$time_discharge_table_cell_edit
    i <- info$row
    j <- info$col
    v <- info$value

    # Validate time format if editing Time column
    if (j == 1) {
      # Clean up the time format - remove any non-numeric characters
      v <- gsub("[^0-9]", "", v)

      # Check if input can be parsed as military time
      if (!grepl("^[0-9]{3,4}$", v)) {
        showNotification("Please enter time in military format (e.g., 1400)", type = "error")
        return()
      }
    }

    # Update the data
    values$time_discharge[i, j] <- v

    # Reset gage heights when time-discharge data is edited
    values$gage_heights <- NULL
    values$gage_times <- NULL
    values$gage_times_decimal <- NULL
  })

  # Helper function to convert military time to decimal hours
  military_to_decimal <- function(time_str) {
    # Clean up and ensure 4 digits
    time_str <- gsub("[^0-9]", "", time_str)
    if (nchar(time_str) == 3) time_str <- paste0("0", time_str)

    # Extract hours and minutes
    hours <- as.numeric(substr(time_str, 1, 2))
    minutes <- as.numeric(substr(time_str, 3, 4))

    # Return decimal hours
    return(hours + minutes/60)
  }

  # Helper function to convert decimal hours to military time
  decimal_to_military <- function(decimal_time) {
    # Ensure time is within 0-24 range
    decimal_time <- decimal_time %% 24

    # Extract hours and minutes
    hours <- floor(decimal_time)
    minutes <- round((decimal_time - hours) * 60)

    # Handle case where rounding minutes gives 60
    if (minutes == 60) {
      hours <- hours + 1
      minutes <- 0
    }

    # Format as military time
    return(sprintf("%02d%02d", hours, minutes))
  }

  # Generate inputs for gage heights at 15-minute intervals
  observeEvent(input$generate_gage_inputs, {
    req(values$time_discharge)

    if (nrow(values$time_discharge) < 2) {
      showNotification("At least 2 time-discharge pairs are needed", type = "error")
      return()
    }

    # Clean up and convert time values to proper format
    tryCatch({
      processed_data <- values$time_discharge %>%
        mutate(
          # Convert time to decimal hours
          Time_decimal = sapply(Time, military_to_decimal)
        )

      # Check for invalid time values
      if (any(is.na(processed_data$Time_decimal))) {
        showNotification("Invalid time values detected", type = "error")
        return()
      }

      # Sort by time for calculation
      processed_data <- processed_data %>% arrange(Time_decimal)

      # Determine if we're using custom times or the discharge data time range
      using_custom_times <- FALSE
      custom_start_time <- NULL
      custom_end_time <- NULL

      # Check if user entered custom start time
      if (!is.null(input$gage_start_time) && input$gage_start_time != "") {
        # Clean up and validate custom start time
        start_time_str <- gsub("[^0-9]", "", input$gage_start_time)
        if (grepl("^[0-9]{3,4}$", start_time_str)) {
          custom_start_time <- military_to_decimal(start_time_str)
          using_custom_times <- TRUE
        } else {
          showNotification("Invalid start time format. Use military time (e.g., 1400)", type = "error")
          return()
        }
      }

      # Check if user entered custom end time
      if (!is.null(input$gage_end_time) && input$gage_end_time != "") {
        # Clean up and validate custom end time
        end_time_str <- gsub("[^0-9]", "", input$gage_end_time)
        if (grepl("^[0-9]{3,4}$", end_time_str)) {
          custom_end_time <- military_to_decimal(end_time_str)
          using_custom_times <- TRUE
        } else {
          showNotification("Invalid end time format. Use military time (e.g., 1600)", type = "error")
          return()
        }
      }

      # Get start and end times in decimal hours
      if (using_custom_times) {
        # Use custom times if provided, otherwise fall back to discharge data
        start_time_decimal <- ifelse(!is.null(custom_start_time),
                                    custom_start_time,
                                    min(processed_data$Time_decimal))

        end_time_decimal <- ifelse(!is.null(custom_end_time),
                                  custom_end_time,
                                  max(processed_data$Time_decimal))
      } else {
        # Use time range from discharge data
        start_time_decimal <- min(processed_data$Time_decimal)
        end_time_decimal <- max(processed_data$Time_decimal)
      }

      # Handle case where end time is next day (e.g., start at 23:00, end at 01:00)
      spans_midnight <- FALSE
      if (end_time_decimal < start_time_decimal) {
        end_time_decimal <- end_time_decimal + 24
        spans_midnight <- TRUE
      }

      # Generate precise sequence of times:
      # 1. Find the nearest 15-minute mark BEFORE the start time
      # 2. Include exact start time
      # 3. Add all 15-minute intervals
      # 4. Include exact end time
      # 5. Add the nearest 15-minute mark AFTER the end time

      # First, find the nearest 15-minute mark before start time
      start_minutes <- floor(start_time_decimal * 60)
      prev_15min_mark_minutes <- start_minutes - (start_minutes %% 15)
      prev_15min_mark_decimal <- prev_15min_mark_minutes / 60

      # If start time is exactly on a 15-minute mark, go back one more
      if (prev_15min_mark_minutes == start_minutes) {
        prev_15min_mark_minutes = prev_15min_mark_minutes - 15
        prev_15min_mark_decimal = prev_15min_mark_minutes / 60
      }

      # Next, get the nearest 15-minute mark after start time
      first_15min_mark_minutes <- prev_15min_mark_minutes + 15
      first_15min_mark_decimal <- first_15min_mark_minutes / 60

      # Find the nearest 15-minute mark after end time
      end_minutes <- ceiling(end_time_decimal * 60)
      next_15min_mark_minutes <- end_minutes + (15 - (end_minutes %% 15))
      if (end_minutes %% 15 == 0) {
        next_15min_mark_minutes = end_minutes + 15
      }
      next_15min_mark_decimal <- next_15min_mark_minutes / 60

      # Generate all 15-minute intervals between the first mark and the end time
      all_15min_marks_minutes <- seq(from = first_15min_mark_minutes, to = end_minutes, by = 15)
      all_15min_marks_decimal <- all_15min_marks_minutes / 60

      # Ensure we don't include any marks that are past the end time
      all_15min_marks_decimal <- all_15min_marks_decimal[all_15min_marks_decimal <= end_time_decimal]

      # Combine previous 15-min mark, start time, all 15-minute marks, end time, and next 15-min mark
      gage_times_decimal <- c(prev_15min_mark_decimal, start_time_decimal, all_15min_marks_decimal)

      # Add end time if it's not already included
      if (!any(abs(gage_times_decimal - end_time_decimal) < 0.0001)) {
        gage_times_decimal <- c(gage_times_decimal, end_time_decimal)
      }

      # Add next 15-min mark if it's not already included
      if (!any(abs(gage_times_decimal - next_15min_mark_decimal) < 0.0001)) {
        gage_times_decimal <- c(gage_times_decimal, next_15min_mark_decimal)
      }

      # Remove any duplicate times
      gage_times_decimal <- unique(gage_times_decimal)

      # Sort times
      gage_times_decimal <- sort(gage_times_decimal)

      # Convert back to military time format
      gage_times_formatted <- sapply(gage_times_decimal, function(t) {
        # If we span midnight, adjust any times that got pushed past 24 hours
        if (spans_midnight && t >= 24) {
          t <- t - 24
        }
        decimal_to_military(t)
      })

      # Store the generated times
      values$gage_times <- gage_times_formatted
      values$gage_times_decimal <- gage_times_decimal

      # Initialize gage heights with empty values
      values$gage_heights <- rep(NA, length(gage_times_formatted))

      # Create notification message based on whether using custom times
      if (using_custom_times) {
        message_text <- paste("Generated", length(gage_times_formatted),
                           "gage height inputs using custom time range:",
                           decimal_to_military(start_time_decimal %% 24), "to",
                           decimal_to_military(end_time_decimal %% 24),
                           "(including 15-min intervals before and after)")
      } else {
        message_text <- paste("Generated", length(gage_times_formatted),
                           "gage height inputs using discharge data time range",
                           "(including 15-min intervals before and after)")
      }

      showNotification(message_text, type = "message")

    }, error = function(e) {
      showNotification(paste("Error generating gage height inputs:", e$message), type = "error")
    })
  })

  # Render gage height input form
  output$gage_height_inputs <- renderUI({
    if(is.null(values$gage_times)) {
      return(h4("Please generate gage height inputs first by clicking the button in the sidebar"))
    }

    gage_inputs <- lapply(1:length(values$gage_times), function(i) {
      time <- values$gage_times[i]
      inputId <- paste0("gage_", i)

      # Format time with a colon for display
      display_time <- paste0(substr(time, 1, nchar(time)-2), ":", substr(time, nchar(time)-1, nchar(time)))

      # Determine if this time point is special
      if (i == 1) {
        # First point is the 15-min before start
        style <- "display: inline-block; margin-right: 20px; margin-bottom: 10px; background-color: #e6f7ff; padding: 5px; border-radius: 5px;"
        display_time <- paste0(display_time, " (15-min before start)")
      } else if (i == 2) {
        # Second point is exact start time
        style <- "display: inline-block; margin-right: 20px; margin-bottom: 10px; background-color: #ffeecc; padding: 5px; border-radius: 5px;"
        display_time <- paste0(display_time, " (Start)")
      } else if (i == length(values$gage_times)) {
        # Last point is 15-min after end
        style <- "display: inline-block; margin-right: 20px; margin-bottom: 10px; background-color: #e6f7ff; padding: 5px; border-radius: 5px;"
        display_time <- paste0(display_time, " (15-min after end)")
      } else if (i == length(values$gage_times) - 1) {
        # Second-to-last point is exact end time
        style <- "display: inline-block; margin-right: 20px; margin-bottom: 10px; background-color: #ffeecc; padding: 5px; border-radius: 5px;"
        display_time <- paste0(display_time, " (End)")
      } else {
        # Regular 15-min interval
        style <- "display: inline-block; margin-right: 20px; margin-bottom: 10px;"
      }

      div(style = style,
          div(strong(display_time)),
          numericInput(inputId, label = NULL, value = values$gage_heights[i], width = "100px")
      )
    })

    # Wrap inputs in a div with flexbox styling
    div(
      style = "display: flex; flex-wrap: wrap; max-width: 100%;",
      div(
        style = "width: 100%; margin-bottom: 15px;",
        p("Enter gage height values for each time point below:", style = "font-weight: bold;"),
        p("The highlighted points include exact start/end times and the 15-minute intervals before start and after end.")
      ),
      gage_inputs
    )
  })

  # Save entered gage heights
  observeEvent(input$save_gage_heights, {
    req(values$gage_times)

    gage_values <- sapply(1:length(values$gage_times), function(i) {
      input[[paste0("gage_", i)]]
    })

    # Check if all gage heights are entered
    if (any(is.na(gage_values))) {
      showNotification("Please enter all gage height values", type = "error")
      return()
    }

    # Save gage heights
    values$gage_heights <- gage_values

    showNotification("Gage heights saved successfully", type = "message")
  })

  # Calculate weighted means
  observeEvent(input$calculate, {
    req(values$time_discharge, values$gage_heights, values$gage_times, values$gage_times_decimal)

    if (any(is.na(values$gage_heights))) {
      showNotification("Please enter and save all gage height values first", type = "error")
      return()
    }

    tryCatch({
      # Process time-discharge data
      time_discharge_data <- values$time_discharge %>%
        mutate(
          # Convert to decimal hours
          Time_decimal = sapply(Time, military_to_decimal)
        ) %>%
        arrange(Time_decimal)

      # Process gage height data
      gage_data <- data.frame(
        Time = values$gage_times,
        Gage_Height = values$gage_heights,
        Time_decimal = values$gage_times_decimal,
        stringsAsFactors = FALSE
      ) %>%
        arrange(Time_decimal)

      # Calculate time differences for weighting gage height values
      n_gage <- nrow(gage_data)
      gage_data$weight <- numeric(n_gage)

      if (n_gage > 1) {
        # Calculate time intervals between consecutive gage height measurements
        for (i in 1:(n_gage-1)) {
          # The weight for each gage height is the time until the next gage height reading
          gage_data$weight[i] <- gage_data$Time_decimal[i+1] - gage_data$Time_decimal[i]
        }

        # The last gage height reading doesn't have a time interval after it
        # So we don't use it in the weighted mean calculation
        gage_data$weight[n_gage] <- 0

        # Calculate weighted mean gage height using only measurements with non-zero weights
        valid_indices <- gage_data$weight > 0
        weighted_mean_gage <- sum(gage_data$Gage_Height[valid_indices] * gage_data$weight[valid_indices]) /
          sum(gage_data$weight[valid_indices])
      } else {
        weighted_mean_gage <- gage_data$Gage_Height[1]
      }

      # For each gage height interval, calculate the discharge volume that occurred
      n_gage <- nrow(gage_data)
      gage_data$volume <- numeric(n_gage)

      if (n_gage > 1) {
        for (i in 1:(n_gage-1)) {
          # Get the start and end times for this interval
          start_time <- gage_data$Time_decimal[i]
          end_time <- gage_data$Time_decimal[i+1]

          # Find discharge measurements that fall within this interval
          # or interpolate/extrapolate as necessary
          interval_discharge <- 0

          # If there are discharge measurements within this interval
          within_interval <- time_discharge_data$Time_decimal >= start_time &
                             time_discharge_data$Time_decimal < end_time

          if (any(within_interval)) {
            # Use the discharge values within this interval
            interval_data <- time_discharge_data[within_interval, ]

            # Calculate time intervals within this segment
            interval_times <- c(start_time, interval_data$Time_decimal, end_time)
            interval_times <- sort(unique(interval_times))

            # For each sub-interval, calculate the volume (discharge × time)
            for (j in 1:(length(interval_times)-1)) {
              sub_start <- interval_times[j]
              sub_end <- interval_times[j+1]
              sub_duration <- sub_end - sub_start

              # Find closest discharge measurement for this sub-interval
              if (j == 1) {
                # For the first sub-interval, use the first measurement in the interval
                # or interpolate between the previous and first measurement
                if (nrow(interval_data) > 0) {
                  sub_discharge <- interval_data$Discharge[1]
                } else {
                  # This shouldn't normally happen given our filters, but just in case
                  sub_discharge <- time_discharge_data$Discharge[1]
                }
              } else {
                # For subsequent sub-intervals, use the previous measurement in the interval
                sub_idx <- which(interval_data$Time_decimal < sub_start)
                if (length(sub_idx) > 0) {
                  sub_discharge <- interval_data$Discharge[max(sub_idx)]
                } else {
                  sub_discharge <- time_discharge_data$Discharge[1]
                }
              }

              # Add volume for this sub-interval
              interval_discharge <- interval_discharge + (sub_discharge * sub_duration)
            }
          } else {
            # If no measurements in this interval, interpolate from surrounding points

            # Find closest points before and after
            before_idx <- which(time_discharge_data$Time_decimal <= start_time)
            after_idx <- which(time_discharge_data$Time_decimal >= end_time)

            if (length(before_idx) > 0 && length(after_idx) > 0) {
              before_time <- time_discharge_data$Time_decimal[max(before_idx)]
              before_discharge <- time_discharge_data$Discharge[max(before_idx)]

              after_time <- time_discharge_data$Time_decimal[min(after_idx)]
              after_discharge <- time_discharge_data$Discharge[min(after_idx)]

              # Linear interpolation for average discharge in this interval
              total_interval <- after_time - before_time
              start_proportion <- (start_time - before_time) / total_interval
              end_proportion <- (end_time - before_time) / total_interval

              start_discharge <- before_discharge + start_proportion * (after_discharge - before_discharge)
              end_discharge <- before_discharge + end_proportion * (after_discharge - before_discharge)

              # Use average of interpolated values
              avg_discharge <- (start_discharge + end_discharge) / 2

              # Calculate volume
              interval_duration <- end_time - start_time
              interval_discharge <- avg_discharge * interval_duration
            } else {
              # If we can't interpolate, use the closest available discharge
              if (length(before_idx) > 0) {
                closest_discharge <- time_discharge_data$Discharge[max(before_idx)]
              } else if (length(after_idx) > 0) {
                closest_discharge <- time_discharge_data$Discharge[min(after_idx)]
              } else {
                closest_discharge <- mean(time_discharge_data$Discharge)
              }

              interval_duration <- end_time - start_time
              interval_discharge <- closest_discharge * interval_duration
            }
          }

          # Store the discharge volume for this interval
          gage_data$volume[i] <- interval_discharge
        }

        # The last gage height doesn't have an interval after it
        gage_data$volume[n_gage] <- 0

        # Calculate volume-weighted mean gage height
        # Only use intervals with positive volume
        valid_indices <- gage_data$volume > 0
        volume_weighted_mean_gage <- sum(gage_data$Gage_Height[valid_indices] * gage_data$volume[valid_indices]) /
                                    sum(gage_data$volume[valid_indices])
      } else {
        volume_weighted_mean_gage <- gage_data$Gage_Height[1]
      }

      # Store results
      values$weighted_mean_gage <- weighted_mean_gage
      values$volume_weighted_mean_gage <- volume_weighted_mean_gage
      values$calculation_details <- list(
        gage = gage_data
      )
      values$volume_calculation_details <- gage_data

      showNotification("Calculations completed successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error in calculations:", e$message), type = "error")
    })
  })

  # Display weighted mean gage height
  output$weighted_mean_gage <- renderText({
    req(values$weighted_mean_gage)
    sprintf("%.2f", values$weighted_mean_gage)
  })

  # Display volume-weighted mean gage height
  output$volume_weighted_mean_gage <- renderText({
    req(values$volume_weighted_mean_gage)
    sprintf("%.2f", values$volume_weighted_mean_gage)
  })

  # Display combined mean gage height
  output$combined_mean_gage <- renderText({
    req(values$weighted_mean_gage, values$volume_weighted_mean_gage)
    combined_mean <- (values$weighted_mean_gage + values$volume_weighted_mean_gage) / 2
    sprintf("%.2f", combined_mean)
  })

  # Display calculation details
  output$calculation_details <- renderPrint({
    req(values$calculation_details)

    cat("=== TIME-WEIGHTED GAGE HEIGHT CALCULATION ===\n")
    gage_details <- values$calculation_details$gage %>%
      select(Time, Gage_Height, Time_decimal, weight) %>%
      mutate(weighted_gage = ifelse(weight > 0, Gage_Height * weight, 0))

    print(gage_details)
    cat("\n")
    cat("Note: Each gage height is weighted by the time interval until the next reading.\n")
    cat("The last gage height has no following interval, so it gets a weight of 0.\n\n")

    valid_indices <- gage_details$weight > 0
    cat(sprintf("Sum of (Gage Height × Weight): %.2f\n", sum(gage_details$weighted_gage[valid_indices])))
    cat(sprintf("Sum of Weights: %.2f\n", sum(gage_details$weight[valid_indices])))
    cat(sprintf("Time-Weighted Mean Gage Height: %.2f\n", values$weighted_mean_gage))
  })

  # Display volume-weighted calculation details
  output$volume_calculation_details <- renderPrint({
    req(values$volume_calculation_details)

    cat("=== VOLUME-WEIGHTED GAGE HEIGHT CALCULATION ===\n")
    cat("Each gage height is weighted by the water volume (discharge × time) that passed through during its period.\n\n")

    volume_details <- values$volume_calculation_details %>%
      select(Time, Gage_Height, Time_decimal, volume) %>%
      mutate(weighted_gage = ifelse(volume > 0, Gage_Height * volume, 0))

    print(volume_details)
    cat("\n")
    cat("Note: Each gage height is weighted by the volume of water passing through its interval.\n")
    cat("The last gage height has no following interval, so it gets a volume of 0.\n\n")

    valid_indices <- volume_details$volume > 0
    cat(sprintf("Sum of (Gage Height × Volume): %.2f\n", sum(volume_details$weighted_gage[valid_indices])))
    cat(sprintf("Sum of Volumes: %.2f\n", sum(volume_details$volume[valid_indices])))
    cat(sprintf("Volume-Weighted Mean Gage Height = %.2f\n", values$volume_weighted_mean_gage))
  })

  # Display combined calculation details
  output$combined_calculation_details <- renderPrint({
    req(values$weighted_mean_gage, values$volume_weighted_mean_gage)

    cat("=== COMBINED CALCULATION ===\n\n")
    cat(sprintf("Time-Weighted Mean Gage Height: %.2f\n", values$weighted_mean_gage))
    cat(sprintf("Volume-Weighted Mean Gage Height: %.2f\n", values$volume_weighted_mean_gage))
    cat(sprintf("Average of Both Methods: %.2f\n\n",
                (values$weighted_mean_gage + values$volume_weighted_mean_gage) / 2))
    cat("Note: This represents the arithmetic mean of both weighting methods.\n")
    cat("Consider which method might be more appropriate for your specific situation.\n")
  })

  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("gage-height-results-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      if (!is.null(values$calculation_details)) {
        # Create results summary
        results <- list(
          Summary = data.frame(
            Metric = c("Time-Weighted Mean Gage Height",
                      "Volume-Weighted Mean Gage Height",
                      "Combined Mean Gage Height"),
            Value = c(values$weighted_mean_gage,
                     values$volume_weighted_mean_gage,
                     (values$weighted_mean_gage + values$volume_weighted_mean_gage) / 2)
          ),
          Gage_Height_Data = values$calculation_details$gage,
          Volume_Weighted_Data = values$volume_calculation_details
        )

        # Write to CSV
        write.csv(results$Summary, file, row.names = FALSE)
        write.csv(results$Gage_Height_Data,
                  paste0(tools::file_path_sans_ext(file), "_time_weighted.csv"),
                  row.names = FALSE)
        write.csv(results$Volume_Weighted_Data,
                  paste0(tools::file_path_sans_ext(file), "_volume_weighted.csv"),
                  row.names = FALSE)

        # Create detailed report
        sink(paste0(tools::file_path_sans_ext(file), "_report.txt"))
        cat("===============================================\n")
        cat("GAGE HEIGHT CALCULATION REPORT\n")
        cat("===============================================\n\n")

        cat("SUMMARY\n")
        cat("-----------------------------------------------\n")
        cat(sprintf("Time-Weighted Mean Gage Height: %.2f\n", values$weighted_mean_gage))
        cat(sprintf("Volume-Weighted Mean Gage Height: %.2f\n", values$volume_weighted_mean_gage))
        cat(sprintf("Combined Mean Gage Height: %.2f\n",
                    (values$weighted_mean_gage + values$volume_weighted_mean_gage) / 2))
        cat("\n")

        cat("TIME-WEIGHTED GAGE HEIGHT CALCULATION\n")
        cat("-----------------------------------------------\n")
        gage_details <- values$calculation_details$gage %>%
          select(Time, Gage_Height, Time_decimal, weight)
        print(gage_details)
        cat("\n")
        cat("Note: Each gage height is weighted by the time interval until the next reading.\n")
        cat("The last gage height has no following interval, so it gets a weight of 0.\n\n")

        cat("VOLUME-WEIGHTED GAGE HEIGHT CALCULATION\n")
        cat("-----------------------------------------------\n")
        volume_details <- values$volume_calculation_details %>%
          select(Time, Gage_Height, Time_decimal, volume)
        print(volume_details)
        cat("\n")
        cat("Note: Each gage height is weighted by the volume of water passing through its interval.\n\n")

        sink()
      } else {
        # Just basic data if no calculations done
        write.csv(data.frame(
          Note = "No calculations performed yet"
        ), file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui = ui, server = server)