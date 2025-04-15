# USGS Discharge Data Viewer and Gap Filler

# Check and install required packages if not already installed
required_packages <- c("shiny", "shinydashboard", "dataRetrieval", "dplyr",
                       "ggplot2", "zoo", "lubridate", "imputeTS", "DT", "plotly",
                       "forecast", "parallel")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Enhanced gap-filling function with multiple methods comparison
try_multiple_time_series_prediction <- function(data, date_range_to_fill = NULL,
                                                training_period = NULL, ensure_continuity = TRUE) {
  # Input validation
  if (!all(c("Date", "Discharge") %in% names(data))) {
    stop("Input data must contain 'Date' and 'Discharge' columns")
  }

  # Create mask for values to fill (either existing NAs or specified date range)
  if (!is.null(date_range_to_fill)) {
    # Create mask for specified date range
    date_mask <- data$Date >= date_range_to_fill[1] & data$Date <= date_range_to_fill[2]
  } else {
    # Use existing NAs
    date_mask <- is.na(data$Discharge)
  }

  # If no points to fill, return early
  if (sum(date_mask) == 0) {
    return(list(
      best_method = "none",
      best_rmse = NA,
      best_mae = NA,
      results = list(data),
      msg = "No data points to fill"
    ))
  }

  # Save original data for validation if replacing known values
  original_discharge <- data$Discharge

  # If we're replacing known values, set them to NA for the prediction
  if (!is.null(date_range_to_fill)) {
    data$Discharge[date_mask] <- NA
  }

  # Methods to try (linear removed)
  methods <- c("arima", "ma", "kalman", "spline", "stine", "exponential")

  # Results lists
  all_results <- list()
  metrics <- data.frame(
    method = methods,
    rmse = NA_real_,
    mae = NA_real_,
    bias = NA_real_,
    correlation = NA_real_
  )

  # Try each method
  for (m in methods) {
    # Create a copy for filling
    filled_data <- data

    # Identify specific gaps (consecutive NA sequences)
    rle_result <- rle(is.na(data$Discharge))
    gap_starts <- cumsum(c(1, rle_result$lengths[-length(rle_result$lengths)]))
    gap_ends <- cumsum(rle_result$lengths)

    # Filter to get only the TRUE runs (gaps)
    gap_indices <- which(rle_result$values)
    gap_segments <- data.frame(
      start = gap_starts[gap_indices],
      end = gap_ends[gap_indices]
    )

    # Convert to time series for forecasting methods
    if (!is.null(training_period) && all(training_period > 0)) {
      # If training period is specified, we'll use it for each gap
      lead_pts <- training_period[1]
      lag_pts <- training_period[2]
    } else {
      # Default: use the entire series
      lead_pts <- nrow(data)
      lag_pts <- nrow(data)
    }

    # Process each gap segment separately
    for (i in 1:nrow(gap_segments)) {
      gap_start <- gap_segments$start[i]
      gap_end <- gap_segments$end[i]

      # Define range for this gap (with training data)
      start_idx <- max(1, gap_start - lead_pts)
      end_idx <- min(nrow(data), gap_end + lag_pts)

      # Create mask for this segment
      segment_mask <- rep(FALSE, nrow(data))
      segment_mask[start_idx:end_idx] <- TRUE

      # Get training + gap data for this segment
      segment_data <- data[segment_mask, ]
      segment_ts <- zoo(segment_data$Discharge, order.by = segment_data$Date)

      # Apply the selected method with error handling
      filled_segment <- tryCatch({
        if (m == "arima") {
          imputeTS::na.kalman(segment_ts, model = "auto.arima")
        } else if (m == "ma") {
          imputeTS::na.ma(segment_ts)
        } else if (m == "kalman") {
          imputeTS::na.kalman(segment_ts)
        } else if (m == "spline") {
          imputeTS::na.interpolation(segment_ts, option = "spline")
        } else if (m == "stine") {
          imputeTS::na.interpolation(segment_ts, option = "stine")
        } else if (m == "exponential") {
          tryCatch({
            # Try exponential smoothing with auto-selected parameters
            fit <- forecast::ets(na.omit(segment_ts))
            # Interpolate based on the model
            pred_vals <- fitted(fit)
            # Create a copy and replace NA values
            filled <- segment_ts
            is_na <- is.na(segment_ts)
            coredata(filled)[is_na] <- pred_vals[is_na]
            filled
          }, error = function(e) {
            # Fallback to spline if exponential fails
            imputeTS::na.interpolation(segment_ts, option = "spline")
          })
        } else {
          # Default fallback to spline
          imputeTS::na.interpolation(segment_ts, option = "spline")
        }
      }, error = function(e) {
        warning(paste("Error in", m, "method for segment", i, ":", e$message,
                      "- Falling back to spline interpolation"))
        imputeTS::na.interpolation(segment_ts, option = "spline")
      })

      # Extract only the gap portion to fill in original data
      gap_indices <- which(segment_mask)[which(is.na(segment_data$Discharge))]

      # Fill the gap in the original data frame
      filled_data$Discharge[gap_indices] <- coredata(filled_segment)[is.na(segment_data$Discharge)]

      # Ensure continuity at boundaries if requested
      if (ensure_continuity && length(gap_indices) > 0) {
        # Find boundary points
        before_gap <- max(which(!is.na(data$Discharge) & 1:nrow(data) < gap_indices[1]),
                          na.rm = TRUE, default = NA)
        after_gap <- min(which(!is.na(data$Discharge) & 1:nrow(data) > gap_indices[length(gap_indices)]),
                         na.rm = TRUE, default = NA)

        # Adjust first point if needed
        if (!is.na(before_gap) && !is.na(filled_data$Discharge[gap_indices[1]])) {
          # Linear adjustment to ensure continuity
          filled_data$Discharge[gap_indices[1]] <- data$Discharge[before_gap]
        }

        # Adjust last point if needed
        if (!is.na(after_gap) && !is.na(filled_data$Discharge[gap_indices[length(gap_indices)]])) {
          filled_data$Discharge[gap_indices[length(gap_indices)]] <- data$Discharge[after_gap]
        }
      }
    }

    # Ensure discharge values are non-negative (physical constraint)
    filled_data$Discharge[filled_data$Discharge < 0] <- 0

    # Calculate performance metrics if we have original data to compare against
    if (!is.null(date_range_to_fill)) {
      actual <- original_discharge[date_mask]
      predicted <- filled_data$Discharge[date_mask]

      # Only include non-NA predictions
      valid_mask <- !is.na(predicted)
      if (sum(valid_mask) > 0) {
        metrics$rmse[metrics$method == m] <- sqrt(mean((actual[valid_mask] - predicted[valid_mask])^2))
        metrics$mae[metrics$method == m] <- mean(abs(actual[valid_mask] - predicted[valid_mask]))
        metrics$bias[metrics$method == m] <- mean(predicted[valid_mask] - actual[valid_mask])
        metrics$correlation[metrics$method == m] <- cor(actual[valid_mask], predicted[valid_mask])
      }
    }

    # Store the filled data
    attr(filled_data, "method") <- m
    all_results[[m]] <- filled_data
  }

  # Find the best method based on RMSE (if we have validation data)
  best_method <- "spline" # Default changed from linear to spline
  best_rmse <- NA
  best_mae <- NA

  if (!is.null(date_range_to_fill)) {
    # Sort by RMSE (lower is better)
    metrics <- metrics[order(metrics$rmse), ]
    best_method <- metrics$method[1]
    best_rmse <- metrics$rmse[1]
    best_mae <- metrics$mae[1]
  }

  # Return results
  return(list(
    best_method = best_method,
    best_rmse = best_rmse,
    best_mae = best_mae,
    metrics = metrics,
    results = all_results,
    msg = ifelse(!is.null(date_range_to_fill),
                 paste("Best method:", best_method, "RMSE:", round(best_rmse, 2), "MAE:", round(best_mae, 2)),
                 "No validation data available. Spline method used as default.")
  ))
}

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "USGS Gap Fill Predictor"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Retrieval", tabName = "data", icon = icon("database")),
      menuItem("Model Prediction", tabName = "predict", icon = icon("chart-bar")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),

  dashboardBody(
    tabItems(
      # Data Retrieval Tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Enter USGS Site Information",
                  width = 4,
                  textInput("site_no", "USGS Site Number:", value = "01646500"),
                  dateRangeInput("date_range", "Date Range:",
                                 start = Sys.Date() - 365,
                                 end = Sys.Date()),
                  selectInput("param_code", "Parameter Code:",
                              choices = c("Discharge (cfs)" = "00060",
                                          "Gage height (ft)" = "00065"),
                              selected = "00060"),
                  actionButton("get_data", "Get Data", icon = icon("download"),
                               class = "btn-primary"),
                  hr(),
                  downloadButton("download_data", "Download Data")
                ),

                box(
                  title = "Site Information",
                  width = 8,
                  htmlOutput("site_info"),
                  plotlyOutput("data_plot", height = "300px"),
                  DT::dataTableOutput("data_table")
                )
              )
      ),

      # Model Prediction Tab
      tabItem(tabName = "predict",
              fluidRow(
                box(
                  title = "Erroneous Data Range Selection",
                  width = 4,
                  dateRangeInput("error_date_range", "Date Range to Replace:",
                                 start = Sys.Date() - 180,
                                 end = Sys.Date() - 150),
                  h4("Training Period"),
                  numericInput("predict_lead_points", "Data points before range:", 60, min = 0),
                  numericInput("predict_lag_points", "Data points after range:", 60, min = 0),
                  checkboxInput("predict_ensure_continuity", "Ensure boundary continuity", value = TRUE),
                  actionButton("run_predictions", "Run All Models", icon = icon("play"),
                               class = "btn-success"),
                  hr(),
                  htmlOutput("model_comparison"),
                  downloadButton("download_predicted", "Download Best Model Results")
                ),

                box(
                  title = "Model Prediction Results",
                  width = 8,
                  plotlyOutput("model_plot", height = "400px"),
                  radioButtons("view_model", "View Results For:",
                               choices = c("Best Model", "All Models"),
                               selected = "Best Model",
                               inline = TRUE),
                  DT::dataTableOutput("models_table")
                )
              )
      ),

      # About Tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About This App",
                  width = 12,
                  HTML("<p>This application allows you to retrieve discharge data from the USGS National Water Information System (NWIS)
                 and predict values for specified time periods using multiple time series models.</p>

                 <h4>Key Features:</h4>
                 <ul>
                   <li><strong>Model Prediction:</strong> Replace erroneous data by testing multiple models and selecting the most accurate</li>
                   <li><strong>Automated Comparison:</strong> The app tests multiple models and selects the best one based on accuracy metrics</li>
                 </ul>

                 <h4>Methods available:</h4>
                 <ul>
                   <li><strong>ARIMA:</strong> Auto-regressive integrated moving average model</li>
                   <li><strong>Moving Average:</strong> Simple moving average imputation</li>
                   <li><strong>Kalman Filter:</strong> State space model using Kalman filtering</li>
                   <li><strong>Spline:</strong> Cubic spline interpolation</li>
                   <li><strong>Stine:</strong> Stineman interpolation</li>
                   <li><strong>Exponential Smoothing:</strong> Time series forecasting method</li>
                 </ul>

                 <h4>Usage Tips:</h4>
                 <p><strong>Training Period:</strong> You can specify how many data points before and after each gap to use for model training.</p>
                 <p><strong>Boundary Continuity:</strong> When enabled, ensures that filled gaps connect smoothly with existing data points.</p>
                 <p><strong>Model Prediction:</strong> Select a date range with questionable data, and the app will test multiple models against the actual data to find the best fit.</p>

                 <p>Data is retrieved using the USGS dataRetrieval R package.</p>
                 <p>For more information about USGS water data, visit
                 <a href='https://waterdata.usgs.gov/nwis' target='_blank'>https://waterdata.usgs.gov/nwis</a>.</p>")
                )
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {

  # Reactive values to store data
  values <- reactiveValues(
    raw_data = NULL,
    prediction_results = NULL,
    site_info = NULL
  )

  # Get USGS data when button is clicked
  observeEvent(input$get_data, {
    req(input$site_no, input$date_range, input$param_code)

    # Show a progress notification
    withProgress(message = "Fetching USGS data...", {

      # Get site info
      site_info <- tryCatch({
        readNWISsite(input$site_no)
      }, error = function(e) {
        showNotification(paste("Error retrieving site info:", e$message), type = "error")
        return(NULL)
      })

      # Get discharge data
      usgs_data <- tryCatch({
        readNWISdv(siteNumbers = input$site_no,
                   parameterCd = input$param_code,
                   startDate = input$date_range[1],
                   endDate = input$date_range[2])
      }, error = function(e) {
        showNotification(paste("Error retrieving data:", e$message), type = "error")
        return(NULL)
      })

      if (!is.null(usgs_data) && nrow(usgs_data) > 0) {
        # Process the data
        usgs_data <- renameNWISColumns(usgs_data)

        # Create a simpler data frame with just Date and Discharge
        if (input$param_code == "00060") {
          data <- data.frame(
            Date = usgs_data$Date,
            Discharge = usgs_data$Flow
          )
        } else {
          data <- data.frame(
            Date = usgs_data$Date,
            Discharge = usgs_data$GH
          )
        }

        values$raw_data <- data
        values$site_info <- site_info

        # Show success message
        showNotification(paste("Successfully retrieved", nrow(data), "records."), type = "message")

        # Update the date ranges for error prediction tab
        mid_point <- floor(nrow(data) / 2)
        start_idx <- max(1, mid_point - 15)
        end_idx <- min(nrow(data), mid_point + 15)

        updateDateRangeInput(session, "error_date_range",
                             start = data$Date[start_idx],
                             end = data$Date[end_idx])
      } else {
        showNotification("No data available for the selected criteria.", type = "warning")
      }
    })
  })

  # Run model predictions for erroneous data
  observeEvent(input$run_predictions, {
    req(values$raw_data, input$error_date_range)

    # Show a progress notification
    withProgress(message = "Running models...", {
      incProgress(0.1, detail = "Preparing data...")

      data <- values$raw_data
      date_range <- input$error_date_range

      # Validate the date range
      date_mask <- data$Date >= date_range[1] & data$Date <= date_range[2]

      if (sum(date_mask) == 0) {
        showNotification("No data points in the selected date range.", type = "warning")
        return(NULL)
      }

      # Check if there's enough data for validation
      if (sum(!is.na(data$Discharge[date_mask])) < 3) {
        showNotification("Not enough non-NA data points in the selected range for validation.", type = "warning")
        return(NULL)
      }

      # Determine training period
      training_period <- c(input$predict_lead_points, input$predict_lag_points)

      incProgress(0.2, detail = "Running multiple models...")

      # Run all models and compare
      prediction_results <- tryCatch({
        try_multiple_time_series_prediction(
          data,
          date_range_to_fill = date_range,
          training_period = training_period,
          ensure_continuity = input$predict_ensure_continuity
        )
      }, error = function(e) {
        showNotification(paste("Error in model predictions:", e$message), type = "error")
        return(NULL)
      })

      if (!is.null(prediction_results)) {
        values$prediction_results <- prediction_results
        showNotification(prediction_results$msg, type = "message")
      }
    })
  })

  # Display site information
  output$site_info <- renderUI({
    req(values$site_info)
    site <- values$site_info

    HTML(paste0(
      "<h4>", site$station_nm, "</h4>",
      "<p><strong>Site Number:</strong> ", site$site_no, "<br>",
      "<strong>Location:</strong> ", site$state_nm, ", ", site$county_nm, " County<br>",
      "<strong>Coordinates:</strong> ", round(site$dec_lat_va, 5), ", ", round(site$dec_long_va, 5), "<br>",
      "<strong>Drainage Area:</strong> ", site$drain_area_va, " square miles</p>"
    ))
  })

  # Data plot
  output$data_plot <- renderPlotly({
    req(values$raw_data)
    data <- values$raw_data

    p <- ggplot(data, aes(x = Date, y = Discharge)) +
      geom_line() +
      geom_point(data = subset(data, is.na(Discharge)),
                 aes(x = Date, y = 0), color = "red") +
      labs(title = "USGS Discharge Data",
           y = ifelse(input$param_code == "00060", "Discharge (cfs)", "Gage Height (ft)"),
           x = "Date") +
      theme_minimal()

    ggplotly(p) %>%
      layout(margin = list(l = 50, r = 20, b = 50, t = 50))
  })

  # Data table
  output$data_table <- renderDT({
    req(values$raw_data)
    data <- values$raw_data

    # Calculate gap statistics
    total_records <- nrow(data)
    gaps <- sum(is.na(data$Discharge))
    gap_pct <- round(100 * gaps / total_records, 1)

    # Add a note about gaps
    gap_note <- paste0("Total records: ", total_records,
                       ", Gaps: ", gaps,
                       " (", gap_pct, "%)")

    # Display the first 10 rows of data
    DT::datatable(
      head(data, 10),
      options = list(
        pageLength = 5,
        dom = 'tip',
        scrollX = TRUE
      ),
      caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: left;',
        gap_note
      )
    )
  })

  # Model prediction plot
  output$model_plot <- renderPlotly({
    req(values$raw_data, values$prediction_results)

    # Get data
    raw_data <- values$raw_data
    pred_results <- values$prediction_results
    best_method <- pred_results$best_method

    # Date range mask
    date_range <- input$error_date_range
    date_mask <- raw_data$Date >= date_range[1] & raw_data$Date <= date_range[2]

    # Prepare plot data frame
    if (input$view_model == "Best Model") {
      best_data <- pred_results$results[[best_method]]

      plot_data <- data.frame(
        Date = raw_data$Date,
        Original = raw_data$Discharge,
        Predicted = best_data$Discharge,
        InRange = date_mask
      )

      # Create plot
      p <- ggplot(plot_data, aes(x = Date)) +
        # Original data outside the prediction range
        geom_line(data = subset(plot_data, !InRange),
                  aes(y = Original, color = "Original Data")) +
        # Original data inside the prediction range
        geom_line(data = subset(plot_data, InRange),
                  aes(y = Original, color = "Original (in range)"),
                  linetype = "dotted") +
        # Model prediction
        geom_line(data = subset(plot_data, InRange),
                  aes(y = Predicted, color = "Best Model Prediction"),
                  size = 1) +
        # Highlight boundary points
        geom_point(data = subset(plot_data, InRange)[c(1, sum(date_mask)), ],
                   aes(y = Original, color = "Range Boundary"),
                   size = 3, shape = 21, fill = "white") +
        scale_color_manual(values = c("Original Data" = "black",
                                      "Original (in range)" = "gray",
                                      "Best Model Prediction" = "red",
                                      "Range Boundary" = "blue")) +
        labs(title = paste("Model Prediction Results -", best_method, "Method (Best)"),
             subtitle = paste("RMSE:", round(pred_results$best_rmse, 2),
                              "MAE:", round(pred_results$best_mae, 2)),
             y = ifelse(input$param_code == "00060", "Discharge (cfs)", "Gage Height (ft)"),
             x = "Date",
             color = "Data Type") +
        theme_minimal()

    } else {
      # Initialize an empty data frame
      all_models_data <- data.frame(
        Date = rep(raw_data$Date[date_mask], length(pred_results$results)),
        Method = rep(names(pred_results$results), each = sum(date_mask)),
        Original = rep(raw_data$Discharge[date_mask], length(pred_results$results)),
        Predicted = numeric(sum(date_mask) * length(pred_results$results))
      )

      # Fill in predictions for each method
      row_index <- 1
      for (method in names(pred_results$results)) {
        method_data <- pred_results$results[[method]]
        all_models_data$Predicted[all_models_data$Method == method] <-
          method_data$Discharge[date_mask]
        row_index <- row_index + sum(date_mask)
      }

      # Get RMSE for each method for the legend
      metrics <- pred_results$metrics
      methods_with_stats <- paste0(metrics$method, " (RMSE: ", round(metrics$rmse, 2), ")")
      names(methods_with_stats) <- metrics$method

      # Create plot
      p <- ggplot(all_models_data, aes(x = Date, y = Predicted, color = Method, group = Method)) +
        geom_line() +
        # Original data
        geom_line(aes(y = Original, color = "Original"), linetype = "dashed", size = 1) +
        # Highlight best method
        geom_line(data = subset(all_models_data, Method == best_method),
                  aes(y = Predicted), size = 1.5) +
        labs(title = "All Model Predictions Comparison",
             subtitle = paste("Best Method:", best_method, "- RMSE:",
                              round(pred_results$best_rmse, 2)),
             y = ifelse(input$param_code == "00060", "Discharge (cfs)", "Gage Height (ft)"),
             x = "Date") +
        theme_minimal() +
        theme(legend.position = "bottom", legend.box = "vertical")
    }

    ggplotly(p) %>%
      layout(margin = list(l = 50, r = 20, b = 50, t = 50))
  })

  # Models comparison table
  output$models_table <- renderDT({
    req(values$prediction_results)

    metrics <- values$prediction_results$metrics

    # Format the metrics for display
    display_metrics <- metrics[order(metrics$rmse), ]
    display_metrics$rmse <- round(display_metrics$rmse, 3)
    display_metrics$mae <- round(display_metrics$mae, 3)
    display_metrics$bias <- round(display_metrics$bias, 3)
    display_metrics$correlation <- round(display_metrics$correlation, 3)

    # Add ranking
    display_metrics$rank <- 1:nrow(display_metrics)

    # Reorder columns
    display_metrics <- display_metrics[, c("rank", "method", "rmse", "mae", "bias", "correlation")]

    # Rename columns for display
    colnames(display_metrics) <- c("Rank", "Method", "RMSE", "MAE", "Bias", "Correlation")

    # Display table
    DT::datatable(
      display_metrics,
      options = list(
        pageLength = 7,
        dom = 'tip',
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      # Highlight the best method
      formatStyle(
        'Rank',
        target = 'row',
        backgroundColor = styleEqual(1, '#e6f7e6')
      )
  })

  # Model comparison statistics
  output$model_comparison <- renderUI({
    req(values$prediction_results)

    results <- values$prediction_results

    # Create an HTML table of the top 3 methods
    top_methods <- head(results$metrics[order(results$metrics$rmse), ], 3)

    table_rows <- ""
    for (i in 1:nrow(top_methods)) {
      table_rows <- paste0(table_rows,
                           "<tr>",
                           "<td>", i, "</td>",
                           "<td>", top_methods$method[i], "</td>",
                           "<td>", round(top_methods$rmse[i], 3), "</td>",
                           "<td>", round(top_methods$mae[i], 3), "</td>",
                           "</tr>")
    }

    # Format the date range
    date_range <- format(input$error_date_range, "%b %d, %Y")

    HTML(paste0(
      "<h4>Model Prediction Results</h4>",
      "<p><strong>Date Range:</strong> ", date_range[1], " to ", date_range[2], "<br>",
      "<strong>Data Points:</strong> ", sum(values$raw_data$Date >= input$error_date_range[1] &
                                              values$raw_data$Date <= input$error_date_range[2]), "<br>",
      "<strong>Best Method:</strong> ", results$best_method, "<br>",
      "<strong>Boundary Continuity:</strong> ", ifelse(input$predict_ensure_continuity, "Ensured", "Not enforced"), "<br>",
      "<strong>Training Period:</strong> ", input$predict_lead_points, " points before, ",
      input$predict_lag_points, " points after</p>",

      "<h5>Top 3 Methods:</h5>",
      "<table class='table table-striped table-sm'>",
      "<thead><tr><th>Rank</th><th>Method</th><th>RMSE</th><th>MAE</th></tr></thead>",
      "<tbody>",
      table_rows,
      "</tbody></table>"
    ))
  })

  # Download raw data
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("USGS_", input$site_no, "_raw_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(values$raw_data)
      write.csv(values$raw_data, file, row.names = FALSE)
    }
  )

  # Download prediction results
  output$download_predicted <- downloadHandler(
    filename = function() {
      paste0("USGS_", input$site_no, "_model_prediction_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(values$prediction_results)

      results <- values$prediction_results
      best_method <- results$best_method
      best_data <- results$results[[best_method]]

      # Create metadata
      meta <- data.frame(
        key = c(
          "Site Number",
          "Date Range",
          "Best Method",
          "RMSE",
          "MAE",
          "Boundary Continuity",
          "Training Period",
          "Created"
        ),
        value = c(
          input$site_no,
          paste(format(input$error_date_range, "%Y-%m-%d"), collapse = " to "),
          best_method,
          round(results$best_rmse, 3),
          round(results$best_mae, 3),
          ifelse(input$predict_ensure_continuity, "Ensured", "Not enforced"),
          paste(input$predict_lead_points, "points before,", input$predict_lag_points, "points after"),
          as.character(Sys.time())
        )
      )

      # Add model comparison data
      model_comparison <- results$metrics

      # Write files
      write.csv(meta, file, row.names = FALSE)
      write.table("", file, append = TRUE, row.names = FALSE, col.names = FALSE)
      write.table("MODEL COMPARISON:", file, append = TRUE, row.names = FALSE, col.names = FALSE)
      write.table(model_comparison, file, sep = ",", row.names = FALSE, append = TRUE)
      write.table("", file, append = TRUE, row.names = FALSE, col.names = FALSE)
      write.table("BEST MODEL RESULTS:", file, append = TRUE, row.names = FALSE, col.names = FALSE)
      write.table(best_data, file, sep = ",", col.names = TRUE,
                  row.names = FALSE, append = TRUE)
    }
  )
}

# Run the app
shinyApp(ui, server)