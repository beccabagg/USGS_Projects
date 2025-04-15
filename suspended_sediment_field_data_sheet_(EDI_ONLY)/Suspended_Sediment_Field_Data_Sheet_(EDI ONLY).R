# Suspended Sediment Field Data Sheet(EDI ONLY)
# Created: 2025-04-11
# Author: beccabagg
# Updated: 2025-04-14 15:47:52

# Load required libraries
library(shiny)
library(xml2)
library(DT)
library(shinythemes)
library(openxlsx)  # For Excel export

# Function to extract data from USGS XML
extract_usgs_data <- function(xml_file) {
  # Read the XML file as plain text
  xml_text <- readLines(xml_file)
  xml_text <- paste(xml_text, collapse = "\n")

  # Remove namespace declarations to simplify parsing
  xml_text <- gsub('xmlns="http://water.usgs.gov/XML/AQ"', '', xml_text)
  xml_text <- gsub('xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"', '', xml_text)

  # Parse the modified XML
  xml_data <- read_xml(xml_text)

  # Find all MidSectionStation elements
  stations <- xml_find_all(xml_data, "//MidSectionStation")

  # Check if any stations were found
  if (length(stations) == 0) {
    return(NULL)
  }

  # Create vectors to store the extracted data
  locations <- numeric(length(stations))
  discharges <- numeric(length(stations))

  # Loop through each station and extract data
  for (i in seq_along(stations)) {
    station <- stations[i]

    # Extract location
    location_node <- xml_find_first(station, ".//DistanceFromInitialPointMeasure")
    locations[i] <- if (!is.na(location_node)) as.numeric(xml_text(location_node)) else NA

    # Extract discharge
    discharge_node <- xml_find_first(station, ".//DischargeMeasure")
    discharges[i] <- if (!is.na(discharge_node)) as.numeric(xml_text(discharge_node)) else NA
  }

  # Create initial data frame
  data <- data.frame(
    location = locations,
    discharge = discharges,
    stringsAsFactors = FALSE
  )

  # Sort data by section location
  data <- data[order(data$location),]

  # Calculate cumulative discharge and percentage
  total_discharge_sum <- sum(data$discharge, na.rm = TRUE)
  data$cumulative_discharge <- cumsum(data$discharge)
  data$cumulative_percentage <- (data$cumulative_discharge / total_discharge_sum) * 100

  # Rename columns to final display names
  names(data) <- c("Section Location (ft)", "Discharge (cfs)",
                   "Cumulative Discharge (cfs)", "Cumulative Percentage (%)")

  # Extract site information without namespaces
  site_number <- tryCatch({
    node <- xml_find_first(xml_data, "//SiteIdentifier")
    xml_text(node)
  }, error = function(e) "Unknown")

  site_name <- tryCatch({
    node <- xml_find_first(xml_data, "//Site/Name")
    xml_text(node)
  }, error = function(e) "Unknown")

  discharge_date <- tryCatch({
    node <- xml_find_first(xml_data, "//DischargeMeasurement/DischargeDateTime")
    text <- xml_text(node)
    if(grepl("T", text)) {
      # Format ISO8601 datetime
      format(as.POSIXct(text, format="%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d %H:%M:%S")
    } else {
      text
    }
  }, error = function(e) "Unknown")

  total_discharge <- tryCatch({
    node <- xml_find_first(xml_data, "//DischargeMeasurement/DischargeMeasure")
    xml_text(node)
  }, error = function(e) "Unknown")

  # Add site info as attributes
  attr(data, "site_info") <- list(
    site_number = site_number,
    site_name = site_name,
    measurement_date = discharge_date,
    total_discharge = total_discharge
  )

  return(data)
}

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),

  # App title
  titlePanel("Suspended Sediment Field Data Sheet(EDI ONLY)"),

  # Sidebar layout
  sidebarLayout(
    # Sidebar panel
    sidebarPanel(
      # File input
      fileInput("xml_file", "Upload USGS XML File",
                multiple = FALSE,
                accept = c("text/xml", ".xml")),

      tags$hr(),

      # Instructions
      tags$p("Upload a USGS Hydro XML file to extract section location and discharge values."),
      tags$p("The table includes cumulative discharge percentage across the stream section."),

      tags$hr(),

      # Measurement information section
      tags$h4("Measurement Information"),
      textInput("station_number", "Station Number", placeholder = "e.g., 12345678"),
      dateInput("measurement_date", "Date", value = Sys.Date()),
      textInput("party", "Party", placeholder = "Names of field personnel"),
      numericInput("water_temp", "Water Temperature (°C)", value = NA, min = 0, max = 40, step = 0.1),

      # Time inputs with HH:MM format
      tags$div(
        style = "display: flex; gap: 10px;",
        textInput("start_time", "Start Time (HH:MM)", placeholder = "e.g., 14:30"),
        textInput("end_time", "End Time (HH:MM)", placeholder = "e.g., 16:45")
      ),

      # Average time (calculated) - this will be display-only
      tags$div(
        style = "margin-top: 10px;",
        tags$label("Average Time (HH:MM)"),
        tags$div(
          style = "padding: 6px; border: 1px solid #ccc; border-radius: 4px; background: #f9f9f9;",
          textOutput("avg_time")
        )
      ),

      tags$hr(),

      # Conditional panel for site info and download
      conditionalPanel(
        condition = "output.data_loaded",

        tags$h4("Download Options"),

        # Download buttons
        tags$div(
          style = "display: flex; gap: 10px; flex-wrap: wrap;",
          downloadButton("download_csv", "Download CSV"),
          downloadButton("download_excel", "Download Excel")
        ),

        tags$hr(),

        # Site information
        htmlOutput("site_info")
      ),

      width = 3
    ),

    # Main panel
    mainPanel(
      # Tabs for different tables
      tabsetPanel(
        tabPanel("Discharge Data",
                 DT::dataTableOutput("data_table"),
                 tags$hr(),

                 # Explanation of columns
                 tags$div(
                   tags$h4("Column Descriptions:"),
                   tags$ul(
                     tags$li(tags$strong("Section Location (ft)"), " - Distance from initial measurement point (usually bank)"),
                     tags$li(tags$strong("Discharge (cfs)"), " - Discharge at each individual section"),
                     tags$li(tags$strong("Cumulative Discharge (cfs)"), " - Sum of discharge up to this section"),
                     tags$li(tags$strong("Cumulative Percentage (%)"), " - Percentage of total discharge accumulated up to this section")
                   )
                 )
        ),
        tabPanel("Sampling Bottle Locations",
                 tags$h4("Sampling Bottle Locations"),
                 tags$p("This table shows the locations where each sampling bottle (1-5) should be used, corresponding to specific cumulative discharge thresholds."),
                 DT::dataTableOutput("threshold_table"),
                 tags$hr(),
                 tags$div(
                   tags$p("Use these values to identify where water samples should be collected across the stream cross-section.")
                 )
        ),
        tabPanel("Measurement Details",
                 tags$h4("Measurement Summary"),
                 tags$div(
                   style = "margin-top: 20px;",
                   htmlOutput("measurement_summary")
                 )
        )
      ),
      width = 9
    )
  ),

  # Footer
  tags$div(
    style = "text-align: center; margin-top: 20px; color: #777; font-size: 0.9em;",
    tags$p(paste("Created:", format(Sys.Date(), "%Y-%m-%d"))),
    tags$p(paste("Last updated:", "2025-04-14 15:47:52")),
    tags$p(paste("User:", "beccabagg"))
  )
)

# Define server
server <- function(input, output, session) {

  # Process XML file
  data <- reactive({
    # Require file input
    req(input$xml_file)

    # Validate file extension
    ext <- tools::file_ext(input$xml_file$name)
    if (ext != "xml") {
      showNotification("Please upload an XML file", type = "error")
      return(NULL)
    }

    # Show processing message
    showNotification("Processing XML file...", type = "message", duration = NULL, id = "processing")

    # Extract data with error handling
    result <- tryCatch({
      data <- extract_usgs_data(input$xml_file$datapath)

      if (is.null(data) || nrow(data) == 0) {
        showNotification("No discharge data found in the XML file", type = "warning")
        return(NULL)
      }

      data
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      NULL
    }, finally = {
      # Remove processing message
      removeNotification("processing")
    })

    return(result)
  })

  # Calculate threshold locations
  threshold_data <- reactive({
    req(data())

    # Get data frame
    df <- data()

    # Define target percentages
    target_percentages <- c(10, 20, 50, 70, 90)
    bottle_numbers <- c(1, 2, 3, 4, 5)

    # Create result data frame
    result <- data.frame(
      Bottle = paste("Bottle", bottle_numbers),
      Percentage = paste0(target_percentages, "%"),
      Location = numeric(length(target_percentages)),
      stringsAsFactors = FALSE
    )

    # For each target percentage, find the corresponding location
    for (i in seq_along(target_percentages)) {
      target <- target_percentages[i]

      # Get percentage column
      perc_col <- df[, "Cumulative Percentage (%)"]
      loc_col <- df[, "Section Location (ft)"]

      # Find the closest points above and below the target percentage
      if (any(perc_col >= target)) {
        # If we have data points at or above the target
        if (any(perc_col <= target)) {
          # If we have data points both above and below, interpolate
          idx_below <- max(which(perc_col <= target))
          idx_above <- min(which(perc_col >= target))

          # If they're the same point, use that exact point
          if (idx_below == idx_above) {
            result$Location[i] <- loc_col[idx_below]
          } else {
            # Otherwise, interpolate between the two points
            perc_below <- perc_col[idx_below]
            perc_above <- perc_col[idx_above]
            loc_below <- loc_col[idx_below]
            loc_above <- loc_col[idx_above]

            # Linear interpolation
            result$Location[i] <- loc_below + (target - perc_below) * (loc_above - loc_below) / (perc_above - perc_below)
          }
        } else {
          # If all points are above target, use the first point
          result$Location[i] <- loc_col[1]
        }
      } else {
        # If all points are below target, use the last point
        result$Location[i] <- loc_col[length(loc_col)]
      }
    }

    # Rename columns for display
    names(result) <- c("Sampling Bottle", "Cumulative Discharge", "Location (ft)")

    return(result)
  })

  # Calculate average time
  avg_time <- reactive({
    # Get start and end times
    start <- input$start_time
    end <- input$end_time

    # Check if both inputs are provided and in correct format
    if (is.null(start) || start == "" || is.null(end) || end == "") {
      return("--:--")
    }

    # Validate time format (HH:MM)
    if (!grepl("^([0-1]?[0-9]|2[0-3]):[0-5][0-9]$", start) ||
        !grepl("^([0-1]?[0-9]|2[0-3]):[0-5][0-9]$", end)) {
      return("Invalid format")
    }

    # Parse times
    start_parts <- as.numeric(strsplit(start, ":")[[1]])
    end_parts <- as.numeric(strsplit(end, ":")[[1]])

    # Convert to minutes since midnight
    start_mins <- start_parts[1] * 60 + start_parts[2]
    end_mins <- end_parts[1] * 60 + end_parts[2]

    # Handle case where measurement spans midnight
    if (end_mins < start_mins) {
      end_mins <- end_mins + 24 * 60
    }

    # Calculate average minutes
    avg_mins <- (start_mins + end_mins) / 2

    # Ensure we stay within 24-hour range
    avg_mins <- avg_mins %% (24 * 60)

    # Convert back to HH:MM
    avg_hours <- floor(avg_mins / 60)
    avg_minutes <- round(avg_mins %% 60)

    # Format as HH:MM
    sprintf("%02d:%02d", avg_hours, avg_minutes)
  })

  # Output for average time
  output$avg_time <- renderText({
    avg_time()
  })

  # Generate measurement summary
  output$measurement_summary <- renderUI({
    # Get site info if available
    site_info <- if (!is.null(data())) attr(data(), "site_info") else NULL

    # Get user inputs
    station <- input$station_number
    date <- format(input$measurement_date, "%Y-%m-%d")
    party <- input$party
    water_temp <- input$water_temp
    start <- input$start_time
    end <- input$end_time

    # If site info is available, use some values from there
    if (!is.null(site_info)) {
      if (station == "") {
        station <- site_info$site_number
      }
      site_name <- site_info$site_name
    } else {
      site_name <- "Not available"
    }

    # Format water temperature with unit
    water_temp_text <- if (is.na(water_temp)) "Not provided" else paste0(water_temp, " °C")

    # Create HTML for summary
    HTML(paste0(
      "<div class='panel panel-default'>",
      "  <div class='panel-heading'><strong>Measurement Details</strong></div>",
      "  <div class='panel-body'>",
      "    <table class='table table-striped'>",
      "      <tr><td>Station Number:</td><td>", station, "</td></tr>",
      "      <tr><td>Station Name:</td><td>", site_name, "</td></tr>",
      "      <tr><td>Measurement Date:</td><td>", date, "</td></tr>",
      "      <tr><td>Field Party:</td><td>", party, "</td></tr>",
      "      <tr><td>Water Temperature:</td><td>", water_temp_text, "</td></tr>",
      "      <tr><td>Start Time:</td><td>", start, "</td></tr>",
      "      <tr><td>End Time:</td><td>", end, "</td></tr>",
      "      <tr><td>Average Time:</td><td>", avg_time(), "</td></tr>",
      if (!is.null(site_info)) paste0(
        "      <tr><td>Total Discharge:</td><td>", site_info$total_discharge, " cfs</td></tr>"
      ) else "",
      "    </table>",
      "  </div>",
      "</div>"
    ))
  })

  # Data loaded flag
  output$data_loaded <- reactive({
    !is.null(data()) && nrow(data()) > 0
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  # Site information
  output$site_info <- renderUI({
    req(data())

    site_info <- attr(data(), "site_info")

    HTML(paste0(
      "<h4>Site Information</h4>",
      "<p><strong>Site Number:</strong> ", site_info$site_number, "</p>",
      "<p><strong>Site Name:</strong> ", site_info$site_name, "</p>",
      "<p><strong>Date:</strong> ", site_info$measurement_date, "</p>",
      "<p><strong>Total Discharge:</strong> ", site_info$total_discharge, " cfs</p>"
    ))
  })

  # Data table
  output$data_table <- DT::renderDataTable({
    req(data())

    # Get column names from the actual data
    data_columns <- colnames(data())

    DT::datatable(
      data(),
      options = list(
        pageLength = 50,
        dom = 'ftip',
        ordering = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = data_columns[1:3], digits = 3) %>%
      formatRound(columns = data_columns[4], digits = 1)
  })

  # Threshold table
  output$threshold_table <- DT::renderDataTable({
    req(threshold_data())

    DT::datatable(
      threshold_data(),
      options = list(
        pageLength = 10,
        dom = 'ft',
        ordering = FALSE
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = "Location (ft)", digits = 3)
  })

  # Download CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      # Use station number from input if available, otherwise from file
      site_info <- attr(data(), "site_info")
      station <- if (input$station_number != "") input$station_number else site_info$site_number
      site_id <- gsub("[^0-9]", "", station)
      paste0("USGS_", site_id, "_Discharge_", format(input$measurement_date, "%Y%m%d"), ".csv")
    },
    content = function(file) {
      # Get data and site info
      df <- data()
      site_info <- attr(df, "site_info")

      # Create header lines with user-entered information
      header <- c(
        paste("# Station Number:", if (input$station_number != "") input$station_number else site_info$site_number),
        paste("# Station Name:", site_info$site_name),
        paste("# Measurement Date:", format(input$measurement_date, "%Y-%m-%d")),
        paste("# Field Party:", input$party),
        paste("# Water Temperature (°C):", if (!is.na(input$water_temp)) input$water_temp else "Not provided"),
        paste("# Start Time:", input$start_time),
        paste("# End Time:", input$end_time),
        paste("# Average Time:", avg_time()),
        paste("# Total Discharge (cfs):", site_info$total_discharge),
        paste("# Generated:", "2025-04-14 15:47:52"),
        paste("# User:", "beccabagg"),
        "#"
      )

      # Write header to file
      writeLines(header, file)

      # Append data
      write.table(df, file, append = TRUE, sep = ",",
                  row.names = FALSE, col.names = TRUE, quote = TRUE)

      # Add threshold data
      writeLines("\n# Sampling Bottle Locations", file, append = TRUE)
      write.table(threshold_data(), file, append = TRUE, sep = ",",
                  row.names = FALSE, col.names = TRUE, quote = TRUE)
    }
  )

  # SUPER MINIMAL Excel download to avoid any styling errors
  output$download_excel <- downloadHandler(
    filename = function() {
      # Use station number from input if available, otherwise from file
      site_info <- attr(data(), "site_info")
      station <- if (input$station_number != "") input$station_number else site_info$site_number
      site_id <- gsub("[^0-9]", "", station)
      paste0(site_id, "_EDI_", format(input$measurement_date, "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      # Ensure we have data
      req(data())
      req(threshold_data())

      df <- data()
      threshold_df <- threshold_data()
      site_info <- attr(df, "site_info")

      # Create a completely unstyled workbook
      wb <- createWorkbook()

      # Add worksheet
      addWorksheet(wb, "Sediment Data")

      # Setup worksheet for printing
      pageSetup(wb, "Sediment Data", orientation = "landscape", fitToWidth = TRUE, fitToHeight = TRUE)

      # Set column widths
      setColWidths(wb, "Sediment Data", cols = 1:3, widths = c(30, 20, 15))

      # Row counter
      currentRow <- 1

      # Title (NO styling - just plain text)
      writeData(wb, "Sediment Data", "SUSPENDED SEDIMENT FIELD DATA SHEET", startRow = currentRow, startCol = 1)
      mergeCells(wb, "Sediment Data", rows = currentRow, cols = 1:3)
      currentRow <- currentRow + 1

      # Timestamp and user
      writeData(wb, "Sediment Data", paste("Generated:", "2025-04-14 15:47:52"), startRow = currentRow, startCol = 1)
      writeData(wb, "Sediment Data", paste("User:", "beccabagg"), startRow = currentRow, startCol = 2)
      currentRow <- currentRow + 2

      # Station information section
      writeData(wb, "Sediment Data", "STATION INFORMATION", startRow = currentRow, startCol = 1)
      mergeCells(wb, "Sediment Data", rows = currentRow, cols = 1:3)
      currentRow <- currentRow + 1

      # Station data without any styling
      writeData(wb, "Sediment Data", "Station Number:", startRow = currentRow, startCol = 1)
      writeData(wb, "Sediment Data", if (input$station_number != "") input$station_number else site_info$site_number,
                startRow = currentRow, startCol = 2)
      currentRow <- currentRow + 1

      writeData(wb, "Sediment Data", "Station Name:", startRow = currentRow, startCol = 1)
      writeData(wb, "Sediment Data", site_info$site_name, startRow = currentRow, startCol = 2)
      currentRow <- currentRow + 1

      writeData(wb, "Sediment Data", "Date:", startRow = currentRow, startCol = 1)
      writeData(wb, "Sediment Data", format(input$measurement_date, "%Y-%m-%d"), startRow = currentRow, startCol = 2)
      currentRow <- currentRow + 1

      writeData(wb, "Sediment Data", "Field Party:", startRow = currentRow, startCol = 1)
      writeData(wb, "Sediment Data", input$party, startRow = currentRow, startCol = 2)
      currentRow <- currentRow + 1

      writeData(wb, "Sediment Data", "Water Temperature (°C):", startRow = currentRow, startCol = 1)
      writeData(wb, "Sediment Data", if (is.na(input$water_temp)) "Not provided" else paste0(input$water_temp, " °C"),
                startRow = currentRow, startCol = 2)
      currentRow <- currentRow + 1

      # Time information
      writeData(wb, "Sediment Data", "Start Time:", startRow = currentRow, startCol = 1)
      writeData(wb, "Sediment Data", input$start_time, startRow = currentRow, startCol = 2)
      currentRow <- currentRow + 1

      writeData(wb, "Sediment Data", "End Time:", startRow = currentRow, startCol = 1)
      writeData(wb, "Sediment Data", input$end_time, startRow = currentRow, startCol = 2)
      currentRow <- currentRow + 1

      writeData(wb, "Sediment Data", "Average Time:", startRow = currentRow, startCol = 1)
      writeData(wb, "Sediment Data", avg_time(), startRow = currentRow, startCol = 2)
      currentRow <- currentRow + 1

      writeData(wb, "Sediment Data", "Total Discharge:", startRow = currentRow, startCol = 1)
      writeData(wb, "Sediment Data", paste0(site_info$total_discharge, " cfs"), startRow = currentRow, startCol = 2)
      currentRow <- currentRow + 2

      # Bottle location section (completely plain, no styling)
      writeData(wb, "Sediment Data", "SAMPLING BOTTLE LOCATIONS", startRow = currentRow, startCol = 1)
      mergeCells(wb, "Sediment Data", rows = currentRow, cols = 1:3)
      currentRow <- currentRow + 1

      # Bottle headers
      writeData(wb, "Sediment Data", "Sampling Bottle", startRow = currentRow, startCol = 1)
      writeData(wb, "Sediment Data", "Cumulative Discharge", startRow = currentRow, startCol = 2)
      writeData(wb, "Sediment Data", "Location (ft)", startRow = currentRow, startCol = 3)
      currentRow <- currentRow + 1

      # Bottle data with NO styling
      for(i in 1:nrow(threshold_df)) {
        writeData(wb, "Sediment Data", threshold_df$`Sampling Bottle`[i], startRow = currentRow, startCol = 1)
        writeData(wb, "Sediment Data", threshold_df$`Cumulative Discharge`[i], startRow = currentRow, startCol = 2)
        writeData(wb, "Sediment Data", round(threshold_df$`Location (ft)`[i], 3), startRow = currentRow, startCol = 3)
        currentRow <- currentRow + 1
      }

      currentRow <- currentRow + 1

      # Discharge data section (no styling)
      writeData(wb, "Sediment Data", "DISCHARGE DATA SUMMARY", startRow = currentRow, startCol = 1)
      mergeCells(wb, "Sediment Data", rows = currentRow, cols = 1:3)
      currentRow <- currentRow + 1

      # Summary stats
      min_loc <- min(df[, "Section Location (ft)"])
      max_loc <- max(df[, "Section Location (ft)"])
      stream_width <- max_loc - min_loc
      avg_discharge <- mean(df[, "Discharge (cfs)"])
      max_discharge <- max(df[, "Discharge (cfs)"])

      # Summary metrics with NO styling
      metrics <- c("Stream Width (ft)", "Minimum Location (ft)", "Maximum Location (ft)",
                   "Average Section Discharge (cfs)", "Maximum Section Discharge (cfs)",
                   "Total Discharge (cfs)")
      values <- c(round(stream_width, 2), round(min_loc, 2), round(max_loc, 2),
                  round(avg_discharge, 2), round(max_discharge, 2), site_info$total_discharge)

      # Write summary data with NO styling
      for(i in 1:length(metrics)) {
        writeData(wb, "Sediment Data", metrics[i], startRow = currentRow, startCol = 1)
        writeData(wb, "Sediment Data", values[i], startRow = currentRow, startCol = 2)
        currentRow <- currentRow + 1
      }

      currentRow <- currentRow + 1

      # Field notes section
      writeData(wb, "Sediment Data", "FIELD NOTES", startRow = currentRow, startCol = 1)
      mergeCells(wb, "Sediment Data", rows = currentRow, cols = 1:3)
      currentRow <- currentRow + 1

      # Empty note cells
      for(i in 1:3) {
        writeData(wb, "Sediment Data", "", startRow = currentRow, startCol = 1)
        mergeCells(wb, "Sediment Data", rows = currentRow, cols = 1:3)
        currentRow <- currentRow + 1
      }

      # Add another sheet with the raw discharge data
      addWorksheet(wb, "Raw Data")
      writeData(wb, "Raw Data", df, startRow = 1, startCol = 1)

      # Add another sheet with the bottle location data
      addWorksheet(wb, "Bottle Locations")
      writeData(wb, "Bottle Locations", threshold_df, startRow = 1, startCol = 1)

      # Save the workbook with NO styling at all
      saveWorkbook(wb, file, overwrite = TRUE)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
}

# Run the application
shinyApp(ui = ui, server = server)