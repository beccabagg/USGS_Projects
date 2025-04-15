# Install required packages if not already installed
if (!require("shiny")) {
  install.packages("shiny")
}

if (!require("DT")) {
  install.packages("DT")
}

library(shiny)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Travel Time Calculator"),

  sidebarLayout(
    sidebarPanel(
      textInput("site_name",
                "Site Name:",
                value = ""),

      textInput("computed_by",
                "Computation By:",
                value = ""),

      textInput("measurement_number",
                "Measurement Number:",
                value = ""),

      dateInput("date",
                "Date:",
                value = Sys.Date()),

      hr(),

      numericInput("distance",
                   "Enter Distance:",
                   value = 1,
                   min = 0),

      selectInput("distance_unit",
                  "Distance Unit:",
                  choices = c("Miles", "Feet"),
                  selected = "Miles"),

      numericInput("velocity",
                   "Enter Average Velocity (feet per second):",
                   value = 5,
                   min = 0),

      actionButton("calculate", "Calculate Travel Time",
                   class = "btn-primary"),

      hr(),

      downloadButton("download_complete_csv", "Export Complete Data")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Results",
                 h3("Results"),
                 verbatimTextOutput("time_result"),
                 h4("Calculation details:"),
                 verbatimTextOutput("calculation_details"),
                 hr(),
                 h4("All Data Summary"),
                 DTOutput("data_table")
        ),

        tabPanel("Time Adjustment",
                 h3("Time Adjustment"),
                 div(class = "alert alert-info",
                     h4("Instructions:"),
                     p("1. Copy data directly from Excel (2 columns only):"),
                     p("   • First column: Time in military format (HH:MM)"),
                     p("   • Second column: Section discharge (any units)"),
                     p("2. No headers needed - just copy the data rows"),
                     p("3. Calculate travel time first (in the main tab)")
                 ),

                 tags$div(style = "margin-bottom: 15px;",
                          tags$b("Example of what to copy from Excel:"),
                          tags$pre(
                            "08:15    123.5\n08:30    125.2\n08:45    124.8"
                          )
                 ),

                 tags$textarea(
                   id = "time_data",
                   rows = 10,
                   cols = 50,
                   placeholder = "Paste your Excel data here (2 columns: Time and Discharge)"
                 ),
                 actionButton("process_time", "Process Time Data", class = "btn-info"),
                 hr(),
                 h4("Adjusted Time Results:"),
                 DTOutput("time_table")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Reactive values to store calculation data
  calculation_data <- reactiveVal(data.frame(
    SiteName = character(),
    ComputedBy = character(),
    MeasurementNumber = character(),
    Date = character(),
    Distance = numeric(),
    DistanceUnit = character(),
    DistanceFeet = numeric(),
    Velocity = numeric(),
    AdjustedVelocity = numeric(),
    TravelTimeMinutes = numeric(),
    stringsAsFactors = FALSE
  ))

  # Reactive value to store the current travel time
  travel_time <- reactiveVal(0)

  # Reactive value to store time adjustment data
  time_adjustment_data <- reactiveVal(NULL)

  # Calculate travel time when button is clicked
  observeEvent(input$calculate, {
    # Convert to feet if miles is selected
    distance_feet <- if(input$distance_unit == "Miles") {
      input$distance * 5280
    } else {
      input$distance
    }

    # Calculate travel time in minutes using the formula
    # ((distance in feet / (average velocity * 1.3)) / 60)
    current_travel_time <- ((distance_feet / (input$velocity * 1.3)) / 60)
    travel_time(current_travel_time)

    # Create a new row with the current calculation
    new_row <- data.frame(
      SiteName = input$site_name,
      ComputedBy = input$computed_by,
      MeasurementNumber = input$measurement_number,
      Date = format(input$date, "%Y-%m-%d"),
      Distance = input$distance,
      DistanceUnit = input$distance_unit,
      DistanceFeet = distance_feet,
      Velocity = input$velocity,
      AdjustedVelocity = input$velocity * 1.3,
      TravelTimeMinutes = round(current_travel_time, 2),
      stringsAsFactors = FALSE
    )

    # Update the calculation data
    calculation_data(rbind(calculation_data(), new_row))

    # Display results
    output$time_result <- renderText({
      paste("Travel Time: ", round(current_travel_time, 2), " minutes")
    })

    # Show calculation details
    output$calculation_details <- renderText({
      original_distance <- paste(input$distance, input$distance_unit)

      paste("Site Name:", input$site_name,
            "\nComputation By:", input$computed_by,
            "\nMeasurement Number:", input$measurement_number,
            "\nDate:", format(input$date, "%Y-%m-%d"),
            "\nOriginal Distance:", original_distance,
            "\nDistance (feet):", distance_feet,
            "\nVelocity (feet/second):", input$velocity,
            "\nAdjusted velocity (velocity × 1.3):", input$velocity * 1.3,
            "\nTravel time (minutes):", round(current_travel_time, 2))
    })
  })

  # Process time data when the button is clicked
  observeEvent(input$process_time, {
    # Get the raw text data
    raw_data <- input$time_data

    # Process the raw data
    if (trimws(raw_data) != "") {
      # Split by newlines
      lines <- strsplit(raw_data, "\n")[[1]]

      # Remove any empty lines
      lines <- lines[trimws(lines) != ""]

      # Process data rows
      data_rows <- lapply(lines, function(line) {
        # Split by tabs or multiple spaces
        values <- unlist(strsplit(trimws(line), "\\s+"))
        # Ensure we have exactly 2 columns
        if (length(values) < 2) {
          return(c(values[1], NA))
        } else if (length(values) > 2) {
          return(values[1:2])
        } else {
          return(values)
        }
      })

      # Convert to data frame
      df <- as.data.frame(do.call(rbind, data_rows), stringsAsFactors = FALSE)

      # Set column names
      colnames(df) <- c("Time", "Discharge")

      # Convert discharge to numeric
      df$Discharge <- as.numeric(df$Discharge)

      # Calculate adjusted time
      current_travel_time <- travel_time()

      # Function to add minutes to time
      add_minutes_to_time <- function(time_str, minutes_to_add) {
        # Check for valid time format
        if (!grepl("^\\d{1,2}:\\d{2}$", time_str)) {
          return(NA)
        }

        # Parse the time (assuming 24-hour format HH:MM)
        time_parts <- strsplit(time_str, ":")[[1]]
        hours <- as.numeric(time_parts[1])
        mins <- as.numeric(time_parts[2])

        # Calculate total minutes
        total_mins <- hours * 60 + mins + minutes_to_add

        # Convert back to hours and minutes
        new_hours <- floor(total_mins / 60) %% 24
        new_mins <- round(total_mins %% 60)

        # Format the new time
        sprintf("%02d:%02d", new_hours, new_mins)
      }

      # Apply the time adjustment
      df$AdjustedTime <- sapply(df$Time, function(t) add_minutes_to_time(t, current_travel_time))

      # Store the adjusted data
      time_adjustment_data(df)
    }
  })

  # Render the data table
  output$data_table <- renderDT({
    datatable(calculation_data(), options = list(pageLength = 5))
  })

  # Render the time adjustment table
  output$time_table <- renderDT({
    req(time_adjustment_data())
    datatable(time_adjustment_data(), options = list(pageLength = 10))
  })

  # Download handler for complete CSV export
  output$download_complete_csv <- downloadHandler(
    filename = function() {
      # Get the site name (use "Site" if empty)
      site_name <- if (input$site_name == "") "Site" else input$site_name

      # Sanitize the site name (remove special characters, replace spaces with underscores)
      clean_site_name <- gsub("[^a-zA-Z0-9]", "_", site_name)

      # Get the date in YYYYMMDD format
      date_str <- format(input$date, "%Y%m%d")

      # Construct filename: TOT_YYYYMMDD_SiteName.csv
      paste0("TOT_", date_str, "_", clean_site_name, ".csv")
    },
    content = function(file) {
      # Get the most recent calculation
      calc_data <- calculation_data()
      if (nrow(calc_data) > 0) {
        latest_calc <- calc_data[nrow(calc_data), ]
      } else {
        latest_calc <- data.frame(
          SiteName = NA, ComputedBy = NA, MeasurementNumber = NA,
          Date = NA, TravelTimeMinutes = NA
        )
      }

      # Get the time adjustment data
      time_data <- time_adjustment_data()

      # Create a CSV with both sections

      # Open the file connection
      con <- file(file, "w")

      # Write header section
      cat("TRAVEL TIME CALCULATION SUMMARY\n\n", file = con)
      cat(paste0("Site Name: ", latest_calc$SiteName, "\n"), file = con)
      cat(paste0("Computation By: ", latest_calc$ComputedBy, "\n"), file = con)
      cat(paste0("Measurement Number: ", latest_calc$MeasurementNumber, "\n"), file = con)
      cat(paste0("Date: ", latest_calc$Date, "\n"), file = con)
      cat(paste0("Travel Time (minutes): ", round(latest_calc$TravelTimeMinutes, 2), "\n\n"), file = con)

      # Write calculation details
      cat("CALCULATION DETAILS\n", file = con)
      write.csv(calc_data, con, row.names = FALSE)

      # Write time adjustment data if available
      if (!is.null(time_data)) {
        cat("\n\nTIME ADJUSTMENT TABLE\n", file = con)
        write.csv(time_data, con, row.names = FALSE)
      }

      # Close the connection
      close(con)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)