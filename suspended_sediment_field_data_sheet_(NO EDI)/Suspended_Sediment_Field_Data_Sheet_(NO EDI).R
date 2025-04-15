# Check and install required packages
required_packages <- c("shiny", "shinythemes", "DT", "openxlsx", "shinyjs")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Corrected sampler types list
sampler_types <- c(
  "DH-48", "DH-59", "DH-81", "DH-95",
  "D-49", "D-74", "D-74AL", "P-61",
  "P-72", "D-95", "D-96", "Other"
)

# Stage options
stage_options <- c("Steady", "Rising", "Falling", "Peak")

# Sampling methods
sampling_methods <- c(
  "EWI Iso", "EWI Non-Iso", "Single Vertical",
  "Multi-Vertical", "Point", "Grab", "ISCO", "Box Single"
)

# User Guide
user_guide_html <- HTML('
<div style="background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px; border-left: 5px solid #4682B4;">
  <h4 style="color: #4682B4;"><i class="fa fa-info-circle"></i> User Guide: Suspended Sediment Field Data Sheet</h4>
  <p><strong>Purpose:</strong> This application helps you document suspended sediment sampling activities in the field and generates properly formatted reports.</p>
  <p><strong>How to Use:</strong></p>
  <ul>
    <li>Enter station details, measurement type, and stage conditions.</li>
    <li>Select and configure sampling methods (e.g., EWI, Grab).</li>
    <li>For methods like EWI and Grab, record start and end times for each individual sample (A, B, C, D).</li>
    <li>For other methods, record start and end times for the entire sampling process.</li>
    <li>Generate a formatted Excel report with all entered data, including detailed section widths for EWI.</li>
  </ul>
</div>
')

# UI definition
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  useShinyjs(),
  titlePanel("Suspended Sediment Field Data Sheet"),

  # User Guide Section
  fluidRow(
    column(12, wellPanel(
      tags$a(
        id = "toggle_guide",
        class = "btn btn-info btn-sm pull-right",
        href = "#",
        style = "margin-top: -5px;",
        HTML('<i class="fa fa-question-circle"></i> Toggle User Guide')
      ),
      tags$div(id = "user_guide", user_guide_html)
    ))
  ),

  # Station Information Section
  fluidRow(
    column(6, wellPanel(
      h4("Station Information"),
      textInput("station_number", "Station Number:", ""),
      dateInput("date", "Date:", Sys.Date()),
      textInput("station_name", "Station Name:", ""),
      textInput("party", "Party:", ""),
      numericInput("water_temp", "Water Temperature (°C):", value = NA, min = -10, max = 50, step = 0.1)
    ))
  ),

  # Channel Information Section
  fluidRow(
    column(12, wellPanel(
      h4("Channel Information"),
      fluidRow(
        column(4, numericInput("left_edge", "Sediment Channel Left Edge of Water (ft):", value = 0, step = 0.1)),
        column(4, numericInput("right_edge", "Sediment Channel Right Edge of Water (ft):", value = 0, step = 0.1)),
        column(4, div(style = "margin-top: 25px;", textOutput("channel_width")))
      )
    ))
  ),

  # Sampling Methods Section
  fluidRow(
    column(12, wellPanel(
      h4("Sampling Methods"),
      checkboxGroupInput("selected_methods", "Select Sampling Methods:", choices = sampling_methods, selected = "EWI Iso"),
      hr(),
      uiOutput("method_panels")
    ))
  ),

  # Save, Download, and Clear Buttons
  fluidRow(
    column(12, wellPanel(
      actionButton("save_btn", "Save Data", icon = icon("save"), class = "btn-primary", width = "48%"),
      downloadButton("download_excel", "Download Excel", class = "btn-success", style = "width:48%; float:right;"),
      br(), br(),
      actionButton("clear_btn", "Clear Form", icon = icon("eraser"), class = "btn-warning", width = "100%")
    ))
  ),

  # Saved Data Section
  fluidRow(
    column(12, wellPanel(
      h4("Saved Data"),
      verbatimTextOutput("data_output")
    ))
  )
)

# Server logic
server <- function(input, output, session) {
  # Store the number of verticals for Multi-Vertical method
  multi_vertical_count <- reactiveVal(3)

  # Additional reactive value to track if update button was pressed
  update_button_pressed <- reactiveVal(FALSE)

  # Hide/show user guide
  observeEvent(input$toggle_guide, {
    shinyjs::toggle("user_guide")
  })

  # Reactive values for saved data
  saved_data <- reactiveVal(data.frame())

  # Channel width calculation
  channel_width <- reactive({
    validate(
      need(!is.na(input$left_edge) && is.numeric(input$left_edge), "Left edge must be a numeric value."),
      need(!is.na(input$right_edge) && is.numeric(input$right_edge), "Right edge must be a numeric value.")
    )
    abs(as.numeric(input$right_edge) - as.numeric(input$left_edge))
  })

  output$channel_width <- renderText({
    paste("Channel Width:", round(channel_width(), 2), "ft")
  })

  # Modified compute_sections function for EWI sampling
  # This places the first section at half the equally divided width
  compute_sections <- function(left_edge, right_edge, num_sections) {
    validate(
      need(!is.na(left_edge) && !is.na(right_edge), "Left and Right Edges must be numeric."),
      need(num_sections > 1, "Number of sections must be greater than 1.")
    )

    # Calculate the width of each section
    section_width <- (right_edge - left_edge) / num_sections

    # Calculate sampling points - starting at half the section width from left edge
    # This places the first point at half the section width
    sampling_points <- left_edge + section_width * (1:num_sections - 0.5)

    data.frame(
      Section = 1:num_sections,
      DistanceFromLEW = round(sampling_points, 2)
    )
  }

  # Render section tables for EWI
  observe({
    req(input$selected_methods)

    for (method in input$selected_methods) {
      if (method %in% c("EWI Iso", "EWI Non-Iso")) {
        method_id <- gsub(" ", "_", method)

        output[[paste0(method_id, "_sections_table")]] <- renderDT({
          req(input$left_edge, input$right_edge, input[[paste0(method_id, "_num_verticals")]])
          sections <- compute_sections(
            as.numeric(input$left_edge),
            as.numeric(input$right_edge),
            as.numeric(input[[paste0(method_id, "_num_verticals")]])
          )
          datatable(sections, options = list(pageLength = 10), rownames = FALSE)
        })
      }
    }
  })

  # Function to create time inputs for a method or sample
  create_time_inputs <- function(id_prefix) {
    tagList(
      fluidRow(
        column(4, textInput(paste0(id_prefix, "_start_time"), "Start Time (HH:MM):", "")),
        column(4, textInput(paste0(id_prefix, "_end_time"), "End Time (HH:MM):", "")),
        column(4, textInput(paste0(id_prefix, "_avg_time"), "Average Time (HH:MM):", ""))
      )
    )
  }

  # Function to create sample selection UI, bottle inputs, and time inputs
  create_sample_inputs <- function(method_id, sample_count) {
    samples <- LETTERS[1:sample_count]

    # Create UI for sample selection
    sample_selection <- checkboxGroupInput(
      paste0(method_id, "_samples"),
      "Sample(s) to collect:",
      choices = samples,
      selected = samples[1],
      inline = TRUE
    )

    # Create dynamic UI for bottle count inputs and time inputs based on selected samples
    bottle_and_time_inputs <- uiOutput(paste0(method_id, "_bottle_time_inputs"))

    tagList(
      sample_selection,
      bottle_and_time_inputs
    )
  }

  # Observer for creating bottle inputs and time inputs when samples are selected
  observe({
    req(input$selected_methods)

    for(method in input$selected_methods) {
      method_id <- gsub(" ", "_", method)

      if (method %in% c("EWI Iso", "EWI Non-Iso", "Grab")) {
        # Get selected samples
        selected_samples <- input[[paste0(method_id, "_samples")]]

        if (!is.null(selected_samples) && length(selected_samples) > 0) {
          # Create bottle input fields and time inputs for selected samples
          output[[paste0(method_id, "_bottle_time_inputs")]] <- renderUI({
            # Create inputs for each selected sample
            sample_panels <- lapply(selected_samples, function(sample) {
              wellPanel(
                h5(paste("Sample", sample)),
                numericInput(
                  paste0(method_id, "_bottles_", sample),
                  paste("Number of bottles for Sample", sample, ":"),
                  value = 1,
                  min = 1,
                  step = 1
                ),
                h5("Time Information"),
                create_time_inputs(paste0(method_id, "_sample_", sample))
              )
            })

            do.call(tagList, sample_panels)
          })
        }
      }
    }
  })

  # Direct observer for the Multi-Vertical update button
  observeEvent(input$Multi_Vertical_update_btn, {
    req(input$Multi_Vertical_num_verticals)
    num_verticals <- as.integer(input$Multi_Vertical_num_verticals)
    num_verticals <- min(max(num_verticals, 1), 10)  # Limit between 1 and 10

    # Update the reactive value to store the number of verticals
    multi_vertical_count(num_verticals)

    # Dynamically generate input fields for distances
    output$Multi_Vertical_distance_inputs <- renderUI({
      lapply(1:num_verticals, function(i) {
        numericInput(
          paste0("Multi_Vertical_distance_", i),
          paste("Vertical", i, "Distance from LEW (ft):"),
          value = 0,
          min = 0,
          step = 0.1
        )
      })
    })
  })

  # Dynamic UI for sampling methods
  output$method_panels <- renderUI({
    req(input$selected_methods)
    method_panels <- lapply(input$selected_methods, function(method) {
      method_id <- gsub(" ", "_", method)

      # Common sampler type selector for all methods
      sampler_selector <- tagList(
        selectInput(paste0(method_id, "_sampler_type"), "Sampler Type:", sampler_types),
        conditionalPanel(
          condition = paste0("input.", method_id, "_sampler_type == 'Other'"),
          textAreaInput(paste0(method_id, "_other_sampler"), "Please explain other sampler:", rows = 2)
        )
      )

      if (method %in% c("EWI Iso", "EWI Non-Iso")) {
        # For EWI methods - allow up to 4 samples (A,B,C,D)
        tagList(
          wellPanel(
            h4(paste(method, "Sampling")),
            sampler_selector,
            create_sample_inputs(method_id, 4), # Add up to 4 samples with bottle inputs and time inputs
            numericInput(paste0(method_id, "_num_verticals"), "Number of Sections:", value = 10, min = 2, step = 1),
            DTOutput(paste0(method_id, "_sections_table"))
          )
        )
      } else if (method == "Single Vertical") {
        # For Single Vertical method
        tagList(
          wellPanel(
            h4(paste(method, "Sampling")),
            sampler_selector,
            numericInput(paste0(method_id, "_bottles"), "Number of bottles:", value = 1, min = 1, step = 1),
            numericInput(paste0(method_id, "_distance"), "Distance from LEW (ft):", value = 0, min = 0, step = 0.1),
            h5("Time Information"),
            create_time_inputs(method_id)
          )
        )
      } else if (method == "Multi-Vertical") {
        # For Multi-Vertical method with manual section entries - using the working example approach
        tagList(
          wellPanel(
            h4("Multi-Vertical Sampling"),
            sampler_selector,
            numericInput("Multi_Vertical_bottles", "Number of bottles:", value = 1, min = 1, step = 1),
            numericInput("Multi_Vertical_num_verticals", "Number of Verticals:", value = multi_vertical_count(), min = 1, max = 10, step = 1),
            actionButton("Multi_Vertical_update_btn", "Update Verticals", class = "btn-info"),
            p("Enter the distances from Left Edge of Water (LEW) where samples were taken:"),
            uiOutput("Multi_Vertical_distance_inputs"),
            h5("Time Information"),
            create_time_inputs("Multi_Vertical")
          )
        )
      } else if (method == "Point") {
        # For Point method - add distance from LEW
        tagList(
          wellPanel(
            h4(paste(method, "Sampling")),
            sampler_selector,
            numericInput(paste0(method_id, "_bottles"), "Number of bottles:", value = 1, min = 1, step = 1),
            numericInput(paste0(method_id, "_distance"), "Distance from LEW (ft):", value = 0, min = 0, step = 0.1),
            h5("Time Information"),
            create_time_inputs(method_id)
          )
        )
      } else if (method == "Grab") {
        # For Grab method - allow up to 2 samples (A,B)
        tagList(
          wellPanel(
            h4(paste(method, "Sampling")),
            sampler_selector,
            create_sample_inputs(method_id, 2) # Add up to 2 samples with bottle inputs and time inputs
          )
        )
      } else {
        # For other methods (ISCO, Box Single)
        tagList(
          wellPanel(
            h4(paste(method, "Sampling")),
            sampler_selector,
            numericInput(paste0(method_id, "_bottles"), "Number of bottles:", value = 1, min = 1, step = 1),
            h5("Time Information"),
            create_time_inputs(method_id)
          )
        )
      }
    })

    do.call(tagList, method_panels)
  })

  # Create initial distance input UI for Multi-Vertical when first loaded
  observe({
    req(input$selected_methods)

    if("Multi-Vertical" %in% input$selected_methods && !update_button_pressed()) {
      # Initialize with the default number of verticals
      num_verticals <- multi_vertical_count()

      output$Multi_Vertical_distance_inputs <- renderUI({
        lapply(1:num_verticals, function(i) {
          numericInput(
            paste0("Multi_Vertical_distance_", i),
            paste("Vertical", i, "Distance from LEW (ft):"),
            value = 0,
            min = 0,
            step = 0.1
          )
        })
      })
    }
  })

  # Function to collect bottle counts for samples
  collect_bottle_counts <- function(method_id, samples) {
    if (is.null(samples) || length(samples) == 0) {
      return(NULL)
    }

    bottle_counts <- sapply(samples, function(sample) {
      input_name <- paste0(method_id, "_bottles_", sample)
      if (!is.null(input[[input_name]])) {
        return(input[[input_name]])
      } else {
        return(NA)
      }
    })

    names(bottle_counts) <- samples
    return(bottle_counts)
  }

  # Function to collect time data for a method or sample
  collect_time_data <- function(id_prefix) {
    time_data <- list(
      start_time = input[[paste0(id_prefix, "_start_time")]],
      end_time = input[[paste0(id_prefix, "_end_time")]],
      avg_time = input[[paste0(id_prefix, "_avg_time")]]
    )
    return(time_data)
  }

  # Function to collect sample time data
  collect_sample_times <- function(method_id, samples) {
    if (is.null(samples) || length(samples) == 0) {
      return(NULL)
    }

    sample_times <- lapply(samples, function(sample) {
      id_prefix <- paste0(method_id, "_sample_", sample)
      collect_time_data(id_prefix)
    })

    names(sample_times) <- samples
    return(sample_times)
  }

  # Function to collect all method data
  collect_method_data <- function() {
    method_data <- list()

    for (method in input$selected_methods) {
      method_id <- gsub(" ", "_", method)

      # Common data for all methods
      method_info <- list(
        sampler_type = input[[paste0(method_id, "_sampler_type")]]
      )

      # Add other sampler info if applicable
      if (input[[paste0(method_id, "_sampler_type")]] == "Other") {
        method_info$other_sampler <- input[[paste0(method_id, "_other_sampler")]]
      }

      if (method %in% c("EWI Iso", "EWI Non-Iso")) {
        # For EWI methods
        num_sections <- as.numeric(input[[paste0(method_id, "_num_verticals")]])
        sections <- compute_sections(
          as.numeric(input$left_edge),
          as.numeric(input$right_edge),
          num_sections
        )

        method_info$num_verticals <- num_sections
        method_info$sections <- sections
        method_info$samples <- input[[paste0(method_id, "_samples")]] # Collect selected samples

        # Collect bottle counts for each sample
        method_info$bottle_counts <- collect_bottle_counts(method_id, method_info$samples)

        # Collect time data for each sample
        method_info$sample_times <- collect_sample_times(method_id, method_info$samples)

      } else if (method == "Single Vertical") {
        # For Single Vertical method
        method_info$distance_from_lew <- input[[paste0(method_id, "_distance")]]
        method_info$bottles <- input[[paste0(method_id, "_bottles")]]
        method_info$time_data <- collect_time_data(method_id)
      } else if (method == "Multi-Vertical") {
        # For Multi-Vertical method - use the stored count
        num_verticals <- multi_vertical_count()

        # Collect distances for each vertical
        vertical_distances <- numeric(num_verticals)
        for (i in 1:num_verticals) {
          distance_input <- paste0(method_id, "_distance_", i)
          if (!is.null(input[[distance_input]])) {
            vertical_distances[i] <- input[[distance_input]]
          }
        }

        method_info$num_verticals <- num_verticals
        method_info$vertical_distances <- vertical_distances
        method_info$bottles <- input$Multi_Vertical_bottles  # Use the flat ID for Multi-Vertical bottles
        method_info$time_data <- collect_time_data("Multi_Vertical")
      } else if (method == "Point") {
        # For Point method including distance from LEW
        method_info$distance_from_lew <- input[[paste0(method_id, "_distance")]]
        method_info$bottles <- input[[paste0(method_id, "_bottles")]]
        method_info$time_data <- collect_time_data(method_id)
      } else if (method == "Grab") {
        # For Grab method, collect selected samples
        method_info$samples <- input[[paste0(method_id, "_samples")]]

        # Collect bottle counts for each sample
        method_info$bottle_counts <- collect_bottle_counts(method_id, method_info$samples)

        # Collect time data for each sample
        method_info$sample_times <- collect_sample_times(method_id, method_info$samples)
      } else {
        # For other methods (ISCO, Box Single)
        method_info$bottles <- input[[paste0(method_id, "_bottles")]]
        method_info$time_data <- collect_time_data(method_id)
      }

      method_data[[method]] <- method_info
    }

    return(method_data)
  }

  # Generate a formatted filename based on station number, date, and sampling methods
  generate_filename <- function() {
    # Get station number (default to "unknown" if empty)
    station_num <- ifelse(input$station_number == "", "unknown", input$station_number)

    # Format date as YYYYMMDD
    date_str <- format(input$date, "%Y%m%d")

    # Get selected methods and format them for the filename
    methods_str <- paste(gsub(" ", "", input$selected_methods), collapse = "_")

    # Create the filename
    filename <- paste0(station_num, "_", date_str, "_", methods_str, ".xlsx")

    return(filename)
  }

  # Download Excel File with color coding and styling
  output$download_excel <- downloadHandler(
    filename = function() {
      generate_filename()
    },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Sediment Data")

      # Define styles for better appearance
      headerStyle <- createStyle(
        fontSize = 12,
        fontColour = "white",
        halign = "center",
        fgFill = "#336699",
        border = "TopBottomLeftRight",
        borderColour = "#D9D9D9",
        textDecoration = "bold"
      )

      subheaderStyle <- createStyle(
        fontSize = 11,
        fontColour = "#000000",
        halign = "left",
        fgFill = "#DEEBF7",
        border = "TopBottomLeftRight",
        borderColour = "#D9D9D9",
        textDecoration = "bold"
      )

      fieldStyle <- createStyle(
        fontSize = 10,
        halign = "left",
        fgFill = "#F2F2F2",
        border = "TopBottomLeftRight",
        borderColour = "#D9D9D9"
      )

      valueStyle <- createStyle(
        fontSize = 10,
        halign = "left",
        border = "TopBottomLeftRight",
        borderColour = "#D9D9D9"
      )

      tableHeaderStyle <- createStyle(
        fontSize = 10,
        halign = "center",
        fgFill = "#BDD7EE",
        border = "TopBottomLeftRight",
        borderColour = "#D9D9D9",
        textDecoration = "bold"
      )

      evenRowStyle <- createStyle(
        fontSize = 10,
        halign = "center",
        border = "TopBottomLeftRight",
        borderColour = "#D9D9D9"
      )

      oddRowStyle <- createStyle(
        fontSize = 10,
        halign = "center",
        fgFill = "#F2F2F2",
        border = "TopBottomLeftRight",
        borderColour = "#D9D9D9"
      )

      timeStyle <- createStyle(
        fontSize = 10,
        halign = "center",
        fgFill = "#E2EFDA",
        border = "TopBottomLeftRight",
        borderColour = "#D9D9D9"
      )

      # Configure for single-page printing
      pageSetup(wb, "Sediment Data",
                orientation = "landscape",
                fitToWidth = TRUE,
                fitToHeight = 1)

      # Set column widths for better readability
      setColWidths(wb, "Sediment Data", cols = 1:2, widths = c(30, 20))

      # Add report title
      mergeCells(wb, "Sediment Data", rows = 1, cols = 1:2)
      writeData(wb, "Sediment Data", "SUSPENDED SEDIMENT FIELD DATA SHEET", startRow = 1, startCol = 1)
      addStyle(wb, "Sediment Data", headerStyle, rows = 1, cols = 1)

      # Add timestamp and creator information using the current values
      current_time <- "2025-04-11 19:31:14"
      current_user <- "beccabagg"

      df_report_info <- data.frame(
        Field = c("Report Generated On", "Report Generated By"),
        Value = c(current_time, current_user)
      )
      writeData(wb, "Sediment Data", df_report_info, startRow = 2)
      addStyle(wb, "Sediment Data", fieldStyle, rows = 2:3, cols = 1)
      addStyle(wb, "Sediment Data", valueStyle, rows = 2:3, cols = 2)

      # Add a blank row
      writeData(wb, "Sediment Data", data.frame(Field = "", Value = ""), startRow = 4)

      # Station Information section header
      mergeCells(wb, "Sediment Data", rows = 5, cols = 1:2)
      writeData(wb, "Sediment Data", "STATION INFORMATION", startRow = 5, startCol = 1)
      addStyle(wb, "Sediment Data", headerStyle, rows = 5, cols = 1)

      # Add station information
      station_data <- data.frame(
        Field = c("Station Number", "Date", "Station Name", "Party", "Water Temp (°C)"),
        Value = c(input$station_number, as.character(input$date), input$station_name, input$party, input$water_temp)
      )
      writeData(wb, "Sediment Data", station_data, startRow = 6)
      addStyle(wb, "Sediment Data", fieldStyle, rows = 6:10, cols = 1)
      addStyle(wb, "Sediment Data", valueStyle, rows = 6:10, cols = 2)

      # Add a blank row
      writeData(wb, "Sediment Data", data.frame(Field = "", Value = ""), startRow = 11)

      # Channel Information section header
      mergeCells(wb, "Sediment Data", rows = 12, cols = 1:2)
      writeData(wb, "Sediment Data", "CHANNEL INFORMATION", startRow = 12, startCol = 1)
      addStyle(wb, "Sediment Data", headerStyle, rows = 12, cols = 1)

      # Add channel information
      channel_data <- data.frame(
        Field = c("Left Edge (ft)", "Right Edge (ft)", "Channel Width (ft)"),
        Value = c(input$left_edge, input$right_edge, round(channel_width(), 2))
      )
      writeData(wb, "Sediment Data", channel_data, startRow = 13)
      addStyle(wb, "Sediment Data", fieldStyle, rows = 13:15, cols = 1)
      addStyle(wb, "Sediment Data", valueStyle, rows = 13:15, cols = 2)

      # Add a blank row
      writeData(wb, "Sediment Data", data.frame(Field = "", Value = ""), startRow = 16)

      # Sampling Methods section header
      mergeCells(wb, "Sediment Data", rows = 17, cols = 1:2)
      writeData(wb, "Sediment Data", "SAMPLING METHODS", startRow = 17, startCol = 1)
      addStyle(wb, "Sediment Data", headerStyle, rows = 17, cols = 1)

      # Add sampling methods and data
      start_row <- 18
      all_method_data <- collect_method_data()

      for (method in input$selected_methods) {
        method_data <- all_method_data[[method]]

        # Method header with colored background
        mergeCells(wb, "Sediment Data", rows = start_row, cols = 1:2)
        writeData(wb, "Sediment Data", paste("Method:", method), startRow = start_row, startCol = 1)
        addStyle(wb, "Sediment Data", subheaderStyle, rows = start_row, cols = 1)
        start_row <- start_row + 1

        # Write sampler type with styling
        writeData(wb, "Sediment Data", "Sampler Type:", startRow = start_row, startCol = 1)
        writeData(wb, "Sediment Data", method_data$sampler_type, startRow = start_row, startCol = 2)
        addStyle(wb, "Sediment Data", fieldStyle, rows = start_row, cols = 1)
        addStyle(wb, "Sediment Data", valueStyle, rows = start_row, cols = 2)
        start_row <- start_row + 1

        if (method_data$sampler_type == "Other") {
          writeData(wb, "Sediment Data", "Other Sampler:", startRow = start_row, startCol = 1)
          writeData(wb, "Sediment Data", method_data$other_sampler, startRow = start_row, startCol = 2)
          addStyle(wb, "Sediment Data", fieldStyle, rows = start_row, cols = 1)
          addStyle(wb, "Sediment Data", valueStyle, rows = start_row, cols = 2)
          start_row <- start_row + 1
        }

        if (method %in% c("EWI Iso", "EWI Non-Iso")) {
          # Write EWI samples, bottle counts, and time information
          if (!is.null(method_data$samples) && length(method_data$samples) > 0) {
            # Samples header
            mergeCells(wb, "Sediment Data", rows = start_row, cols = 1:2)
            sample_info <- paste("Samples:", paste(method_data$samples, collapse = ", "))
            writeData(wb, "Sediment Data", sample_info, startRow = start_row, startCol = 1)
            addStyle(wb, "Sediment Data", subheaderStyle, rows = start_row, cols = 1)
            start_row <- start_row + 1

            # Write bottle counts and time information for each sample
            if (!is.null(method_data$bottle_counts) && !is.null(method_data$sample_times)) {
              for (sample in names(method_data$bottle_counts)) {
                bottle_count <- method_data$bottle_counts[sample]
                sample_time <- method_data$sample_times[[sample]]

                # Sample header
                writeData(wb, "Sediment Data", paste("Sample", sample), startRow = start_row, startCol = 1)
                mergeCells(wb, "Sediment Data", rows = start_row, cols = 1:2)
                addStyle(wb, "Sediment Data", subheaderStyle, rows = start_row, cols = 1)
                start_row <- start_row + 1

                # Sample data
                bottle_data <- data.frame(
                  Field = c(
                    "Bottle Count",
                    "Start Time",
                    "End Time",
                    "Average Time"
                  ),
                  Value = c(
                    bottle_count,
                    sample_time$start_time,
                    sample_time$end_time,
                    sample_time$avg_time
                  )
                )
                writeData(wb, "Sediment Data", bottle_data, startRow = start_row)
                addStyle(wb, "Sediment Data", fieldStyle, rows = start_row:(start_row+3), cols = 1)
                addStyle(wb, "Sediment Data", valueStyle, rows = start_row, cols = 2)
                addStyle(wb, "Sediment Data", timeStyle, rows = (start_row+1):(start_row+3), cols = 2)
                start_row <- start_row + 5  # Add extra space after each sample
              }
            }
          }

          # Write EWI section data with better formatting
          mergeCells(wb, "Sediment Data", rows = start_row, cols = 1:2)
          writeData(wb, "Sediment Data", paste("Sections Data (", method_data$num_verticals, " verticals)"),
                    startRow = start_row, startCol = 1)
          addStyle(wb, "Sediment Data", subheaderStyle, rows = start_row, cols = 1)
          start_row <- start_row + 1

          # Write section headers with style
          writeData(wb, "Sediment Data", "Section", startRow = start_row, startCol = 1)
          writeData(wb, "Sediment Data", "Distance From LEW (ft)", startRow = start_row, startCol = 2)
          addStyle(wb, "Sediment Data", tableHeaderStyle, rows = start_row, cols = 1:2)
          start_row <- start_row + 1

          # Write section data with alternating row colors
          sections <- method_data$sections
          for (i in 1:nrow(sections)) {
            writeData(wb, "Sediment Data", sections$Section[i], startRow = start_row, startCol = 1)
            writeData(wb, "Sediment Data", sections$DistanceFromLEW[i], startRow = start_row, startCol = 2)

            # Apply alternating row styles
            if (i %% 2 == 0) {
              addStyle(wb, "Sediment Data", evenRowStyle, rows = start_row, cols = 1:2)
            } else {
              addStyle(wb, "Sediment Data", oddRowStyle, rows = start_row, cols = 1:2)
            }

            start_row <- start_row + 1
          }
          start_row <- start_row + 2  # Add spacing after the section data
        } else if (method == "Single Vertical") {
          # Write Single Vertical data including bottle count and time info
          time_data <- method_data$time_data
          vertical_data <- data.frame(
            Field = c(
              "Distance from LEW (ft)",
              "Number of bottles",
              "Start Time",
              "End Time",
              "Average Time"
            ),
            Value = c(
              method_data$distance_from_lew,
              method_data$bottles,
              time_data$start_time,
              time_data$end_time,
              time_data$avg_time
            )
          )
          writeData(wb, "Sediment Data", vertical_data, startRow = start_row)
          addStyle(wb, "Sediment Data", fieldStyle, rows = start_row:(start_row+4), cols = 1)
          addStyle(wb, "Sediment Data", valueStyle, rows = start_row:(start_row+1), cols = 2)
          addStyle(wb, "Sediment Data", timeStyle, rows = (start_row+2):(start_row+4), cols = 2)
          start_row <- start_row + 6
        } else if (method == "Point") {
          # Write Point data including distance from LEW, bottle count, and time info
          time_data <- method_data$time_data
          point_data <- data.frame(
            Field = c(
              "Distance from LEW (ft)",
              "Number of bottles",
              "Start Time",
              "End Time",
              "Average Time"
            ),
            Value = c(
              method_data$distance_from_lew,
              method_data$bottles,
              time_data$start_time,
              time_data$end_time,
              time_data$avg_time
            )
          )
          writeData(wb, "Sediment Data", point_data, startRow = start_row)
          addStyle(wb, "Sediment Data", fieldStyle, rows = start_row:(start_row+4), cols = 1)
          addStyle(wb, "Sediment Data", valueStyle, rows = start_row:(start_row+1), cols = 2)
          addStyle(wb, "Sediment Data", timeStyle, rows = (start_row+2):(start_row+4), cols = 2)
          start_row <- start_row + 6
        } else if (method == "Multi-Vertical") {
          # Write bottle count and time info
          time_data <- method_data$time_data
          multi_data <- data.frame(
            Field = c(
              "Number of bottles",
              "Start Time",
              "End Time",
              "Average Time"
            ),
            Value = c(
              method_data$bottles,
              time_data$start_time,
              time_data$end_time,
              time_data$avg_time
            )
          )
          writeData(wb, "Sediment Data", multi_data, startRow = start_row)
          addStyle(wb, "Sediment Data", fieldStyle, rows = start_row:(start_row+3), cols = 1)
          addStyle(wb, "Sediment Data", valueStyle, rows = start_row, cols = 2)
          addStyle(wb, "Sediment Data", timeStyle, rows = (start_row+1):(start_row+3), cols = 2)
          start_row <- start_row + 5

          # Write Multi-Vertical verticals header
          mergeCells(wb, "Sediment Data", rows = start_row, cols = 1:2)
          writeData(wb, "Sediment Data", "Vertical Distances", startRow = start_row, startCol = 1)
          addStyle(wb, "Sediment Data", subheaderStyle, rows = start_row, cols = 1)
          start_row <- start_row + 1

          # Write vertical headers
          writeData(wb, "Sediment Data", "Vertical", startRow = start_row, startCol = 1)
          writeData(wb, "Sediment Data", "Distance From LEW (ft)", startRow = start_row, startCol = 2)
          addStyle(wb, "Sediment Data", tableHeaderStyle, rows = start_row, cols = 1:2)
          start_row <- start_row + 1

          # Write vertical data with alternating row colors
          for (i in 1:method_data$num_verticals) {
            writeData(wb, "Sediment Data", i, startRow = start_row, startCol = 1)
            writeData(wb, "Sediment Data", method_data$vertical_distances[i], startRow = start_row, startCol = 2)

            # Apply alternating row styles
            if (i %% 2 == 0) {
              addStyle(wb, "Sediment Data", evenRowStyle, rows = start_row, cols = 1:2)
            } else {
              addStyle(wb, "Sediment Data", oddRowStyle, rows = start_row, cols = 1:2)
            }

            start_row <- start_row + 1
          }
          start_row <- start_row + 2  # Add spacing after the vertical data
        } else if (method == "Grab") {
          # Write Grab sample data with bottle counts and time info
          if (!is.null(method_data$samples) && length(method_data$samples) > 0) {
            for (sample in names(method_data$bottle_counts)) {
              bottle_count <- method_data$bottle_counts[sample]
              sample_time <- method_data$sample_times[[sample]]

              # Sample header
              writeData(wb, "Sediment Data", paste("Sample", sample), startRow = start_row, startCol = 1)
              mergeCells(wb, "Sediment Data", rows = start_row, cols = 1:2)
              addStyle(wb, "Sediment Data", subheaderStyle, rows = start_row, cols = 1)
              start_row <- start_row + 1

              # Sample data
              sample_data <- data.frame(
                Field = c(
                  "Bottle Count",
                  "Start Time",
                  "End Time",
                  "Average Time"
                ),
                Value = c(
                  bottle_count,
                  sample_time$start_time,
                  sample_time$end_time,
                  sample_time$avg_time
                )
              )
              writeData(wb, "Sediment Data", sample_data, startRow = start_row)
              addStyle(wb, "Sediment Data", fieldStyle, rows = start_row:(start_row+3), cols = 1)
              addStyle(wb, "Sediment Data", valueStyle, rows = start_row, cols = 2)
              addStyle(wb, "Sediment Data", timeStyle, rows = (start_row+1):(start_row+3), cols = 2)
              start_row <- start_row + 5  # Add spacing after each sample
            }
          }
          start_row <- start_row + 1
        } else {
          # For other methods (ISCO, Box Single), include bottle count and time info
          time_data <- method_data$time_data
          other_data <- data.frame(
            Field = c(
              "Number of bottles",
              "Start Time",
              "End Time",
              "Average Time"
            ),
            Value = c(
              method_data$bottles,
              time_data$start_time,
              time_data$end_time,
              time_data$avg_time
            )
          )
          writeData(wb, "Sediment Data", other_data, startRow = start_row)
          addStyle(wb, "Sediment Data", fieldStyle, rows = start_row:(start_row+3), cols = 1)
          addStyle(wb, "Sediment Data", valueStyle, rows = start_row, cols = 2)
          addStyle(wb, "Sediment Data", timeStyle, rows = (start_row+1):(start_row+3), cols = 2)
          start_row <- start_row + 5
        }
      }

      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # Helper function to calculate average time
  calculate_avg_time <- function(start_time, end_time) {
    if (start_time == "" || end_time == "") {
      return("")
    }

    # Try to parse times
    tryCatch({
      # Convert to POSIXct
      start_time_obj <- as.POSIXct(paste(Sys.Date(), start_time), format = "%Y-%m-%d %H:%M")
      end_time_obj <- as.POSIXct(paste(Sys.Date(), end_time), format = "%Y-%m-%d %H:%M")

      # Calculate average time
      avg_time_obj <- start_time_obj + (difftime(end_time_obj, start_time_obj, units = "secs") / 2)

      # Format as HH:MM
      format(avg_time_obj, "%H:%M")
    }, error = function(e) {
      # Return empty string on error
      return("")
    })
  }

  # Observers to auto-calculate average times
  observe({
    req(input$selected_methods)

    for (method in input$selected_methods) {
      method_id <- gsub(" ", "_", method)

      # Only add time observers for methods that are not EWI or Grab
      if (!method %in% c("EWI Iso", "EWI Non-Iso", "Grab")) {
        # Create local variables to avoid issues with reactive context
        local_method_id <- method_id

        # Special case for Multi-Vertical
        actual_id <- ifelse(method == "Multi-Vertical", "Multi_Vertical", local_method_id)

        # Watch for changes in start and end time
        observeEvent(c(input[[paste0(actual_id, "_start_time")]],
                       input[[paste0(actual_id, "_end_time")]]), {

                         start_time <- input[[paste0(actual_id, "_start_time")]]
                         end_time <- input[[paste0(actual_id, "_end_time")]]

                         if (start_time != "" && end_time != "") {
                           avg_time <- calculate_avg_time(start_time, end_time)
                           updateTextInput(session, paste0(actual_id, "_avg_time"), value = avg_time)
                         }
                       })
      }

      # Add sample time observers for EWI and Grab methods
      if (method %in% c("EWI Iso", "EWI Non-Iso", "Grab")) {
        local_method_id <- method_id

        observe({
          req(input[[paste0(local_method_id, "_samples")]])

          samples <- input[[paste0(local_method_id, "_samples")]]

          for (sample in samples) {
            local_sample <- sample
            sample_prefix <- paste0(local_method_id, "_sample_", local_sample)

            observeEvent(c(input[[paste0(sample_prefix, "_start_time")]],
                           input[[paste0(sample_prefix, "_end_time")]]), {

                             sample_start_time <- input[[paste0(sample_prefix, "_start_time")]]
                             sample_end_time <- input[[paste0(sample_prefix, "_end_time")]]

                             if (sample_start_time != "" && sample_end_time != "") {
                               sample_avg_time <- calculate_avg_time(sample_start_time, sample_end_time)
                               updateTextInput(session, paste0(sample_prefix, "_avg_time"), value = sample_avg_time)
                             }
                           })
          }
        })
      }
    }
  })

  # Clear form
  observeEvent(input$clear_btn, {
    # Reset all inputs
    updateTextInput(session, "station_number", value = "")
    updateDateInput(session, "date", value = Sys.Date())
    updateTextInput(session, "station_name", value = "")
    updateTextInput(session, "party", value = "")
    updateNumericInput(session, "water_temp", value = NA)
    updateNumericInput(session, "left_edge", value = 0)
    updateNumericInput(session, "right_edge", value = 0)
    updateCheckboxGroupInput(session, "selected_methods", selected = "EWI Iso")

    # Reset Multi-Vertical counter and flag
    multi_vertical_count(3)
    update_button_pressed(FALSE)
  })

  # Create output placeholder for saved data
  output$data_output <- renderPrint({
    if (length(saved_data()) > 0) {
      saved_data()
    } else {
      cat("No data saved yet. Click 'Save Data' to save the current form data.")
    }
  })

  # Save button handler
  observeEvent(input$save_btn, {
    # Collect all data
    all_data <- list(
      timestamp = "2025-04-11 19:31:14",
      user = "beccabagg",
      station = list(
        number = input$station_number,
        name = input$station_name,
        date = input$date,
        party = input$party,
        water_temp = input$water_temp
      ),
      channel = list(
        left_edge = input$left_edge,
        right_edge = input$right_edge,
        width = channel_width()
      ),
      methods = collect_method_data()
    )

    # Show a confirmation message
    saved_data(paste("Data saved at", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

    # Show confirmation dialog
    showModal(modalDialog(
      title = "Data Saved",
      "Form data has been saved successfully.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)