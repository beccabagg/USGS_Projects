# This script analyzes total station data to extract the summary metadata data frames
# It requires a CSV to be uploaded by providing the local path to the file on your system.
# ----------------------------------------------------------------------------
# NOTE: This script requires 2 elevation values for each location.
# Additional values should be removed from the CSV prior to importing.
# Code by Rebecca Baggott, contact rbaggott@usgs.gov for questions
# ----------------------------------------------------------------------------

# Load required libraries
packages <- c("dplyr", "tidyr", "xml2", "shiny","shinythemes")

for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# Define UI for the application
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Automated Total Station Data Analyzer"),

  p("Welcome to the Automated Total Station Data Analyzer web application! This tool analyzes CSV data from total stations, specifically Trimble S7 with TSC5. Please upload a total station CSV file by entering the local path."),

  p(tags$strong("Important Requirements:")),
  p("Replicate point names must be identical in spelling and case throughout the dataset. Correct any errors before import."),

  p(tags$strong("Entering Dial/Nail/Bolt Readings:")),
  p("Enrich the summary data by entering Dial/Nail/Bolt readings. Input the values and click 'Update Summary' to include them in the analysis. Similarly, input party name, site number, and date, then select 'Update Info' to save these details. To correct any input errors, select 'Analyze Data' to refresh and remove entered values."),

  p(tags$strong("Starting Object Elevation:")),
  p("When exporting to XML, you can specify a Starting Object Point and its known elevation. This value will override the calculated elevation in the exported XML file."),

  p(tags$strong("Troubleshooting:")),
  p("If the code fails to run, open the selected CSV, and select all cells. Select Home --> Format --> Number (with 4 decimal places)."),
  p(tags$em("This script was developed using R version 4.4.2.")),
  p(tags$em("Developed by Rebecca Baggott. For assistance, contact rbaggott@usgs.gov.")),

  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      actionButton("analyze", "Analyze Data"),
      hr(),

      textInput("point_name", "Enter Point Name"),
      textInput("hex_input", "Enter Dial/Nail/Bolt Reading"),
      actionButton("update_summary", "Update Summary"),
      hr(),

      textInput("party_name", "Enter Party Name"),
      textInput("site_number", "Enter Site Number"),
      textInput("date", "Enter Date (YYYY-MM-DD)"),
      textInput("time", "Enter Start Time (HH:MM:SS)"),
      textInput("end_time", "Enter End Time (HH:MM:SS)"),
      selectInput("time_datum_code", "Select Time Datum Code",
                  choices = c("PDT" = "-07:00", "PST" = "-08:00", "MDT" = "-06:00", "MST" = "-07:00",
                              "CDT" = "-05:00", "CST" = "-06:00", "EDT" = "-04:00", "EST" = "-05:00",
                              "AKDT" = "-08:00", "AKST" = "-09:00", "HADT" = "-09:00", "HAST" = "-10:00")),
      hr(),

      textInput("digital_collimation_date", "Digital Collimation Test Date (YYYY-MM-DD)"),
      textInput("digital_collimation", "Digital Collimation (4 decimal places)"),

      actionButton("update_info", "Update Info"),
      hr(),

      textInput("base_object", "Starting Object Point Name"),
      textInput("base_object_elevation", "Starting Object Elevation (3 decimal places)"),
      downloadButton("download_xml", "Export to XML")
    ),
    mainPanel(
      tableOutput("data_summary"),
      tableOutput("info_data"),
      tableOutput("info")
    )
  )
)

# Define server logic
server <- function(input, output) {
  data_summary <- reactiveVal(data.frame())
  data_frames <- reactiveVal(list())
  total_error <- reactiveVal(NA)
  starting_object_elevation <- reactiveVal(NA)

  observeEvent(input$analyze, {
    req(input$file1)

    # Load the CSV file
    dat <- read.csv(input$file1$datapath, header = FALSE)

    # Extract Total Error [sft] from the metadata section
    metadata <- dat[1:20, ]  # Assuming metadata is within the first 20 rows
    total_error_row <- metadata %>%
      filter(grepl("Total Error \\[sft\\]:", V1)) %>%
      pull(V1)
    if (length(total_error_row) == 0) {
      total_error_value <- NA
    } else {
      total_error_value <- strsplit(total_error_row, ":")[[1]][2]
      total_error_value <- as.numeric(trimws(total_error_value))  # Ensure it is numeric
    }
    total_error(total_error_value)
    print(paste("Total Error [sft]:", total_error_value))  # Debugging: Print the extracted total error value

    # Identify rows with blank entries
    blank_rows <- which(apply(dat, 1, function(x) all(is.na(x) | x == "")))

    # Split the data into DataFrames
    data_frames_list <- list()
    start_idx <- 1
    for (i in seq_along(blank_rows)) {
      end_idx <- blank_rows[i] - 1
      if (end_idx >= start_idx) {
        data_frames_list[[i]] <- dat[start_idx:end_idx, ]
      }
      start_idx <- blank_rows[i] + 1
    }

    # Append remaining data after last blank row
    if (start_idx <= nrow(dat)) {
      data_frames_list[[length(data_frames_list) + 1]] <- dat[start_idx:nrow(dat), ]
    }

    # Remove completely blank columns
    remove_blank_columns <- function(df) {
      df %>% select(where(~ !all(is.na(.) | . == "")))
    }

    # Clean each DataFrame
    for (i in seq_along(data_frames_list)) {
      df <- data_frames_list[[i]]
      data_frames_list[[i]] <- remove_blank_columns(df)
    }

    # Process first DataFrame
    dat1 <- data_frames_list[[1]]
    dat1 <- dat1 %>% separate(V1, into = c("Metadata Variable Name", "Value"), sep = ":", fill = "right")

    # Set column names for each DataFrame
    for (i in seq_along(data_frames_list)) {
      df <- data_frames_list[[i]]
      colnames(df) <- as.character(unlist(df[1, ]))
      data_frames_list[[i]] <- df[-1, ]
    }

    # Identify target DataFrame
    target_column_names <- c("Point name", "Target height", "HA", "VA", "Slope distance", "Elevation", "Adjust", "Adj Z", "Reference point")
    df_to_keep <- NULL

    for (i in seq_along(data_frames_list)) {
      df <- data_frames_list[[i]]
      if (all(target_column_names %in% colnames(df))) {
        df_to_keep <- i
        break
      }
    }

    # Rename DataFrames
    info <- dat1
    data <- data_frames_list[[df_to_keep]]

    data_frames(data_frames_list)

    if (exists("data")) {
      data$`Adj Z` <- as.numeric(as.character(data$`Adj Z`))

      data <- data %>% filter(!is.na(`Adj Z`))

      if (nrow(data) > 0) {
        data_summary_value <- data %>%
          group_by(`Point name`) %>%
          summarize(
            `Average Elevation` = mean(`Adj Z`, na.rm = TRUE),
            `Standard Deviation` = round(sd(`Adj Z`, na.rm = TRUE), 3),
            `Elevation Values` = paste(format(`Adj Z`, nsmall = 3, digits = 3), collapse = ", "),
            .groups = 'drop'
          ) %>%
          mutate(`Dial/Nail/Bolt Reading` = NA, Difference = NA)

        data_summary(data_summary_value)
      } else {
        data_summary(data.frame("No valid data available after filtering."))
      }
    } else {
      data_summary(data.frame("Data frame 'data' does not exist."))
    }

    # Render tables
    output$data_summary <- renderTable({
      formatted_summary <- data_summary()
      # Format the relevant columns to 3 decimal places
      formatted_summary$Difference <- round(formatted_summary$Difference, 3)
      formatted_summary$`Average Elevation` <- round(formatted_summary$`Average Elevation`, 3)
      formatted_summary$`Standard Deviation` <- round(formatted_summary$`Standard Deviation`, 3)
      formatted_summary$`Dial/Nail/Bolt Reading` <- round(formatted_summary$`Dial/Nail/Bolt Reading`, 3)
      formatted_summary$`Standard Deviation` <- format(formatted_summary$`Standard Deviation`, nsmall = 3)
      formatted_summary$`Average Elevation` <- format(formatted_summary$`Average Elevation`, nsmall = 3)
      formatted_summary$`Dial/Nail/Bolt Reading` <- format(formatted_summary$`Dial/Nail/Bolt Reading`, nsmall = 3)
      formatted_summary$Difference <- format(formatted_summary$Difference, nsmall=3)
      formatted_summary
    })
    output$info <- renderTable(info)
  })

  observeEvent(input$update_info, {
    req(input$party_name, input$site_number, input$date, input$time, input$end_time)

    info_data <- data.frame(
      Variable = c("Party Name", "Site Number", "Date", "Start Time", "End Time",
                  "Digital Collimation Test Date", "Digital Collimation"),
      Value = c(input$party_name, input$site_number, input$date, input$time, input$end_time,
               input$digital_collimation_date, input$digital_collimation),
      stringsAsFactors = FALSE
    )
    rownames(info_data) <- info_data$Variable
    output$info_data <- renderTable(info_data)
  })

  observeEvent(input$update_summary, {
    req(input$point_name, input$hex_input)

    point_name <- input$point_name
    hex_value <- as.numeric(input$hex_input)

    current_summary <- data_summary()

    if (nrow(current_summary) > 0) {
      if (point_name %in% current_summary$`Point name`) {
        current_summary$`Dial/Nail/Bolt Reading`[current_summary$`Point name` == point_name] <- hex_value
      } else {
        new_row <- data.frame(`Point name` = point_name, `Average Elevation` = NA, `Dial/Nail/Bolt Reading` = hex_value, Difference = NA, check.names = FALSE)
        current_summary <- rbind(current_summary, new_row)
      }

      current_summary <- current_summary %>%
        mutate(Difference = as.numeric(`Average Elevation`) - `Dial/Nail/Bolt Reading`)

      data_summary(current_summary)
    }
  })

  output$download_xml <- downloadHandler(
    filename = function() {
      paste("data_summary", Sys.Date(), ".xml", sep = "")
    },
    content = function(file) {
      summary_data <- data_summary()
      base_object <- input$base_object
      base_object_elevation <- input$base_object_elevation
      site_number <- input$site_number
      party_name <- input$party_name

      # Get the time zone offset from the selection
      timezone_offset <- input$time_datum_code
      time_datum_code <- names(which(c("PDT" = "-07:00", "PST" = "-08:00", "MDT" = "-06:00",
                                      "MST" = "-07:00", "CDT" = "-05:00", "CST" = "-06:00",
                                      "EDT" = "-04:00", "EST" = "-05:00", "AKDT" = "-08:00",
                                      "AKST" = "-09:00", "HADT" = "-09:00", "HAST" = "-10:00") == timezone_offset))[1]

      # Combine date and time inputs with proper formatting
      datetime <- paste0(input$date, "T", input$time, timezone_offset)
      end_datetime <- paste0(input$date, "T", input$end_time, timezone_offset)

      # Extract Total Error [sft]
      total_error_value <- total_error()
      # Format the total error to avoid scientific notation
      if(!is.na(total_error_value)) {
        total_error_value <- format(total_error_value, scientific = FALSE, trim = TRUE)
      } else {
        total_error_value <- "N/A"
      }

      # Create base XML content without header
      xml <- xml2::xml_new_root("UsgsHydroML")

      # Add site information
      site <- xml2::xml_add_child(xml, "Site")
      xml2::xml_add_child(site, "SiteIdentifier", organizationCode = "USGS") %>%
        xml2::xml_set_text(site_number)

      # Add site data
      site_data <- xml2::xml_add_child(site, "SiteData")
      site_visit <- xml2::xml_add_child(site_data, "SiteVisit")
      xml2::xml_add_child(site_visit, "StartDateTime", timeDatumCode = time_datum_code) %>%
        xml2::xml_set_text(datetime)
      xml2::xml_add_child(site_visit, "EndDateTime", timeDatumCode = time_datum_code) %>%  # Added timeDatumCode attribute
        xml2::xml_set_text(end_datetime)
      xml2::xml_add_child(site_visit, "PartyText") %>%
        xml2::xml_set_text(party_name)
      xml2::xml_add_child(site_visit, "WeatherDescription") %>%
        xml2::xml_set_text("cloudy, cool, no precip, calm winds")

      # Add gaging station levels
      gaging_station_levels <- xml2::xml_add_child(site_visit, "GagingStationLevels")
      gaging_station_levels_circuit <- xml2::xml_add_child(gaging_station_levels, "GagingStationLevelsCircuit")
      xml2::xml_add_child(gaging_station_levels_circuit, "CircuitNumber") %>%
        xml2::xml_set_text("1")
      xml2::xml_add_child(gaging_station_levels_circuit, "AirTemperature", unitsCode = "F") %>%
        xml2::xml_set_text("10")
      xml2::xml_add_child(gaging_station_levels_circuit, "StartingObjectName") %>%
        xml2::xml_set_text(base_object)
      xml2::xml_add_child(gaging_station_levels_circuit, "ClosureError") %>%
        xml2::xml_set_text(total_error_value)
      xml2::xml_add_child(gaging_station_levels_circuit, "ClosureErrorAdjustment") %>%
        xml2::xml_set_text("0.000")

      # Add engineer level
      engineer_level <- xml2::xml_add_child(gaging_station_levels_circuit, "EngineerLevel")
      equipment <- xml2::xml_add_child(engineer_level, "Equipment")
      xml2::xml_add_child(equipment, "ManufacturerName") # This creates the empty tag
      xml2::xml_add_child(equipment, "ModelNumber")      # This creates the empty tag
      xml2::xml_add_child(equipment, "SerialNumber") %>%
        xml2::xml_set_text("9684")
      xml2::xml_add_child(engineer_level, "DigitalCollimationTestDate") %>%
        xml2::xml_set_text(input$digital_collimation_date)
      xml2::xml_add_child(engineer_level, "DigitalCollimation") %>%
        xml2::xml_set_text(ifelse(is.na(as.numeric(input$digital_collimation)), "",
                                format(as.numeric(input$digital_collimation), scientific = FALSE, nsmall=4)))

      # Add leveling rod
      leveling_rod <- xml2::xml_add_child(gaging_station_levels_circuit, "LevelingRod")
      equipment <- xml2::xml_add_child(leveling_rod, "Equipment")
      xml2::xml_add_child(equipment, "MethodCode") %>%
        xml2::xml_set_text("FGR")
      xml2::xml_add_child(equipment, "ManufacturerName") # This creates the empty tag
      xml2::xml_add_child(equipment, "ModelNumber")      # This creates the empty tag
      xml2::xml_add_child(equipment, "SerialNumber") %>%
        xml2::xml_set_text("083")
      xml2::xml_add_child(leveling_rod, "LevelingRodLevelUsed") %>%
        xml2::xml_set_text("true")
      xml2::xml_add_child(leveling_rod, "LevelingRodTaped") %>%
        xml2::xml_set_text("true")
      xml2::xml_add_child(leveling_rod, "CoefficientThermalExpansion") %>%
        xml2::xml_set_text("0.00002000")
      xml2::xml_add_child(leveling_rod, "CTECalibrationTemperature") %>%
        xml2::xml_set_text("20")

      # Add circuit summary - Use original object name (no renaming to BASE)
      circuit_summary <- xml2::xml_add_child(gaging_station_levels_circuit, "CircuitSummary")
      for (i in 1:nrow(summary_data)) {
        object <- xml2::xml_add_child(circuit_summary, "Object")
        object_name <- ifelse(is.na(summary_data$`Point name`[i]), "", as.character(summary_data$`Point name`[i]))
        xml2::xml_add_child(object, "ObjectName") %>%
          xml2::xml_set_text(object_name)
        description_node <- xml2::xml_add_child(object, "Description")
      }
      comment_node <- xml2::xml_add_child(circuit_summary, "Comment")

      # Add final summary - Use original object name (no renaming to BASE)
      final_summary <- xml2::xml_add_child(gaging_station_levels, "FinalSummary")
      for (i in 1:nrow(summary_data)) {
        object <- xml2::xml_add_child(final_summary, "Object")
        object_name <- ifelse(is.na(summary_data$`Point name`[i]), "", as.character(summary_data$`Point name`[i]))
        xml2::xml_add_child(object, "ObjectName") %>%
          xml2::xml_set_text(object_name)

        # Use the user-provided elevation for the starting object, otherwise use the calculated one
        elevation_value <- if(object_name == base_object && !is.na(base_object_elevation) && base_object_elevation != "") {
          base_object_elevation
        } else {
          ifelse(is.na(summary_data$`Average Elevation`[i]), "", as.character(summary_data$`Average Elevation`[i]))
        }

        xml2::xml_add_child(object, "ElevationFound") %>%
          xml2::xml_set_text(elevation_value)
        xml2::xml_add_child(object, "CircuitNumber") %>%
          xml2::xml_set_text("1")
      }

      # Create the correct XML header
      xml_header <- paste(
        '<?xml version="1.0" encoding="utf-8"?>',
        '<?xml-stylesheet type="text/xsl" href="http://water.usgs.gov/XML/AQ/stylesheets/AQSVMobile_SS1.xsl"?>',
        '<UsgsHydroML xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://water.usgs.gov/XML/AQ" schemaLocation="http://water.usgs.gov/XML/AQ http://water.usgs.gov/XML/AQ/schema/UsgsHydroML.xsd">',
        sep = "\n"
      )

      # Get the XML content without the default header
      xml_content <- as.character(xml)
      xml_content <- gsub('.*?<UsgsHydroML>', '', xml_content)
      xml_content <- sub('</UsgsHydroML>', '', xml_content)

      # Combine header and content
      final_xml <- paste0(xml_header, xml_content, "\n</UsgsHydroML>")

      # Clean up any extra newlines before closing tags to ensure proper formatting
      final_xml <- gsub("\n\n+</UsgsHydroML>", "\n</UsgsHydroML>", final_xml)
      final_xml <- gsub("\\s+</Site>", "\n  </Site>", final_xml)
      final_xml <- gsub("\\s+</SiteData>", "\n    </SiteData>", final_xml)

      # Fix the final XML formatting to ensure spaces in empty tags - moved earlier in the process
      final_xml <- gsub("<([^>]+)/>", "<\\1 />", final_xml)

      # Write to file
      writeLines(final_xml, file, useBytes = TRUE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)