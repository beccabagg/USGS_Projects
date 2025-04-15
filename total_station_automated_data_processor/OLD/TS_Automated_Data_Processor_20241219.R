# This script analyzes total station data to extract the summary metadata data frames
# It requires a CSV to be uploaded by providing the local path to the file on your system.
# Set the working directory to your local desktop at the top of the script.

# ----------------------------------------------------------------------------
# NOTE: This script requires 2 elevation values for each location.
# Additional values should be removed from the CSV prior to importing.
# Code by Rebecca Baggott, contact rbaggott@usgs.gov for questions
# ----------------------------------------------------------------------------

# Load required libraries
packages <- c("dplyr", "tidyr", "xml2", "shiny")
for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# Define UI for the application
ui <- fluidPage(
  titlePanel("Automated Total Station Data Analyzer"),
  
  p("Welcome to the Automated Total Station Data Analyzer web application! This tool analyzes CSV data from total stations, specifically Trimble S7 with TSC5. Please upload a total station CSV file by entering the local path."),
  
  p(tags$strong("Important Requirements:")),
  p("Replicate point names must be identical in spelling and case throughout the dataset. Correct any errors before import."),
  
  p(tags$strong("Entering Dial/Nail/Bolt Readings:")),
  p("Enrich the summary data by entering Dial/Nail/Bolt readings. Input the values and click 'Update Summary' to include them in the analysis. Similarly, input party name, site name, site number, and date, then select 'Update Info' to save these details. To correct any input errors, select 'Analyze Data' to refresh and remove entered values."),
  p(tags$strong("Troubleshooting:")),
    p("If the code fails to run, open the selected CSV, and select all cells. Select Home --> Format --> Number (with 4 decimal places)."),
  p(tags$em("Developed by Rebecca Baggott. For assistance, contact rbaggott@usgs.gov.")),
  p(tags$em("Export to XML options coming soon!")),
  
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
      textInput("site_name", "Enter Site Name"),
      textInput("site_number", "Enter Site Number"),
      textInput("date", "Enter Date"),
      actionButton("update_info", "Update Info")
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
  observeEvent(input$analyze, {
    req(input$file1)
    
    # Load the CSV file
    dat <- read.csv(input$file1$datapath, header = FALSE)
    
    # Identify rows with blank entries
    blank_rows <- which(apply(dat, 1, function(x) all(is.na(x) | x == "")))
    
    # Split the data into DataFrames
    data_frames <- list()
    start_idx <- 1
    for (i in seq_along(blank_rows)) {
      end_idx <- blank_rows[i] - 1
      if (end_idx >= start_idx) {
        data_frames[[i]] <- dat[start_idx:end_idx, ]
      }
      start_idx <- blank_rows[i] + 1
    }
    
    # Append remaining data after last blank row
    if (start_idx <= nrow(dat)) {
      data_frames[[length(data_frames) + 1]] <- dat[start_idx:nrow(dat), ]
    }
    
    # Remove completely blank columns
    remove_blank_columns <- function(df) {
      df %>% select(where(~ !all(is.na(.) | . == "")))
    }
    
    # Clean each DataFrame
    for (i in seq_along(data_frames)) {
      df <- data_frames[[i]]
      data_frames[[i]] <- remove_blank_columns(df)
    }
    
    # Process first DataFrame
    dat1 <- data_frames[[1]]
    dat1 <- dat1 %>% separate(V1, into = c("Metadata Variable Name", "Value"), sep = ":", fill = "right")
    
    # Set column names for each DataFrame
    for (i in seq_along(data_frames)) {
      df <- data_frames[[i]]
      colnames(df) <- as.character(unlist(df[1, ]))
      data_frames[[i]] <- df[-1, ]
    }
    
    # Identify target DataFrame
    target_column_names <- c("Point name", "Target height", "HA", "VA", "Slope distance", "Elevation", "Adjust", "Adj Z", "Reference point")
    df_to_keep <- NULL
    
    for (i in seq_along(data_frames)) {
      df <- data_frames[[i]]
      if (all(target_column_names %in% colnames(df))) {
        df_to_keep <- i
        break
      }
    }
    
    # Rename DataFrames
    info <- dat1
    data <- data_frames[[df_to_keep]]
    
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
    req(input$party_name, input$site_name, input$site_number, input$date)
    
    info_data <- data.frame(
      Variable = c("Party Name", "Site Name", "Site Number", "Date"),
      Value = c(input$party_name, input$site_name, input$site_number, input$date),
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
}

# Run the application 
shinyApp(ui = ui, server = server)