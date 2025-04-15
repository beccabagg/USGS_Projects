if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

library(shiny)
library(readxl)
library(writexl)
library(dplyr)

# Conversion functions
convert_units <- function(df) {
  # Create new dataframe with only needed columns
  result_df <- data.frame(
    "Start Time" = df$`Start Time`,
    "Q_cfs" = df$Q * 35.3147
  )
  
  return(result_df)
}

ui <- fluidPage(
  titlePanel("Metric to Customary Unit Converter"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload .xlsx File", accept = ".xlsx"),
      helpText("Upload an Excel file with metric units.",
               "The output will contain:",
               "- Start Time",
               "- Q (converted from m³/s to ft³/s)"),
      downloadButton("download", "Download Converted File")
    ),
    mainPanel(
      tableOutput("preview")
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    req(input$file)
    read_excel(input$file$datapath)
  })
  
  converted_data <- reactive({
    req(data())
    convert_units(data())
  })
  
  output$preview <- renderTable({
    head(converted_data())
  })
  
  output$download <- downloadHandler(
    filename = function() { "converted_file.xlsx" },
    content = function(file) {
      write_xlsx(converted_data(), file)
    }
  )
}

shinyApp(ui, server)
