Rain Forecaster Script for Santa Cruz
Overview
This R script retrieves and visualizes precipitation forecasts for Santa Cruz, California. It features separate forecasts for day (5AM-5PM) and night (6PM-4AM) periods, providing a detailed view of expected rainfall patterns over the next 7 days.

Features
Web scraping of precipitation forecast data from the California Nevada River Forecast Center (CNRFC)
Fallback to mock data generation if web scraping fails
Tabular display of precipitation forecasts with day/night separation
Visual representation using ggplot2:
Bar charts showing day and night precipitation amounts
Daily totals clearly labeled
Color-coded visualization (day vs night periods)
Summary statistics of daily precipitation totals
Requirements
The script requires the following R packages:

rvest (web scraping)
dplyr (data manipulation)
lubridate (date handling)
ggplot2 (visualization)
stringr (string manipulation)
tidyr (data tidying)
These packages will be automatically installed if not already present.

Usage
Ensure you have R installed on your system
Save the script as Rain_Forecaster.R
Run the script in R or RStudio:
Or run directly from the command line:

Output
The script generates:

A tabular display of forecasted precipitation by date and period (day/night)
A bar chart visualization of the forecasted precipitation
A summary table of daily precipitation totals
Functions
get_santa_cruz_rain_forecast(): Retrieves or generates precipitation forecast data
generate_forecast_table(): Creates formatted tables and visualizations of the forecast data
Note
If the web scraping fails due to website structure changes or connectivity issues, the script will automatically fall back to generating sample forecast data to demonstrate functionality.