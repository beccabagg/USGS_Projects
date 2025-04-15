QRev MS Unit Converter
Overview
QRev MS Unit Converter is a Shiny web application that converts discharge measurements from metric units (m³/s) to customary units (ft³/s). The application is designed to process Excel (.xlsx) files containing QRev discharge measurements.

Features
Simple web interface for uploading Excel files
Automatic conversion of discharge measurements from m³/s to ft³/s
Preview of converted data
Download capability for the converted data as an Excel file
Requirements
The application requires the following R packages:

shiny
readxl
writexl
dplyr
These packages will be automatically installed if they are not already present.

Usage
Run the QRev_MS_Unit_Converter.R script in R or RStudio
Upload an Excel file containing discharge measurements with a column named 'Q' (in m³/s) and 'Start Time'
Preview the converted data in the application
Click the "Download Converted File" button to save the converted data
Output
The output Excel file will contain:

Start Time (unchanged)
Q_cfs (discharge in cubic feet per second)
Conversion Formula
Discharge is converted using the following formula: Q (ft³/s) = Q (m³/s) × 35.3147