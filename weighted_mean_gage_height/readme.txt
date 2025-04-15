# ================================================================
# README: Weighted Mean Gage Height Calculator
# ================================================================
#
# OVERVIEW:
# ---------
# This R Shiny application calculates weighted mean gage heights using
# time-discharge data. It provides two different weighting methods:
#   1. Time-weighted - where each gage height is weighted by time duration
#   2. Volume-weighted - where each gage height is weighted by water volume
#   3. Combined - the average of both methods
#
# REQUIREMENTS:
# ------------
# Required R packages:
#   - shiny
#   - DT
#   - dplyr
#   - lubridate
#   - shinyjs
# (The application will automatically attempt to install missing packages)
#
# USAGE INSTRUCTIONS:
# ------------------
# 1. INPUT DATA:
#    - Paste time-discharge data in military format (e.g., 1400) and discharge values
#    - Data should have two columns: Time and Discharge
#    - Time should be in military format (e.g., 1430)
#    - Discharge should be numeric values
#    - Click "Load Time-Discharge Data" to process the input
#
# 2. ENTER GAGE HEIGHTS:
#    - Click "Generate Gage Height Inputs" to create input fields
#    - Optional: Set custom start/end times for gage heights
#    - Enter gage height values for each time point:
#      * The 15-minute interval before start time
#      * The exact start time
#      * Every 15-minute interval between start and end
#      * The exact end time
#      * The 15-minute interval after end time
#    - Click "Save Gage Heights" to store your entered values
#
# 3. CALCULATE RESULTS:
#    - Click "Calculate Weighted Means" to perform calculations
#    - View results in the three results tabs:
#      * Time-Weighted Results
#      * Volume-Weighted Results
#      * Combined Results
#    - Download results with the "Download Results" button
#
# OUTPUT FILES:
# ------------
# When downloading results, the application will create:
#   - A main CSV file with summary results
#   - Time-weighted calculation details (CSV)
#   - Volume-weighted calculation details (CSV)
#   - A comprehensive text report with all calculations
#
# CALCULATIONS:
# ------------
# 1. Time-Weighted Method:
#    - Each gage height is weighted by the time duration it represents
#    - Formula: Σ(gage × time) / Σ(time)
#
# 2. Volume-Weighted Method:
#    - Each gage height is weighted by the water volume (discharge × time)
#    - Formula: Σ(gage × volume) / Σ(volume)
#
# 3. Combined Method:
#    - Simple average of both methods
#    - Formula: (time-weighted + volume-weighted) / 2
#
# AUTHOR:
# -------
# Created by: [Your Name]
# Last Updated: [Date]
#
# ================================================================