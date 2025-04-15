## Time of Travel Calculator

OVERVIEW
--------
This R Shiny application calculates the travel time for water flow 
based on distance and velocity inputs. It provides tools for:
- Converting distances between miles and feet
- Calculating travel time using the formula: 
  ((distance in feet / (velocity × 1.3)) / 60)
- Adjusting measurement times by adding the calculated travel time
- Exporting calculation details and adjusted times to CSV files

INSTALLATION REQUIREMENTS
------------------------
Required packages:
- shiny
- DT

To install these packages, run:
> install.packages("shiny")
> install.packages("DT")

HOW TO USE
----------
1. BASIC INFORMATION
   Fill in the site details:
   - Site Name
   - Computation By (who performed the calculation)
   - Measurement Number
   - Date

2. TRAVEL TIME CALCULATION
   - Enter the distance (in miles or feet)
   - Select the appropriate distance unit
   - Enter the average velocity in feet per second
   - Click "Calculate Travel Time"
   - Review the calculation results in the "Results" tab

3. TIME ADJUSTMENT
   - Navigate to the "Time Adjustment" tab
   - Copy data directly from Excel (two columns only):
     * First column: Time in military format (HH:MM)
     * Second column: Section discharge (any numeric value)
   - Paste the data into the text area (no headers needed)
   - Click "Process Time Data"
   - Review the adjusted times in the table below

4. DATA EXPORT
   Click "Export Complete Data" to download a CSV file containing:
   - A summary of the calculation details
   - The full calculation data
   - The time adjustment table (if available)

   The exported file will be named: TOT_YYYYMMDD_SiteName.csv

CALCULATION FORMULA
------------------
Travel Time (minutes) = ((distance in feet / (velocity × 1.3)) / 60)

NOTES
-----
- No headers are needed when pasting measurement data
- Times should be in 24-hour format (HH:MM)
- The application automatically converts miles to feet (1 mile = 5280 feet)
- Discharge values can be in any units
- All calculations are saved to the data table for reference
- Time adjustments can be recalculated as needed

TROUBLESHOOTING
--------------
- If the export button doesn't produce a file, ensure you've performed 
  at least one calculation
- For time adjustments, ensure the travel time has been calculated first
- Times must be in HH:MM format (24-hour)
- If pasted data isn't recognized correctly, check for extra spaces or formatting

EXAMPLE USAGE
------------
1. Enter site information: "River Crossing", "John Doe", "M-2023-15", "2023-06-15"
2. Calculate travel time: 2.5 miles, 3.2 feet/second → 16.9 minutes
3. Paste measurement times and discharges
4. Process time data to get adjusted measurement times
5. Export the complete dataset for documentation