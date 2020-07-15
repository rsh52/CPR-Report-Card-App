# CPR Report Card App

## Background
The pediRES-Q CPR Report Card Application is designed to assist users of Zoll defibrilators with data processing and snapshot analytics. Using the freely available [Zoll Rescue Net Code Review](https://www.zoll.com/medical-products/data-management/rescue-net-fire-ems/code-review/) software you can export your data to a .csv format for upload to the application.

The application is freely available at: https://chopccm.shinyapps.io/CPR-Report-Card-App/

A handy SOP/Instructions on how to interact with the application and pre-process data is available for download on the app. 

The application also comes ready with a de-identified compression file for testing.

## Data Calculation Methodoloy
Only observations where validity flags are equal to 1 are included in the final calculations.

**Depth** is calculated from the remainder per each depth recording.
- For <1 year olds, target guidelines are >= 3.4 cm.
- For 1 - <8 year olds, target guidelines are >= 4.4 cm.
- For 8 - <18 year olds, target guidelines are between 5 and 6 cm.
*In accordance with the pediRES-Q Landscape Paper by Niles et al.: [https://pubmed.ncbi.nlm.nih.gov/29533355/](https://pubmed.ncbi.nlm.nih.gov/29533355/)

**Rate** is calculated from the remainder per each rate recording. The defib calculates the time between compressions and returns a value as compressions per minute.
- Rate target guidelines are considered in compliance between 100 and 120 cpm.

**Release Velocity** target guidelines are greater than 400 mm/s.

**Chest Compression Fraction (CCF)** is calculated using the time spent in compressions divided by the entire time spent in the event. To eliminate intermittent ROSC periods the following assumptions are made:
- If the time difference between two compressions is less than 2 seconds then the period of time is considered CPR.
- If the time difference between two compressions is greater than or equal to 2 seconds and less than 120 seconds then the period of time is considered a Pause.
- If the time difference between two compressions is greater than or equal to 120 seconds then the period of time is considered ROSC and does not negatively impact the calculation (functionally treated as compressions).

## Epoch Calculation Method
To calculate epoch averages, all time stamps are shifted up to zero and rounded to the nearest whole second. Averages are then taken (using means) of compressions in between seconds and interpolations are inserted with blank measurements for seconds that are missing. A rolling window is then applied aggregating the CPR measurements as the user dictates above. Epochs are defined in seconds of time.

### Disclaimer
This application is intended for QI and debriefing purposes only, it is not meant to dictate practices or clinical research.
