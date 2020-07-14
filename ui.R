library(shiny)
library(shinythemes)
library(plotly)
library(highcharter)
library(tidyverse)
library(zoo)
library(kableExtra)

ui <- fluidPage(
  theme = shinytheme("united"),
  # shinythemes::themeSelector(),
  headerPanel("CPR Report Card Application"),
  
  column(12,
         sidebarPanel(
           downloadButton("downloadSOP", "Download Instructions"),
           downloadButton("downloadCCs", "Download Sample Compression File"),
           fileInput('datafile', 'Choose CSV file',
                     accept=c('text/csv', 'text/comma-separated-values,text/plain')),
           # Age input - Defines criteria for depth guidelines
           numericInput(inputId = "ageinput", label = "Enter Patient Age", value = 0, min = 0, max = 17.9, step = NA),
           #Download Button - Currently exports to a PDF file
           # Sys.setenv(PATH = paste("/var/texlive/2018/bin/x86_64-linux", Sys.getenv("PATH"), sep=.Platform$path.sep)),
           # downloadButton("report", "Generate report"),
           radioButtons(inputId = "epochfx", label = "Choose Epoch Eval Fx.", selected = "mean", choices = c("mean", "median")),
           numericInput(inputId = "epochwin", label = "Input Epoch Window Interval (seconds)", value = 30, min = 5),
           width = 12
           
         )
  ),
  
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel(title = "Data",
               fluidRow(
                 column(width = 4,
                        htmlOutput("AvgTbl")),
                 column(width = 4,
                        htmlOutput("EpochEval")),
                 column(width = 4,
                        highchartOutput("CCFPiePlot"))
               ),
               
               column(12,
                      "",
                      fluidRow(
                        column(width = 12,
                               highchartOutput("DepthHC"))
                      ), fluidRow(
                        column(width = 12,
                               highchartOutput("RateHC"))
                      )
               )
      ),
      tabPanel(
        title = "User Information",
        h1("User Information", align = "center", style = "color:#139ed1"),
        h3("Introduction", align = "left", style = "color:#139ed1"),
        p("The CPR Report Card Application is a useful tool for obtaining calculations and visualizations of compression data outputs from Zoll defibrilators. On the sidebar panel above are example files for how to obtain a processable file and an example file for use."),
        h3("Data Calculation Methodoloy", align = "left", style = "color:#139ed1"),
        p("Only observations where validity flags are equal to 1 are included in the final calculations."),
        p(span("Depth", style = "color:red"), " is calculated from the remainder per each depth recording."),
        p("   ", span("- For <1 year olds, target guidelines are >= 3.4 cm.")),
        p("   ", span("- For 1 - <8 year olds, target guidelines are >= 4.4 cm.")),
        p("   ", span("- For 8 - <18 year olds, target guidelines are between 5 and 6 cm.")),
        br(),
        p("   ", span("*In accordance with the pediRES-Q Landscape Paper by Niles et al.:", style = "color: red")),
        a("https://pubmed.ncbi.nlm.nih.gov/29533355/"),
        br(),
        br(),
        p(span("Rate", style = "color:red"), " is calculated from the remainder per each rate recording. The defib calculates the time between compressions and returns a value as compressions per minute."),
        p("   ", span("- Rate target guidelines are considered in compliance between 100 and 120 cpm.")),
        p(span("CCF", style = "color:red"), " is calculated using the time spent in compressions divided by the entire time spent in the event. To eliminate intermittent ROSC periods the following assumptions are made: "),
        p("   ", span("- If the time difference between two compressions is less than 2 seconds then the period of time is considered CPR.")),
        p("   ", span("- If the time difference between two compressions is greater than or equal to 2 seconds and less than 120 seconds then the period of time is considered a Pause.")),
        p("   ", span("- If the time difference between two compressions is greater than or equal to 120 seconds then the period of time is considered ROSC and does not negatively impact the calculation (functionally treated as compressions).")),
        h4("Epoch Calculations", align = "left", style = "color:#139ed1"),
        p("To calculate epoch averages, all time stamps are shifted up to zero and rounded to the nearest whole second. Averages are then taken (using means) of compressions in between seconds and interpolations are inserted with blank measurements for seconds that are missing. A rolling window is then applied aggregating the CPR measurements as the user dictates above."),
        p("Epochs are defined in seconds of time.")
        )
      
    )
    
  )
  
)
