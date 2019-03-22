# This is the UI script for the CPR Report Card App. 
# The purpose of this script is to designate the locations, content, and set up
# for all interactive and reactive components of the App.

library(shiny)

ui <- fluidPage(
  headerPanel("CPR Report Card"),
  
  sidebarPanel(
    #Selector for file upload - MUST be an appropriately formatted .CSV
    fileInput('datafile', 'Choose CSV file',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    # Age input - Defines criteria for depth guidelines
    numericInput(inputId = "ageinput", label = "Enter Patient Age", value = 0, min = 0, max = 17.9, step = NA),
    #Download Button - Currently exports toa PDF file
    Sys.setenv(PATH = paste("/var/texlive/2018/bin/x86_64-linux", Sys.getenv("PATH"), sep=.Platform$path.sep)),
    downloadButton("report", "Generate report"),
    width = 3
    
  ),

    column(12,
       "",
       fluidRow(
         column(width = 3,
                tableOutput("AvgDepthRate")),
         column(width = 3,
                tableOutput("PercDepthRate")),
         column(width = 3,
                tableOutput("CCF")),
         column(width = 3,
                plotOutput("CCFPiePlot"))
       )), fluidRow(
         column(width = 12,
                plotOutput("DepthPlot"))
       ), fluidRow(
         column(width = 12,
                plotOutput("RatePlot"))
       )
)
