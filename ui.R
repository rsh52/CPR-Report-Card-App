#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# library(shiny)
# 
# shinyUI(pageWithSidebar(
#   headerPanel("Zoll Report Card"),
#   
#   sidebarPanel(
#     #Selector for file upload
#     fileInput('datafile', 'Choose CSV file',
#               accept=c('text/csv', 'text/comma-separated-values,text/plain')),
#     # Age input
#     numericInput(inputId = "ageinput", label = "Enter Patient Age", value = 0, min = 0, max = 17.9, step = NA)
#   ),
#   mainPanel(
#     # Shows first 10 rows of input data set in table form
#     tableOutput("filetable"),
#     tableOutput("AvgDepthRate"),
#     tableOutput("CCF"),
#     plotOutput("DepthPlot")
#   )
# ))
# 
library(shiny)

ui <- fluidPage(
  headerPanel("CPR Report Card"),
  
  sidebarPanel(
    #Selector for file upload
    fileInput('datafile', 'Choose CSV file',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    # Age input
    numericInput(inputId = "ageinput", label = "Enter Patient Age", value = 0, min = 0, max = 17.9, step = NA),
    #Download Button
    downloadButton("report", "Generate report"),
    width = 3
    
  ),

    column(12,
       "",
       fluidRow(
         column(width = 3,
                tableOutput("AvgDepthRate")),
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
