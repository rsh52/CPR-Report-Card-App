#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Zoll Report Card"),
  
  sidebarPanel(
    #Selector for file upload
    fileInput('datafile', 'Choose CSV file',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    # Age input
    numericInput(inputId = "ageinput", label = "Enter Patient Age", value = 0, min = 0, max = 17.9, step = NA)
  ),
  mainPanel(
    # Shows first 10 rows of input data set in table form
    tableOutput("filetable"),
    tableOutput("AvgDepthRate")
  )
))
