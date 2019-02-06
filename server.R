library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  #This function is responsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    uploaddata <- read.csv(infile$datapath, skipNul = TRUE)
    colnames(uploaddata) <- c("Second", "Milisecond", "DepthIn", "RateCPM", "RVMMS",
                        "Valid")
    uploaddata
  })
  
  #This previews the CSV data file through the Zoll_Data frame
  output$filetable <- renderTable({
    if (input$datafile == 0) return(NULL)
    head(filedata())
  })
  
  
# The following lines read in the CSV file, rename the columns, adjusts the units,
# and create a data frame with average depth and rate for the event.
  AvgDepthRate <- reactive({
    Zoll_Data <- filedata()
    colnames(Zoll_Data) <- c("Second", "Milisecond", "DepthIn", "RateCPM", "RVMMS",
                             "Valid")
    Zoll_Data$FixedTime <- Zoll_Data$Second + Zoll_Data$Milisecond/1000
    Zoll_Data$DepthCm <- Zoll_Data$DepthIn*2.54
    
    Zoll_Depth_Total_Average <- round(mean(Zoll_Data$DepthCm, na.rm = TRUE), digits = 1) #Average Depth for the event
    Zoll_Rate_Total_Average <- round(mean(Zoll_Data$RateCPM, na.rm = TRUE), digits = 1) #Average Rate for the event
    ADRframe <- data.frame(Zoll_Depth_Total_Average, Zoll_Rate_Total_Average)
    colnames(ADRframe) <- c("Average Depth (cm)", "Average Rate (cpm)")
    ADRframe
  })
  

  #Outputs the average depth and rate
  output$AvgDepthRate <- renderTable({
    if (input$datafile == 0) return(NULL)
    AvgDepthRate()
  })
  
  
  
  
})
