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
    uploaddata$FixedTime <- uploaddata$Second + uploaddata$Milisecond/1000
    uploaddata$DepthCm <- uploaddata$DepthIn*2.54
    uploaddata$DepthIn <- NULL
    uploaddata$Second <- NULL
    uploaddata$Milisecond <- NULL
    uploaddata <- uploaddata[c(4,5,1,2,3)]
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

    Zoll_Depth_Total_Average <- round(mean(Zoll_Data$DepthCm, na.rm = TRUE), digits = 1) #Average Depth for the event
    Zoll_Rate_Total_Average <- round(mean(Zoll_Data$RateCPM, na.rm = TRUE), digits = 1) #Average Rate for the event
    ADRframe <- data.frame(Zoll_Depth_Total_Average, Zoll_Rate_Total_Average)

    AvgDp <- print(paste0(round(Zoll_Depth_Total_Average, digits = 4)))
    AvgRp <- print(paste0(round(Zoll_Rate_Total_Average, digits = 4)))
    FillVars <- c("Depth (cm)", "Rate (cpm)")
    ADRframe <- data.frame(FillVars, c(AvgDp, AvgRp))
    colnames(ADRframe) <- c("Event Average Depth & Rate","")
    ADRframe
  })
  
  #Outputs the average depth and rate
  output$AvgDepthRate <- renderTable({
    if (input$datafile == 0) return(NULL)
    AvgDepthRate()
  })
  
  CCF <- reactive({
    Zoll_Data <- filedata()
    
    for (i in 1:nrow(Zoll_Data)) {
      Zoll_Data$Pause[i] <- ifelse(
        (Zoll_Data$FixedTime[i+1] - Zoll_Data$FixedTime[i]) < 1, "In range", "Pause")
    }
    
    
    Zoll_CCF_Time_In <- 0 # Initialize new variable to test time in compressions
    
    for(i in 1:nrow(Zoll_Data)) {
      ifelse((Zoll_Data$FixedTime[i+1] - Zoll_Data$FixedTime[i]) <1, 
             Zoll_CCF_Time_In[i] <- Zoll_Data$FixedTime[i+1] - Zoll_Data$FixedTime[i],
             Zoll_CCF_Time_In[i] <-  0)
    }
    
    Zoll_CCF_Total <- sum(Zoll_CCF_Time_In)/(max(Zoll_Data$FixedTime) - 
                                               min(Zoll_Data$FixedTime))
    
    Time_Out_CCs <- (max(Zoll_Data$FixedTime) - sum(Zoll_CCF_Time_In))/
      (max(Zoll_Data$FixedTime) - min(Zoll_Data$FixedTime))
    
    Total_Number_CCs <- nrow(subset(Zoll_Data, Zoll_Data$Valid == "Valid"))
    
    Minute_Time <- max(Zoll_Data$FixedTime)/60
    
    CCFp <- print(paste0(round(100*Zoll_CCF_Total, digits = 2), "%"))
    CCFo <- print(paste0(round(100*Time_Out_CCs, digits = 2), "%"))
    
    CCn <- print(paste0(Total_Number_CCs))
    CCt <- print(paste0(round(Minute_Time, digits = 2)))
    
    CCFlabels <- c("CCF (%)", "(%) Not in CCs", "Total CCs (n)", "Total Time of Event (min)")
    
    CCFdf <- data.frame(CCFlabels, c(CCFp, CCFo, CCn, CCt))
    colnames(CCFdf) <- c("Event CCF Metrics", "")
    CCFdf
                 
  })
  
  #Outputs the average depth and rate
  output$CCF <- renderTable({
    if (input$datafile == 0) return(NULL)
    CCF()
  })
  
  
  DepthPlot <- reactive({
    Zoll_Data <- filedata() 
    
    if(input$ageinput <1){
      targetDh <- geom_hline(yintercept = 4, colour = "red", linetype = 4, size = 1)
      targetDl <- geom_hline(yintercept = 3.3, colour = "blue", linetype = 2, size = 1)
      targetDb <- geom_rect(aes(xmin = min(Zoll_Data$FixedTime), 
                                xmax = max(Zoll_Data$FixedTime), ymin = 3.3, ymax = 4), 
                            fill = "seagreen3", alpha = 0.01)
    } else if(input$ageinput >= 1 & input$ageinput <8){
      targetDh <- geom_hline(yintercept = 5, colour = "red", linetype = 4, size = 1)
      targetDl <- geom_hline(yintercept = 4.4, colour = "blue", linetype = 2, size = 1)
      targetDb <- geom_rect(aes(xmin = min(Zoll_Data$FixedTime), 
                                xmax = max(Zoll_Data$FixedTime), ymin = 4.4, ymax = 5), 
                            fill = "seagreen3", alpha = 0.01)
    } else if(input$ageinput >= 8 & input$ageinput <18){
      targetDh <- geom_hline(yintercept = 5, colour = "red", linetype = 4, size = 1)
      targetDl <- geom_hline(yintercept = 6, colour = "red", linetype = 4, size = 1)
      targetDb <- geom_rect(aes(xmin = min(Zoll_Data$FixedTime), 
                                xmax = max(Zoll_Data$FixedTime), ymin = 5, ymax = 6), 
                            fill = "seagreen3", alpha = 0.01)
    } else{
      print("Inelligible")
    }
    
    
    Depth_Plot <- ggplot(Zoll_Data, 
                         aes(x = FixedTime, y = DepthCm)) +
      theme_bw() +
      geom_bar(stat = "identity", width = 0.1, color = "blue") +
      xlab("Time (s)") + ylab("Depth (cm)") +
      targetDh + targetDl + targetDb + theme(axis.line = element_line(size = 1, colour = "black")) +
      theme(axis.line = element_line(arrow = arrow()))
    
    ggplotly(Depth_Plot)
    
  })
  
  #Outputs the average depth and rate
  output$DepthPlot <- renderPlotly({
    if (input$datafile == 0) return(NULL)
    DepthPlot()
  })
  
  
  RatePlot <- reactive({
    Zoll_Data <- filedata() 
    
    targetRh <- geom_hline(yintercept = 120, colour = "red", linetype = 4, size = 1)
    targetRl <- geom_hline(yintercept = 100, colour = "red", linetype = 4, size = 1)
    targetRb <- geom_rect(aes(xmin = min(Zoll_Data$FixedTime), 
                              xmax = max(Zoll_Data$FixedTime), ymin = 100, ymax = 120), 
                          fill = "seagreen3", alpha = 0.01)
    
    Rate_Plot <- ggplot(Zoll_Data, 
                        aes(x = FixedTime, y = RateCPM)) +
      theme_bw() +
      geom_point(stat = "identity", color = "blue", size = 1) +
      xlab("Time (s)") + ylab("Rate (cpm)") +
      targetRh + targetRl + targetRb + theme(axis.line = element_line(size = 1, colour = "black")) +
      theme(axis.line = element_line(arrow = arrow()))
    
    ggplotly(Rate_Plot)
    
  })
  
  #Outputs the average depth and rate
  output$RatePlot <- renderPlotly({
    if (input$datafile == 0) return(NULL)
    RatePlot()
  })
  

})
