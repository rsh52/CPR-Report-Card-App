# The server.r script is responsible for all of the computation and coding
# that makes the widgets, plots, and analytics of the app work. 

# Especially important to note is that the downloadHandler is dependent on the 
# report.Rmd file. The rmd serves to make the framework for the pdf that the 
# downloadHandler feeds the information into. 

shinyServer(function(input, output) {
  ##Load Libraries--------------------------------------------------------------
  library(ggplot2) # Grammar of Graphics Library
  library(plotly) # Addition of Plotly interactive displays if desired
  library(dplyr) # Allows for grammar and piping ( %>% )
  library(kableExtra) # Allows for interactive html tables
  library(knitr) # Allows for interactive html tables
  
# File Data Reactive & Output---------------------------------------------------   
  # This function is responsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    uploaddata <- read.csv(infile$datapath, skipNul = TRUE)
    colnames(uploaddata) <- c("Second", "Milisecond", "DepthIn", "RateCPM", "RVMMS",
                        "Valid")
    # Convert time into one column of seconds + miliseconds
    uploaddata$FixedTime <- uploaddata$Second + uploaddata$Milisecond/1000
    # Convert depth in inches column to new depth in cm column
    uploaddata$DepthCm <- uploaddata$DepthIn*2.54
    # Remove inches, second, and milisecond columns
    uploaddata$DepthIn <- NULL
    uploaddata$Second <- NULL
    uploaddata$Milisecond <- NULL
    # Shift order in case display of data is desired
    uploaddata <- uploaddata[c(4,5,1,2,3)]
    uploaddata
  })
  
  #This previews the CSV data file through the Zoll_Data frame
  output$filetable <- renderTable({
    if (input$datafile == 0) return(NULL)
    head(filedata())
  })

# Avg Depth & Rate Tbl---------------------------------------------------------- 
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
    
    # Uncomment below if you want interactive html, NOTE YOU WILL NOT BE ABLE TO GENERATE PDF
    
    # ADRframe <-  ADRframe %>% 
    #   kable("html") %>%
    #   kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)
    
    ADRframe
  })
  
  #Outputs the average depth and rate
  # Note that validate and need commands control initial blank error messages
  output$AvgDepthRate <- renderTable({
    validate(
      need(input$datafile != "", "Please Upload a Dataset"),
      need(input$ageinput != "", "Please Select an Age")
    )
    if (input$datafile == 0) return(NULL)
    AvgDepthRate()
  })

# CCF Tbl ----------------------------------------------------------------------
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
    
    Time_Out_CCs <- ((max(Zoll_Data$FixedTime) - min(Zoll_Data$FixedTime)) - sum(Zoll_CCF_Time_In))/
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
    
    # CCFdf <- CCFdf %>%
    #   kable("html") %>%
    #   kable_styling(bootstrap_options = c("striped", "hover"), full_width = F)
    
    CCFdf
                 
  })
  
  #Outputs the average depth and rate
  output$CCF <- renderTable({
    validate(
      need(input$datafile != "", ""),
      need(input$ageinput != "", "")
    )
    if (input$datafile == 0) return(NULL)
    CCF()
  })
  
## CCF Pie Plot ----------------------------------------------------------------
  CCFPiePlot <- reactive({
    
    Zoll_Data <- filedata()
    
    ##Same as CCF Code 
    
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
    
    Time_Out_CCs <- ((max(Zoll_Data$FixedTime) - min(Zoll_Data$FixedTime)) - sum(Zoll_CCF_Time_In))/
      (max(Zoll_Data$FixedTime) - min(Zoll_Data$FixedTime))
    
    Total_Number_CCs <- nrow(subset(Zoll_Data, Zoll_Data$Valid == "Valid"))
    
    Minute_Time <- max(Zoll_Data$FixedTime)/60
    
    CCFp <- round(100*Zoll_CCF_Total, digits = 2)
    CCFo <- round(100*Time_Out_CCs, digits = 2)
    
    CCFPlotDF <- data.frame(c("In CC", "Out CC"), c(CCFp, CCFo))
    colnames(CCFPlotDF) <- c("Var", "Val")
    
    
    #Pie Plot Addition
    
    blank_theme <- theme_minimal()+
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid=element_blank(),
        axis.ticks = element_blank(),
        plot.title=element_text(size=14, face="bold")
      )
    
    CCF_Pie <- ggplot(CCFPlotDF, aes(x = "", y = CCFPlotDF$Val, fill= CCFPlotDF$Var)) +
      geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) +
      blank_theme +
      scale_fill_manual(values = c("In CC" = "#30AC30", "Out CC" = "#DB2E2E")) +
      theme(axis.text.x=element_blank(), plot.title = element_text(hjust = 0.5, size = 18)) +
      labs(title = "CCF For Event") + theme(legend.position="none")
    
    CCF_Pie
    
  })
  
  #Outputs the average depth and rate
  output$CCFPiePlot <- renderPlot({
    validate(
      need(input$datafile != "", ""),
      need(input$ageinput != "", "")
    )
    if (input$datafile == 0) return(NULL)
    CCFPiePlot()
  })
  
## Depth Plot ------------------------------------------------------------------
  DepthPlot <- reactive({
    Zoll_Data <- filedata() 
    
    if(input$ageinput <1){
      targetDh <- geom_hline(yintercept = 4, colour = "red", linetype = 4, size = 1)
      targetDl <- geom_hline(yintercept = 3.3, colour = "blue", linetype = 2, size = 1)
      targetDb <- geom_rect(aes(xmin = min(Zoll_Data$FixedTime), 
                                xmax = max(Zoll_Data$FixedTime), ymin = 3.3, ymax = 4), 
                            fill = "seagreen3", alpha = 0.01)
      targetDhi <- 4
      targetDli <- 3.3
    } else if(input$ageinput >= 1 & input$ageinput <8){
      targetDh <- geom_hline(yintercept = 5, colour = "red", linetype = 4, size = 1)
      targetDl <- geom_hline(yintercept = 4.4, colour = "blue", linetype = 2, size = 1)
      targetDb <- geom_rect(aes(xmin = min(Zoll_Data$FixedTime), 
                                xmax = max(Zoll_Data$FixedTime), ymin = 4.4, ymax = 5), 
                            fill = "seagreen3", alpha = 0.01)
      targetDhi <- 5
      targetDli <- 4.4
    } else if(input$ageinput >= 8 & input$ageinput <18){
      targetDh <- geom_hline(yintercept = 5, colour = "red", linetype = 4, size = 1)
      targetDl <- geom_hline(yintercept = 6, colour = "red", linetype = 4, size = 1)
      targetDb <- geom_rect(aes(xmin = min(Zoll_Data$FixedTime), 
                                xmax = max(Zoll_Data$FixedTime), ymin = 5, ymax = 6), 
                            fill = "seagreen3", alpha = 0.01)
      targetDhi <- 6
      targetDli <- 5
    } else{
      print("Inelligible")
    }
    
    Zoll_Data$color <- ifelse(Zoll_Data$DepthCm < targetDli | Zoll_Data$DepthCm > targetDhi, "blue", "red")
    
    Depth_Plot <- ggplot(Zoll_Data, aes(x = FixedTime, y = DepthCm, color = color)) +
      theme_bw() +
      geom_bar(stat = "identity", width = 0.1) +
      xlab("Time (s)") + ylab("Depth (cm)") +
      targetDh + targetDl + targetDb + theme(axis.line = element_line(size = 1, colour = "black")) +
      theme(axis.line = element_line(arrow = arrow())) + theme(legend.position="none")
    
    Depth_Plot
    
  })
  
  #Outputs the average depth and rate
  output$DepthPlot <- renderPlot({
    validate(
      need(input$datafile != "", ""),
      need(input$ageinput != "", "")
    )
    if (input$datafile == 0) return(NULL)
    DepthPlot()
  })
  
## Rate Plot-------------------------------------------------------------------- 
  RatePlot <- reactive({
    
    Zoll_Data <- filedata() 
    
    targetRh <- geom_hline(yintercept = 120, colour = "red", linetype = 4, size = 1)
    targetRl <- geom_hline(yintercept = 100, colour = "red", linetype = 4, size = 1)
    targetRb <- geom_rect(aes(xmin = min(Zoll_Data$FixedTime), 
                              xmax = max(Zoll_Data$FixedTime), ymin = 100, ymax = 120), 
                          fill = "seagreen3", alpha = 0.01)
    
    Zoll_Data$colorRate <- ifelse(Zoll_Data$RateCPM < 100 | Zoll_Data$RateCPM > 120, "red", "blue")
    
    Rate_Plot <- ggplot(Zoll_Data, aes(x = FixedTime, y = RateCPM)) +
      theme_bw() +
      geom_point(stat = "identity", color = Zoll_Data$colorRate, size = 1) +
      xlab("Time (s)") + ylab("Rate (cpm)") +
      targetRh + targetRl + targetRb + theme(axis.line = element_line(size = 1, colour = "black")) +
      theme(axis.line = element_line(arrow = arrow()))
    
    Rate_Plot
    
  })
  
  #Outputs the average depth and rate
  output$RatePlot <- renderPlot({
    validate(
      need(input$datafile != "", ""),
      need(input$ageinput != "", "")
    )
    if (input$datafile == 0) return(NULL)
    RatePlot()
  })



  
  # output$report <- downloadHandler(
  #   # For PDF output, change this to "report.pdf"
  #   filename = "report.pdf",
  #   content = function(file) {
  #     # Copy the report file to a temporary directory before processing it, in
  #     # case we don't have write permissions to the current working dir (which
  #     # can happen when deployed).
  #     tempReport <- file.path(tempdir(), "report.Rmd")
  #     file.copy("report.Rmd", tempReport, overwrite = TRUE)
  #     
  #     # Set up parameters to pass to Rmd document
  #     params <- list(n = input$datafile)
  #     
  #     # Knit the document, passing in the `params` list, and eval it in a
  #     # child of the global environment (this isolates the code in the document
  #     # from the code in this app).
  #     rmarkdown::render(tempReport, output_file = file,
  #                       params = params,
  #                       envir = new.env(parent = globalenv())
  #     )
  #   }
  # )
  

#* Download Handler------------------------------------------------------------
  
  output$report <- 
    downloadHandler(
      "results_from_shiny.pdf",
      content = 
        function(file)
        {
          rmarkdown::render(
            input = "report.Rmd",
            output_file = "report.pdf",
            params = list(DepthPlot = DepthPlot(),
                          RatePlot = RatePlot(),
                          CCFTbl = CCF(),
                          CCFPie = CCFPiePlot(),
                          AvgDepthRate = AvgDepthRate()),
            envir = new.env(parent = globalenv())
          ) 
          readBin(con = "report.pdf", 
                  what = "raw",
                  n = file.info("report.pdf")[, "size"]) %>%
            writeBin(con = file)
        }
    )
   
  
})
