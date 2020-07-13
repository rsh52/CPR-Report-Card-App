# The server.r script is responsible for all of the computation and coding
# that makes the widgets, plots, and analytics of the app work. 

# Especially important to note is that the downloadHandler is dependent on the 
# report.Rmd file. The rmd serves to make the framework for the pdf that the 
# downloadHandler feeds the information into. 

shinyServer(function(input, output) {
  
  # File Data --------------------------------------------------------------------
  # This function is responsible for loading in the selected file
  filedata <- reactive({
    infile <- input$datafile
    
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    filedata <- read.csv(infile$datapath, skipNul = TRUE)
    colnames(filedata) <- c("Second", "Milisecond", "DepthIn", "RateCPM", "RVMMS",
                            "Valid")
    
    # Convert second and milisecond into one Time Column
    # Drop Inches and extra time columns
    # Create time difference and period columns to define pause/rosc/CCs,
    # and inform CCF calculations
    filedata <- filedata %>%
      mutate(Time = Second + Milisecond/1000,
             DepthCm = DepthIn*2.54/1000) %>% 
      select(Time, DepthCm, RateCPM, RVMMS, Valid) %>% 
      mutate(
        TDiff = Time - lag(Time), # Create a Time difference column with lag
        Period = if_else(TDiff <2, "compressions", 
                         ifelse(TDiff >=2 & TDiff < 120, "pause", 
                                ifelse(TDiff >= 120, "rosc", "NA")))
      )
    
    # Replace NA of first compression with 0 and compressions
    # NOTE THIS DOES NOT CURRENTLY WORK IN THE OUTPUT
    # filedata$TDiff[is.na(filedata$TDiff)] <- 0
    # filedata$Period[is.na(filedata$Period)] <- "compressions"
    
  })
  
  #This previews the CSV data file through the Zoll_Data frame
  output$filetable <- renderTable({
    if (input$datafile == 0) return(NULL)
    head(filedata())
  })
  
  # Average Tbl ----------------------------------------------------
  AvgTbl <- reactive({
    Zoll_Data <- filedata()
    
    # Define Age criteria based on user input
    if(input$ageinput <1){
      targetDhi <- 4.6
      targetDli <- 3.6
    } else if(input$ageinput >= 1 & input$ageinput <8){
      targetDhi <- 5.6
      targetDli <- 4.6
    } else if(input$ageinput >= 8 & input$ageinput <18){
      targetDhi <- 6
      targetDli <- 5
    } else{
      print("Inelligible")
    }
    
    # Contruct averages to feed final table
    Avgframe <- Zoll_Data %>% 
      mutate(DepthAvg = round(mean(DepthCm, na.rm = T), digits = 1),
             RateAvg = round(mean(RateCPM, na.rm = T), digits = 1),
             RVAvg = round(mean(RVMMS, na.rm = T), digits = 1)) %>% 
      select(DepthAvg, RateAvg, RVAvg) %>% 
      distinct()

    # Calculate percentages in range of criteria
    Zoll_CC_Total <- nrow(Zoll_Data)
    Depth_CC_In <- length(subset(Zoll_Data$DepthCm, Zoll_Data$DepthCm >= targetDli & Zoll_Data$DepthCm <= targetDhi))
    Depth_Perc_In <- 100*round(Depth_CC_In / Zoll_CC_Total,2)
    Rate_CC_In <- length(subset(Zoll_Data$RateCPM, Zoll_Data$RateCPM >= 100 & Zoll_Data$RateCPM <= 120))
    Rate_Perc_In <- 100*round(Rate_CC_In / Zoll_CC_Total,2)
    RV_CC_In <- length(subset(Zoll_Data$RVMMS, Zoll_Data$RVMMS >= 400))
    RV_Perc_In <- 100*round(RV_CC_In/Zoll_CC_Total, 2)
    
    ## CCF Calculations
    CCFTbl <- Zoll_Data %>% 
      summarise(Compressions = paste0(round(sum(Period == "compressions" | Period == "rosc", na.rm = T)/nrow(.), 2)*100, "%"),
                Pause = paste0(round(sum(Period == "pause", na.rm = T)/nrow(.), 2)*100, "%"))
    
    CCFTbl <- CCFTbl %>% 
      mutate(" " = "CCF Score: ") %>% 
      select(" ", Compressions, Pause)
    
    # Construct final dataframe to feed to html output
    Avgframe <- data.frame("Metric" = c("Avg. Depth (cm)", "% in Target Depth", "% in Target Rate", "Avg. Rate (cpm)", "Avg. RV (mm/s)", "% in Target RV", "CCF Score:"),
               "Value" = unlist(c(Avgframe[1], paste0(Depth_Perc_In,"%"), Avgframe[2], paste0(Rate_Perc_In,"%"), Avgframe[3], paste0(RV_Perc_In,"%"), CCFTbl[1,2])))
  
    Avgframe %>% 
      kable() %>% 
      kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, font_size = 18)

  })
  
  # Outputs the average depth and rate
  # Note that validate and need commands control initial blank error messages
  output$AvgTbl <- renderText({
    validate(
      need(input$datafile != "", "Please Upload a Dataset"),
      need(input$ageinput != "", "Please Select an Age")
    )
    if (input$datafile == 0) return(NULL)
    AvgTbl()
  })

  ## CCF Pie Plot ----------------------------------------------------------------
  CCFPiePlot <- reactive({
    
    Zoll_Data <- filedata()
    
    CCFTbl <- Zoll_Data %>% 
      summarise(Score = c(round(sum(Period == "compressions" | Period == "rosc", na.rm = T)/nrow(.), 4)*100,
                          round(sum(Period == "pause", na.rm = T)/nrow(.), 4)*100),
                Labels = c("Compressions", "Pause"),
                color = c("#55ac5d", "#ad1f1f")
      )
    
    hchart(CCFTbl, type = "pie", hcaes(labels = Labels, y = Score, color = color)) %>% 
      hc_tooltip(pointFormat = "{point.Labels}: <b>{point.Score}%</b>", 
                 headerFormat = NULL, crosshairs = TRUE, borderWidth = 1, shared=T) %>% 
      hc_plotOptions(pie=list(dataLabels=list(enabled = F))) %>% 
      hc_title(text = "CCF Score*") %>% 
      hc_credits(enabled = TRUE,
                 text = "*CCF Score is a rough approximation") %>% 
      hc_add_theme(hc_theme_smpl())
    
  })
  
  #Outputs the average depth and rate
  output$CCFPiePlot <- renderHighchart({
    validate(
      need(input$datafile != "", ""),
      need(input$ageinput != "", "")
    )
    if (input$datafile == 0) return(NULL)
    CCFPiePlot()
  })
  
  ## HC Depth Plot ------------------------------------------------------------
  DepthHC <- reactive({
    Zoll_Data <- filedata()
    
    if(input$ageinput <1){
      targetDhi <- 4
      targetDli <- 3.3
    } else if(input$ageinput >= 1 & input$ageinput <8){
      targetDhi <- 5
      targetDli <- 4.4
    } else if(input$ageinput >= 8 & input$ageinput <18){
      targetDhi <- 6
      targetDli <- 5
    } else{
      print("Inelligible")
    }
    
    Zoll_Data <- Zoll_Data %>% 
      mutate(color = ifelse(DepthCm < targetDli | DepthCm > targetDhi, "#ad1f1f", "#55ac5d")) %>% 
      filter(DepthCm > 0.4, DepthCm < 10)
    
    # Color coding for age-specific target depth guidelines using ifelse
    ifelse(targetDhi == 6,
           depth.hc <- hchart(Zoll_Data, type = "column", hcaes(x = Time, y = round(DepthCm, digits = 2), color = color)) %>%
             hc_tooltip(pointFormat = "Depth (cm): <b>{point.RateCPM}</b>", 
                        crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
             hc_yAxis_multiples(plotLines = list(list(value = targetDli, color = "red", width = 2, dashStyle = "shortdash"),
                                                 list(value = targetDhi, color = "red", width = 2, dashStyle = "shortdash"))) %>%
             hc_chart(zoomType = "xy", dataSorting.enabled = TRUE) %>%
             hc_exporting(enabled = TRUE,
                          filename = "DepthPlot") %>%
             hc_yAxis(title = list(text = "Depth (cm)")) %>%
             hc_xAxis(title = list(text = "Time (s)")) %>% 
             hc_title(text = "CPR Depth Plot", 
                      align = "left") %>%
             hc_add_theme(hc_theme_smpl()),
           
           depth.hc <- hchart(Zoll_Data, type = "column", hcaes(x = Time, y = round(DepthCm, digits = 2), color = color)) %>%
             hc_tooltip(pointFormat = "Depth (cm): <b>{point.RateCPM}</b>", 
                        crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
             hc_yAxis_multiples(plotLines = list(list(value = targetDli, color = "blue", width = 2, dashStyle = "shortdash"),
                                                 list(value = targetDhi, color = "red", width = 2, dashStyle = "shortdash"))) %>%
             hc_chart(zoomType = "xy", dataSorting.enabled = TRUE) %>%
             hc_exporting(enabled = TRUE,
                          filename = "DepthPlot") %>%
             hc_yAxis(title = list(text = "Depth (cm)")) %>%
             hc_xAxis(title = list(text = "Time (s)")) %>% 
             hc_title(text = "CPR Depth Plot", 
                      align = "left") %>%
             hc_add_theme(hc_theme_smpl())
    )
    
    depth.hc
    
  })
  
  #Outputs the average depth and rate
  output$DepthHC <- renderHighchart({
    validate(
      need(input$datafile != "", ""),
      need(input$ageinput != "", "")
    )
    if (input$datafile == 0) return(NULL)
    DepthHC()
  })
  
  ## HC Rate Plot--------------------------------------------------------------
  RateHC <- reactive({
    Zoll_Data <- filedata()
    
    Zoll_Data <- Zoll_Data %>% 
      mutate(color = ifelse(RateCPM < 100 | RateCPM > 120,"#ad1f1f", "#55ac5d")) %>% 
      filter(RateCPM > 50, RateCPM < 180)
    
    hchart(Zoll_Data, type = "column", hcaes(x = Time, y = round(RateCPM, digits = 2), color = color)) %>%
      hc_tooltip(pointFormat = "Rate (cpm): <b>{point.RateCPM}</b>", 
                 crosshairs = TRUE, shared = TRUE, borderWidth = 1, shared=T) %>%
      hc_yAxis_multiples(plotLines = list(list(value = 120, color = "blue", width = 2, dashStyle = "shortdash"),
                                          list(value = 100, color = "blue", width = 2, dashStyle = "shortdash"))) %>%
      hc_chart(zoomType = "xy", dataSorting.enabled = TRUE) %>%
      hc_exporting(enabled = TRUE,
                   filename = "RatePlot") %>%
      hc_title(text = "CPR Rate Plot", 
               align = "left") %>%
      hc_yAxis(title = list(text = "Rate (cpm)")) %>%
      hc_xAxis(title = list(text = "Time (s)")) %>% 
      hc_add_theme(hc_theme_smpl())
    
  })
  
  #Outputs the average depth and rate
  output$RateHC <- renderHighchart({
    validate(
      need(input$datafile != "", ""),
      need(input$ageinput != "", "")
    )
    if (input$datafile == 0) return(NULL)
    RateHC()
  })
  
  ## Report Depth Plot ------------------------------------------------------------
  DepthPlot <- reactive({
    Zoll_Data <- filedata()
    
    if(input$ageinput <1){
      targetDh <- geom_hline(yintercept = 4.6, colour = "red", linetype = 4, size = 1)
      targetDl <- geom_hline(yintercept = 3.6, colour = "blue", linetype = 2, size = 1)
      # targetDb <- geom_rect(aes(xmin = min(Zoll_Data$Time), 
      #                           xmax = max(Zoll_Data$Time), ymin = 3.3, ymax = 4), 
      #                       fill = "seagreen3", alpha = 0.01)
      targetDhi <- 4
      targetDli <- 3.3
    } else if(input$ageinput >= 1 & input$ageinput <8){
      targetDh <- geom_hline(yintercept = 5.6, colour = "red", linetype = 4, size = 1)
      targetDl <- geom_hline(yintercept = 4.6, colour = "blue", linetype = 2, size = 1)
      # targetDb <- geom_rect(aes(xmin = min(Zoll_Data$Time), 
      #                           xmax = max(Zoll_Data$Time), ymin = 4.4, ymax = 5), 
      #                       fill = "seagreen3", alpha = 0.01)
      targetDhi <- 5
      targetDli <- 4.4
    } else if(input$ageinput >= 8 & input$ageinput <18){
      targetDh <- geom_hline(yintercept = 5, colour = "red", linetype = 4, size = 1)
      targetDl <- geom_hline(yintercept = 6, colour = "red", linetype = 4, size = 1)
      # targetDb <- geom_rect(aes(xmin = min(Zoll_Data$Time), 
      #                           xmax = max(Zoll_Data$Time), ymin = 5, ymax = 6), 
      #                       fill = "seagreen3", alpha = 0.01)
      targetDhi <- 6
      targetDli <- 5
    } else{
      print("Inelligible")
    }
    
    Zoll_Data$color <- ifelse(Zoll_Data$DepthCm < targetDli | Zoll_Data$DepthCm > targetDhi, "#ad1f1f", "#55ac5d")
    
    Zoll_Data <- Zoll_Data %>% 
      filter(DepthCm > 0.4, DepthCm < 10)
    
    Depth_Plot <- ggplot(Zoll_Data, aes(x = Time, y = DepthCm)) +
      theme_bw() +
      geom_bar(stat = "identity", width = 0.1, color = Zoll_Data$color) +
      #geom_col(aes(color = color)) +
      xlab("Time (s)") + ylab("Depth (cm)") +
      targetDh + targetDl + theme(axis.line = element_line(size = 1, colour = "black")) +
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
  
  ## Report Rate Plot--------------------------------------------------------------
  RatePlot <- reactive({
    Zoll_Data <- filedata()
    
    targetRh <- geom_hline(yintercept = 120, colour = "red", linetype = 4, size = 1)
    targetRl <- geom_hline(yintercept = 100, colour = "red", linetype = 4, size = 1)
    targetRb <- geom_rect(aes(xmin = min(Zoll_Data$Time), 
                              xmax = max(Zoll_Data$Time), ymin = 100, ymax = 120))
    
    Zoll_Data$colorRate <- ifelse(Zoll_Data$RateCPM < 100 | Zoll_Data$RateCPM > 120, "#ad1f1f", "#55ac5d")
    
    Zoll_Data <- Zoll_Data %>% 
      filter(RateCPM > 50, RateCPM < 180)
    
    Rate_Plot <- ggplot(Zoll_Data, aes(x = Time, y = RateCPM)) +
      theme_bw() +
      geom_point(stat = "identity", color = Zoll_Data$colorRate, size = 1) +
      xlab("Time (s)") + ylab("Rate (cpm)") +
      targetRh + targetRl  + theme(axis.line = element_line(size = 1, colour = "black")) +
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
  
  ## Epoch Report Table--------------------------------------------------------------
  EpochEval <- reactive({
    Zoll_Data <- filedata()
    
    epoch.df <- Zoll_Data %>%
      mutate(
        CompTimeZero = Time - dplyr::first(Time), # Set first to 0 and subtract from all subsequent
        CompTimeRound = round(CompTimeZero, 0) # Round to make interpolation easier
      ) %>% 
      group_by(CompTimeRound) %>% 
      summarise(
        RateCPM = mean(RateCPM),
        DepthCm = mean(DepthCm),
        RVMMS = mean(RVMMS)
      ) %>% 
      # Linearly interpolate and fill in missing seconds, leaving NA for compression metrics
      complete(CompTimeRound = full_seq(0:max(CompTimeRound), period = 1), fill = list(
        RateCPM = NA, DepthCm = NA,
        RVMMS = NA)) %>% 
      # Apply a rolling average by a window of observations, excluding NA values
      summarise(
        epoch.rate = round(rollapply(RateCPM, width = input$epochwin, FUN = input$epochfx, na.rm = T, fill = NA, by = input$epochwin),1),
        epoch.depth = round(rollapply(DepthCm, width = input$epochwin, FUN = input$epochfx, na.rm = T, fill = NA, by = input$epochwin),1),
        epoch.RV = round(rollapply(RVMMS, width = input$epochwin, FUN = input$epochfx, na.rm = T, fill = NA, by = input$epochwin),1)
        # Uncomment below for testing
        # epoch.rate = round(rollapply(RateCPM, width = 30, FUN = mean, na.rm = T, fill = NA, by = 30), 1),
        # epoch.depth = round(rollapply(DepthCm, width = 30, FUN = mean, na.rm = T, fill = NA, by = 30), 1),
        # epoch.RV = round(rollapply(RVMMS, width = 30, FUN = mean, na.rm = T, fill = NA, by = 30),1)
      ) %>% 
      # Filter for distinct observations
      distinct() %>% 
      # Remove NA epochs
      filter(!is.na(epoch.rate) & !is.nan(epoch.rate)) %>% 
      # Assign Epoch Number per event
      mutate("Epoch No." = row_number())
      
    epoch.df <- epoch.df[c(4,2,1,3)]
      
    colnames(epoch.df) <- c("Epoch No.", "Depth (cm)", "Rate (cpm)", "Release Velocity(mm/s)")
    
    epoch.df %>% 
      mutate(
        `Rate (cpm)` = cell_spec(`Rate (cpm)`, color = "white", background = ifelse(`Rate (cpm)` > 100 & `Rate (cpm)` <120, "green", "red")),
        `Depth (cm)` = cell_spec(`Depth (cm)`, color = "white", background = ifelse(input$ageinput <1, 
                                                                                    ifelse(`Depth (cm)` >= 3.3 & `Depth (cm)` <= 4, "green", "red"),
                                                                                    ifelse(input$ageinput >= 1 & input$ageinput <8, 
                                                                                           ifelse(`Depth (cm)` >= 4.4 & `Depth (cm)` <=5, "green", "red"),
                                                                                           ifelse(`Depth (cm)` >= 5 & `Depth (cm)` <= 6, "green", "red")))),
        `Release Velocity(mm/s)` = cell_spec(`Release Velocity(mm/s)`, color = "white", background = ifelse(`Release Velocity(mm/s)` > 400, "green", "red"))
      ) %>% 
      # Add in kableExtra HTML features
      kable(escape = F) %>% 
      kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>% 
      scroll_box(width = "100%", height = "400px")
    
  })
  
  #Outputs the average depth and rate
  output$EpochEval <- renderText({
    validate(
      need(input$datafile != "", ""),
      need(input$ageinput != "", "")
    )
    if (input$datafile == 0) return(NULL)
    EpochEval()
  })
  
  
  
  #* Download Handler------------------------------------------------------------
  
  # output$report <- 
  #   downloadHandler(
  #     "results_from_shiny.pdf",
  #     content = 
  #       function(file)
  #       {
  #         rmarkdown::render(
  #           input = "report.Rmd",
  #           output_file = "report.pdf",
  #           params = list(DepthPlot = DepthPlot(),
  #                         RatePlot = RatePlot(),
  #                         CCFTbl = CCF(),
  #                         CCFPie = CCFPiePlot(),
  #                         PercTbl = PercDepthRate(),
  #                         AvgDepthRate = AvgDepthRate()),
  #           envir = new.env(parent = globalenv())
  #         ) 
  #         readBin(con = "report.pdf", 
  #                 what = "raw",
  #                 n = file.info("report.pdf")[, "size"]) %>%
  #           writeBin(con = file)
  #       }
  #   )
  
  # Download SOP for formatting proper Upload file
  output$downloadSOP <- downloadHandler(
    filename <- function() {
      paste("SOP_007A_CPR_Report_Card_App_Documentation", "pdf", sep=".")
    },
    
    content <- function(file) {
      file.copy("SOP_007A_CPR_Report_Card_App_Documentation.pdf", file)
    },
    contentType = "pdf"
  )
  
  # Download sample pre-formatted upload file
  output$downloadCCs <- downloadHandler(
    filename <- function() {
      paste("Example_Pediatric_Compressions", "csv", sep=".")
    },
    
    content <- function(file) {
      file.copy("Example_Pediatric_Compressions.csv", file)
    },
    contentType = "csv"
  )
  
  
})

