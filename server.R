library(shiny)
nvisitors = reactiveVal(0)

# Function to format output of analysis ------------------------------------------------------------------------------------------------------------------------
format.list <- function (l) {
  s <- capture.output(print(l))
  writeLines(
    sub("^ *(\\[,\\d+\\] *)+$", "",        # Remove indices from second dimension.
        sub("^ *\\[\\d+,?\\] *", "", s)))  # Remove indices from first dimension.
}

# Define server logic ------------------------------------------------------------------------------------------------------------------------------------------
shinyServer(function(input, output, session) {
  
  options(shiny.reactlog=TRUE) 
  
  # Video instructions ------------------------------------------------------------------------------------------------------------------------------------------
  output$video <- renderUI({
    h3("How to use the tool? Watch the video!", align="center", tags$video(type = "video/mp4", src = "video_final.mp4", width="350px", height="350px", controls = "controls",  style="display: block; margin-left: auto; margin-right: auto;"))
  })
  
  # Track number of visitors at the bottom of page ---------------------------------------------------------------------------------------------------------------
  nvisitors(isolate(nvisitors()) + 1)
  onSessionEnded(function(x){ 
    nvisitors(isolate(nvisitors()) - 1)
  })
  
  output$counter = renderText({
    paste("Users:", nvisitors())
  })
  
  # To make actionButton to move to a tab
  observeEvent(input$gotodata, {
    updateTabsetPanel(session, inputId = 'mytabsetpanel', selected = 'data')
  })
  
  observeEvent(input$SEfromSD, {
    updateTabsetPanel(session, inputId = 'mytabsetpanel', selected = 'missing')
  })
  
  # Define reactive values ---------------------------------------------------------------------------------------------------------------------------------------
  
  # Load default data 
  
  defaultDat <- reactive({
    defaultDat <- read.csv("./www/example.csv", sep=";")
    
  })
  
  
  df_upload <- reactive({
    inFile <- input$data_upload
    if (is.null(inFile)) {return(defaultDat())}
    else 
      #rv$df <- try(read.csv(inFile$datapath, header = input$header, sep = input$separator, dec =input$dec), silent=T)  
      
      try(read.csv(inFile$datapath, header = input$header, sep = input$separator, dec =input$dec), silent=T)
    
    # validate(need(is.data.frame(rv$df) == TRUE, paste("Error with file import:",  rv$df)) )
    # rv$df
    
    
    # df_upload <- reactive({
    #      inFile <- input$data_upload
    #       if (is.null(inFile)) {return(NULL)}
    #       rv$df <- try(read.csv(inFile$datapath, header = input$header, sep = input$separator, dec =input$dec), silent=T)
    # 
    #   validate(need(is.data.frame(rv$df) == TRUE, paste("Error with file import:",  rv$df)) )
    #   rv$df
    # 
    # rv$ID           <- rv$df[[1]]
    # rv$Study        <- rv$df[[2]]
    # rv$MeanBaseline <- rv$df[[3]]
    # rv$sdBaseline   <- rv$df[[4]]
    # rv$seBaseline   <- rv$df[[5]]
    # rv$MeanFU       <- rv$df[[6]]
    # rv$sdFU         <- rv$df[[7]]
    # rv$seFU         <- rv$df[[8]]
    # rv$Correlation  <- rv$df[[9]]
    # rv$MeanCFB      <- rv$df[[10]]
    # rv$sdCFB        <- rv$df[[11]]
    # rv$seCFB        <- rv$df[[12]]
    # rv$NCFB         <- rv$df[[13]]
    # rv$group        <- rv$df[[14]]
    
    
    # Note to try later: once the analysis_data are loaded, I want  make reactive the sliders for the ANCOVA method
    #  if the code below is uncommented out, there is lag in uploading the dataset
    
    # test <- analysis_data()
    # rv$cor1    <- tapply(test$Correlation, test$group, mean, na.rm=T)[[1]]
    # rv$cor2    <- tapply(test$Correlation, test$group, mean, na.rm=T)[[2]]
    # updateNumericInput(session, "cor1", value =    rv$cor1)
    # updateNumericInput(session, "cor2", value =    rv$cor2)
    
  })
  
  # Data table of input/example dataset -------------------------------------------------------------------------------------------------------------------
  output$input_table <- DT::renderDataTable({
    df_upload()
    if (is.null(df_upload())){return(NULL)}
    DT::datatable(df_upload() )
                  # ,
                  # filter = 'top',
                  # selection = list(mode = 'single', selected = 1),
                  # options = list(search = list(caseInsensitive = TRUE),
                  #                searchHighlight = TRUE,
                  #                scrollX = TRUE,
                  #                pageLength = 10))
  })
  
  # Print data structure ---------------------------------------------------------------------------------------------------------------------------------
  output$structure <- renderPrint({
    req(df_upload())
    Dataset <- df_upload() # Renaming the data set to appear better in the table
    skim(Dataset)
    
  })
  
  
  # Need to build multiple reactive dataframes to make the final (filled-in) data set
  
  # Dataset 1: make reactive calculating means -------------------------------------------------------------------------------------------------------------------
  rv <- reactiveValues()
  
  df1 <- reactive({
    rv$df <- df_upload()
    
    rv$ID           <- rv$df[[1]]
    rv$Study        <- rv$df[[2]]
    rv$MeanBaseline <- rv$df[[3]]
    rv$sdBaseline   <- rv$df[[4]]
    rv$seBaseline   <- rv$df[[5]]
    rv$MeanFU       <- rv$df[[6]]
    rv$sdFU         <- rv$df[[7]]
    rv$seFU         <- rv$df[[8]]
    rv$Correlation  <- rv$df[[9]]
    rv$MeanCFB      <- rv$df[[10]]
    rv$sdCFB        <- rv$df[[11]]
    rv$seCFB        <- rv$df[[12]]
    rv$NCFB         <- rv$df[[13]]
    rv$group        <- rv$df[[14]]
    
    rv$df2  <- rv$df %>%
      mutate (MeanFU     = ifelse(is.na(rv$MeanFU),  rv$MeanCFB + rv$MeanBaseline, rv$MeanFU),
              MeanCFB    = ifelse(is.na(rv$MeanCFB), rv$MeanFU - rv$MeanBaseline, rv$MeanCFB) )
  })
  
  # Dataset 2: Calculate SDs from SEs-----------------------------------------------------------------------------------------------------------------------------
  df2 <- eventReactive(input$SEfromSD, {
    df1() %>%
      mutate (sdBaseline = ifelse(is.na(rv$sdBaseline), rv$seBaseline*sqrt(rv$NCFB), rv$sdBaseline), 
              sdFU       = ifelse(is.na(rv$sdFU),       rv$seFU*sqrt(rv$NCFB),  rv$sdFU),
              sdCFB      = ifelse(is.na(rv$sdCFB),      rv$seCFB*sqrt(rv$NCFB), rv$sdCFB)
      )
  })
  
  # Output table of SDs from SEs actionbutton 
  output$SDfromSE<-renderTable({
    if(is.null(df2()))
    {return(NULL)}
    df2()
    #coalsce(df2(),df1())
  })
  
  # Dataset 3: Assume equal pre and post SDs ---------------------------------------------------------------------------------------------------------------------
  df3 <- eventReactive(input$sameSD, {
    df2() %>%
      mutate (sdFU = ifelse(is.na(sdFU), sdBaseline, sdFU) 
      )
  })
  
  # Output table of equal SDs pre/post actiobutton
  output$sameSDboth<-renderTable({
    if(is.null(df3()))
    {return(NULL)}
    df3()
    
  })
  
  
  # Dataset 4: Calculate Correlations from SDs--------------------------------------------------------------------------------------------------------------------
  df4 <- eventReactive(input$correl, {
    df3() %>%
      mutate (Correlation = ifelse(is.na(Correlation), (sdBaseline^2+sdFU^2-sdCFB^2)/(2*sdBaseline*sdFU), Correlation)
      )
  })
  
  # Output table of correlation actiobutton   
  output$correl_output<-renderTable({
    if(is.null(df4()))
    {return(NULL)}
    df4()
    
  })
  
  # Dataset 5: Make final calculations --------------------------------------------------------------------------------------------------------------------------
  df5 <- eventReactive(input$final_calc, {
    df4() %>%
      mutate (Correlation  = ifelse(is.na(Correlation) & (group=="0"), tapply(Correlation, group, median, na.rm=T)[1], Correlation), # for control group
              Correlation  = ifelse(is.na(Correlation) & (group=="1"), tapply(Correlation, group, median, na.rm=T)[2], Correlation), # for treatment group
              seBaseline   = ifelse(is.na(seBaseline), sdBaseline/sqrt(NCFB), seBaseline),
              seFU         = ifelse(is.na(seFU), sdFU/sqrt(NCFB), seFU),
              sdCFB        = ifelse(is.na(sdCFB), sqrt(sdBaseline^2+sdFU^2-2*Correlation*sdBaseline*sdFU), sdCFB),
              seCFB        = ifelse(is.na(seCFB), sdCFB/sqrt(NCFB), seCFB)
      )
  })
  
  # Output table of final calculations
  output$full_data <-renderTable({
    if(is.null(df5()))
    {return(NULL)}
    df5()
    #coalesce(df5(), df1())
    
    # inFile <- input$data_upload
    # ifelse(is.na(input$data_upload), return (df5()), return (rv$df))
    # 
  })
  
  
  # Final dataset where models will be fitted on ------------------------------------------------------------------------------------------------------------------
  analysis_data <- reactive({
    if(is.null(df5()))
    {return(NULL)}
    df5()
  })
  
  
  # Different attempts for the 'View final data' tab - It should show the uploaded dataset (rv$df) if no missing data occur and the 
  #  df5() dataset if missing data occuu
  
  # analysis_data <-reactive({
  #     df_upload()
  #      # ifelse(sum(is.na(df_upload()))!=0,df5(),rv$df)
  #     if (sum(is.na(df_upload()))!=0)  {return (df5()) }
  #     else { return (rv$df)}
  #   })
  
  # analysis_data <-reactive({
  #     df_upload()
  #     ifelse(sum(is.na(df_upload()))!=0, return (df5()), ifelse(sum(is.na(df_upload()))==0, return (rv$df), return(df5())) )
  #     })
  # 
  
  # analysis_data <-reactive({
  # inFile <- input$data_upload
  # ifelse(is.na(input$data_upload), return (df5()), return (df1()))
  # 
  # })
  
  # Output data table of final dataset  
  output$final_data<-renderTable({
    if(is.null(analysis_data()))
    {return(NULL)}
    analysis_data()
  })
  
  
  #----------------------------------------------------------------------------------------------------------------------------------------------------------------    
  # Output Standard AD analyses
  
  # Output final (follow-up) scores analysis ----------------------------------------------------------------
  
  # FE analysis results 
  final.FE <- reactive({
    
    if (input$type == "ce") {
      
      if (is.null(analysis_data())){return(NULL)}
      analysis_data()
      df <- analysis_data()
      # change to wide format
      drop         <- which(colnames(df) %in% "Study")
      df           <- df[,-drop]
      data.AD_wide <- dcast(melt(df, id.vars=c("ID", "group")), ID~variable+group)
      
      MA.fixed.final <- rma(m1i=MeanFU_1, m2i=MeanFU_0, sd1i=sdFU_1, sd2i=sdFU_0, n1i=NCFB_1, n2i=NCFB_0, data=data.AD_wide, measure="MD", method="FE")
      list(MA.fixed.final = MA.fixed.final) 
    }
    
  })
  
  
  fe.final <- reactive({
    
    if (input$type == "ce")  {
      
      MA.fixed.final <- final.FE()$MA.fixed.final
      
      cat("--- Mean differences based on final (follow-up) scores under the CE model ---","\n")
      
      MA.fixed.final
    }
    
  })
  
  
  output$final_fe.out <- renderPrint({
    fe.final()
  })
  
  # RE analysis results 
  final.RE <- reactive({
    
    if (input$type == "re")  {
      
      if (is.null(analysis_data())){return(NULL)}
      analysis_data()
      df <- analysis_data()
      # change to wide format 
      drop         <- which(colnames(df) %in% "Study")
      df           <-  df[,-drop]
      data.AD_wide <- dcast(melt(df, id.vars=c("ID", "group")), ID~variable+group)
      
      MA.random.final <- rma(m1i=MeanFU_1, m2i=MeanFU_0, sd1i=sdFU_1, sd2i=sdFU_0, n1i=NCFB_1, n2i=NCFB_0, data=data.AD_wide, measure="MD", method="REML", knha=input$HK)
      list(MA.random.final = MA.random.final) 
      
      
    }
    
  })
  
  re.final <- reactive({
    
    if (input$type == "re") {
      MA.random.final <- final.RE()$MA.random.final
      
      cat("--- Mean differences based on final (follow-up) scores under the RE model ---","\n")
      
      MA.random.final
    }
  })
  
  
  output$final_re.out<- renderPrint({
    re.final()
  })
  
  # Forest plots for FE and RE - final scores 
  forest.final <- function(){
    
    if (input$type == "ce") {
      
      MA.fixed.final <- final.FE()$MA.fixed.final
      
      forest(MA.fixed.final, showweights=TRUE)
    }
    
    else if (input$type == "re") {
      
      MA.random.final <- final.RE()$MA.random.final
      
      forest(MA.random.final, showweights=TRUE)
    }
    
  }
  
  output$final.forest <- renderPlot({
    withProgress(message = 'Rendering', detail = 'Forest plot', value = 0, {
      for (i in 1:10) {
        incProgress(1/10)
        Sys.sleep(0.05)
      }
    })
    print(forest.final())
  })
  
  
  output$downloadfinalForest <- downloadHandler(
    filename = function() {
      paste('final.forest', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
      pdf(file=FILE)
      print(forest.final())
      dev.off()
    }
  )  
  
  # Funnel plots for FE and RE  - final scores 
  funnel.final <- function(){
    
    if (input$type == "ce") {
      
      MA.fixed.final <- final.FE()$MA.fixed.final
      
      funnel(MA.fixed.final, main="Standard Error", level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), legend=TRUE, back="cadetblue")
    }
    
    else if (input$type == "re") {
      
      MA.random.final <- final.RE()$MA.random.final
      
      funnel(MA.random.final, main="Standard Error", level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), legend=TRUE, back="cadetblue")
    }
    
  }
  
  output$final.funnel <- renderPlot({
    withProgress(message = 'Rendering', detail = 'Funnel plot', value = 0, {
      for (i in 1:10) {
        incProgress(1/10)
        Sys.sleep(0.05)
      }
    })
    print(funnel.final())
  })
  
  
  output$downloadfinalFunnel <- downloadHandler(
    filename = function() {
      paste('final.funnel', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
      pdf(file=FILE)
      print(funnel.final())
      dev.off()
    }
  )  
  
  
  # Output change scores analysis --------------------------------------------------------------------------------------------------------------------------------
  
  # FE analysis results 
  change.FE <- reactive({
    
    if (input$type == "ce") {
      
      if (is.null(analysis_data())){return(NULL)}
      analysis_data()
      df <- analysis_data()
      
      # change to wide format 
      drop         <- which(colnames(df) %in% "Study")
      df           <-  df[,-drop]
      data.AD_wide <- dcast(melt(df, id.vars=c("ID", "group")), ID~variable+group)
      
      MA.fixed.change <- rma(m1i=MeanCFB_1, m2i=MeanCFB_0, sd1i=sdCFB_1, sd2i=sdCFB_0, n1i=NCFB_1, n2i=NCFB_0,
                             data=data.AD_wide, measure="MD", method="FE")
      list(MA.fixed.change = MA.fixed.change) 
      
    }
    
  })
  
  fe.change <- reactive({
    
    if (input$type == "ce")  {
      
      MA.fixed.change <- change.FE()$MA.fixed.change
      
      cat("--- Mean differences based on change scores under the CE model ---","\n")
      
      MA.fixed.change
    }
    
  }) 
  
  
  output$change_fe.out <- renderPrint({
    fe.change()
  })
  
  # RE analysis results 
  change.RE <- reactive({
    
    if (input$type == "re") {
      
      if (is.null(analysis_data())){return(NULL)}
      analysis_data()
      df <- analysis_data()
      
      # change to wide format 
      drop         <- which(colnames(df) %in% "Study")
      df           <-  df[,-drop]
      data.AD_wide <- dcast(melt(df, id.vars=c("ID", "group")), ID~variable+group)
      
      MA.random.change <- rma(m1i=MeanCFB_1, m2i=MeanCFB_0, sd1i=sdCFB_1, sd2i=sdCFB_0, n1i=NCFB_1, n2i=NCFB_0,
                              data=data.AD_wide, measure="MD", method="REML", knha=input$HK)
      list(MA.random.change = MA.random.change) 
      
      
    }
    
  })
  
  re.change <- reactive({
    
    if (input$type == "re")  {
      
      MA.random.change <- change.RE()$MA.random.change
      
      cat("--- Mean differences based on change scores under the RE model ---","\n")
      
      MA.random.change
    }
    
  }) 
  
  output$change_re.out <- renderPrint({
    re.change()
  })
  
  
  # Forest plots for FE and RE - change scores 
  forest.change <- function(){
    
    if (input$type == "ce") {
      
      MA.fixed.change <- change.FE()$MA.fixed.change
      
      forest(MA.fixed.change, showweights=TRUE)
    }
    
    else if (input$type == "re") {
      
      MA.random.change <- change.RE()$MA.random.change
      
      forest(MA.random.change, showweights=TRUE)
    }
    
  }
  
  output$change.forest <- renderPlot(
    {
      withProgress(message = 'Rendering', detail = 'Forest plot', value = 0, {
        for (i in 1:10) {
          incProgress(1/10)
          Sys.sleep(0.05)
        }
      })
      print(forest.change())
    })
  
  
  output$downloadchangeForest <- downloadHandler(
    filename = function() {
      paste('change.forest', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
      pdf(file=FILE)
      print(forest.change())
      dev.off()
    }
  )  
  
  # Funnel plots for FE and RE - change scores 
  funnel.change<- function(){
    
    if (input$type == "ce") {
      
      MA.fixed.change <- change.FE()$MA.fixed.change
      
      funnel(MA.fixed.change, main="Standard Error", level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), legend=TRUE, back="cadetblue")
    }
    
    else if (input$type == "re") {
      
      MA.random.change <- change.RE()$MA.random.change
      
      funnel(MA.random.change, main="Standard Error", level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), legend=TRUE, back="cadetblue")
    }
    
  }
  
  output$change.funnel <- renderPlot({
    withProgress(message = 'Rendering', detail = 'Funnel plot', value = 0, {
      for (i in 1:10) {
        incProgress(1/10)
        Sys.sleep(0.05)
      }
    })
    print(funnel.change())
  })
  
  
  output$downloadchangeFunnel <- downloadHandler(
    filename = function() {
      paste('change.funnel', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
      pdf(file=FILE)
      print(funnel.change())
      dev.off()
    }
  )  
  
  
  
  # Output recovered ANCOVA approach -------------------------------------------------------------------------------------------------------------------------------
  
  # FE analysis results 
  ancova.FE <- reactive({
    
    if (input$type == "ce") {
      
      if (is.null(analysis_data())){return(NULL)}
      analysis_data()
      df <- analysis_data()
      
      
      drop         <- which(colnames(df) %in% "Study")
      df           <-  df[,-drop]
      data.AD_wide <- dcast(melt(df, id.vars=c("ID", "group")), ID~variable+group)
      
      # calculate pooled standard deviations of baseline and follow-up values
      # For the first three methods the data need to be in wide format
      
      sdpooledB<- with(data.AD_wide, sqrt((((NCFB_1 - 1)*(sdBaseline_1^2)) + (NCFB_0 - 1)*(sdBaseline_0^2))/((NCFB_1+NCFB_0)-2)))
      sdpooledF<- with(data.AD_wide, sqrt((((NCFB_1 - 1)*(sdFU_1^2)) + (NCFB_0 - 1)*(sdFU_0^2))/((NCFB_1+NCFB_0)-2)))
      
      # Calculate ancova estimate using formula from Senn et al. 2007 and McKenzie et al 2016
      # using the pooled correlation
      
      ripooled <- with(data.AD_wide, ((NCFB_1*Correlation_1*sdBaseline_1*sdFU_1 +  NCFB_0*Correlation_0*sdBaseline_0*sdFU_0) )
                       /((NCFB_1+NCFB_0)*sdpooledB*sdpooledF))
      
      # ripooled <- with(data.AD_wide, ((NCFB_1*Correlation_1*sdBaseline_1*sdFU_1 +  NCFB_0*Correlation_0 *sdBaseline_0*sdFU_0) )
      #                  /((NCFB_1+NCFB_0)*sdpooledB*sdpooledF))
      
      ancova_est      <- with(data.AD_wide, (MeanFU_1-MeanFU_0)-ripooled*(sdpooledF/sdpooledB)*(MeanBaseline_1-MeanBaseline_0))
      
      var_ancova_est  <- with(data.AD_wide, sdpooledF^2*(1/NCFB_1)+sdpooledF^2*(1/NCFB_0))*(1-ripooled^2) # for different sample sizes from McKenzie and from Senn
      
      se_ancovas_est  <- with(data.AD_wide,sqrt(var_ancova_est))
      
      MA.fixed.ANCOVA <- rma(yi=ancova_est, sei=se_ancovas_est, method="FE")
      list(MA.fixed.ANCOVA = MA.fixed.ANCOVA) 
      
    }
    
  })
  
  fe.ancova <- reactive({
    
    if (input$type == "ce")  {
      
      MA.fixed.ANCOVA <- ancova.FE()$MA.fixed.ANCOVA
      
      cat("--- Mean differences based on reconstructed ANCOVA estimates under the CE model ---","\n")
      
      MA.fixed.ANCOVA
    }
    
  }) 
  
  
  output$ancova_fe.out <- renderPrint({
    fe.ancova()
  })
  
  
  # RE analysis results 
  ancova.RE <- reactive({
    
    if (input$type == "re") {
      if (is.null(analysis_data())){return(NULL)}
      analysis_data()
      df <- analysis_data()
      # calculate pooled standard deviations of baseline and follow-up values
      # For the first three methods the data need to be in wide format
      drop         <- which(colnames(df) %in% "Study")
      df           <- df[,-drop]
      data.AD_wide <- dcast(melt(df, id.vars=c("ID", "group")), ID~variable+group)
      
      sdpooledB<- with(data.AD_wide, sqrt((((NCFB_1 - 1)*(sdBaseline_1^2)) + (NCFB_0 - 1)*(sdBaseline_0^2))/((NCFB_1+NCFB_0)-2)))
      sdpooledF<- with(data.AD_wide, sqrt((((NCFB_1 - 1)*(sdFU_1^2)) + (NCFB_0 - 1)*(sdFU_0^2))/((NCFB_1+NCFB_0)-2)))
      
      # Calculate ancova estimate using formula from Senn et al. 2007
      
      # Correlation_0 <-input$cor1
      # Correlation_1 <-input$cor2
      
      ripooled <- with(data.AD_wide, ((NCFB_1*Correlation_1*sdBaseline_1*sdFU_1 +  NCFB_0*Correlation_0 *sdBaseline_0*sdFU_0) )
                       /((NCFB_1+NCFB_0)*sdpooledB*sdpooledF))
      
      ancova_est      <- with(data.AD_wide, (MeanFU_1-MeanFU_0)-ripooled*(sdpooledF/sdpooledB)*(MeanBaseline_1-MeanBaseline_0))
      
      var_ancova_est  <- with(data.AD_wide, sdpooledF^2*(1/NCFB_1)+sdpooledF^2*(1/NCFB_0))*(1-ripooled^2) # for different sample sizes from McKenzie and from Senn
      
      se_ancovas_est  <- with(data.AD_wide,sqrt(var_ancova_est))
      
      MA.random.ANCOVA <- rma(yi=ancova_est, sei=se_ancovas_est, method="REML", knha=input$HK)
      list(MA.random.ANCOVA = MA.random.ANCOVA) 
      
    }
    
  })
  
  re.ancova <- reactive({
    
    if (input$type == "re")  {
      
      MA.random.ANCOVA <- ancova.RE()$MA.random.ANCOVA
      
      cat("--- Mean differences based on reconstructed ANCOVA estimates under the RE model ---","\n")
      
      MA.random.ANCOVA
    }
    
  }) 
  
  output$ancova_re.out <- renderPrint({
    re.ancova()
  })
  
  
  # Forest plots for FE and RE 
  forest.ancova <- function(){
    
    if (input$type == "ce") {
      
      MA.fixed.ANCOVA <- ancova.FE()$MA.fixed.ANCOVA
      
      forest(MA.fixed.ANCOVA, showweights=TRUE)
    }
    
    else if (input$type == "re") {
      
      MA.random.ANCOVA <- ancova.RE()$MA.random.ANCOVA
      
      forest(MA.random.ANCOVA, showweights=TRUE)
    }
    
  }
  
  output$ancova.forest <- renderPlot(
    {
      withProgress(message = 'Rendering', detail = 'Forest plot - CE model', value = 0, {
        for (i in 1:10) {
          incProgress(1/10)
          Sys.sleep(0.05)
        }
      })
      print(forest.ancova())
    })
  
  
  output$downloadANCOVAForest <- downloadHandler(
    filename = function() {
      paste('ancova.forest', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
      pdf(file=FILE)
      print(forest.ancova())
      dev.off()
    }
  ) 
  
  
  # Funnel plots for FE and RE - ANCOVA reconstructed  
  funnel.ancova<- function(){
    
    if (input$type == "ce") {
      
      MA.fixed.ANCOVA <- ancova.FE()$MA.fixed.ANCOVA
      
      funnel(MA.fixed.ANCOVA, main="Standard Error", level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), legend=TRUE, back="cadetblue")
    }
    
    else if (input$type == "re") {
      
      MA.random.ANCOVA <- ancova.RE()$MA.random.ANCOVA
      
      funnel(MA.random.ANCOVA, main="Standard Error", level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), legend=TRUE, back="cadetblue")
    }
    
  }
  
  output$ancova.funnel <- renderPlot({
    withProgress(message = 'Rendering', detail = 'Funnel plot', value = 0, {
      for (i in 1:10) {
        incProgress(1/10)
        Sys.sleep(0.05)
      }
    })
    print(funnel.ancova())
  })
  
  
  
  output$downloadANCOVAFunnel <- downloadHandler(
    filename = function() {
      paste('ancova.funnel', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
      pdf(file=FILE)
      print(funnel.ancova())
      dev.off()
    }
  )
  
  
  
  # Print window of standard AD results -----------------------------------------------------------------------------------------------------------------------------
  observeEvent(input$print,{
    js$winprint()
  })
  
  
  # One-stage pseudo IPD main effect---------------------------------------------------------------------------------------------------------------------------------
  
  # Make the pseudo IPD as reactive data set to be used in the modelling further
  pseudoIPD <- reactive({
    
    if (is.null(analysis_data())){return(NULL)}
    analysis_data()
    df2 <- analysis_data()
    # Generate the pseudo baselines and outcomes
    data.IPD <- data.frame(study         = rep(df2$ID, df2$NCFB),
                           group         = rep(df2$group, df2$NCFB),
                           meanBaseline  = rep(df2$MeanBaseline, df2$NCFB),
                           sdBaseline    = rep(df2$sdBaseline, df2$NCFB),
                           meanPost      = rep(df2$MeanFU, df2$NCFB),
                           sdPost        = rep(df2$sdFU, df2$NCFB),
                           correlation   = rep(df2$Correlation,df2$NCFB))
    
    set.seed(123456)
    data.IPD$ytmp1 <- rnorm(nrow(data.IPD),0,1)
    set.seed(7891011)
    data.IPD$ytmp2 <- rnorm(nrow(data.IPD),0,1)
    
    # Standardize ytmp1 and ytmp2, calculate correlation between ytmp1 and ytmp2, 
    # and the residuals of regressing ytmp2 on ytmp1
    # per study and group
    
    data.IPD2 <- NULL
    for(study in unique(data.IPD$study))
    {   for (group in unique(data.IPD$group))
    { datatmp     <- data.IPD[data.IPD$study==study & data.IPD$group==group,]
    # standardized y1tmp
    datatmp$ytmp1 <- (datatmp$ytmp1-mean(datatmp$ytmp1))/sd(datatmp$ytmp1)
    # standardized y2tmp
    datatmp$ytmp2 <- (datatmp$ytmp2-mean(datatmp$ytmp2))/sd(datatmp$ytmp2)
    # correlation between y1tmp and y2tmp
    cor.ytmp      <- cor(datatmp$ytmp1, datatmp$ytmp2)
    # residuals of regression of ytmp2 on ytmp1
    resid         <- residuals(lm(ytmp2 ~ ytmp1 - 1 , data = datatmp))
    Resid <- datatmp$ytmp2 - cor.ytmp*datatmp$ytmp1
    # coefficient beta of regression of ytmp2 on ytmp1
    #coef          <- coef(lm(ytmp2 ~ ytmp1 - 1 , data = datatmp))
    data.IPD2     <- rbind( data.IPD2, data.frame(datatmp,cor.ytmp,resid,Resid))
    }  
    } 
    
    # temporary variable needed to generate the pseudo baseline and pseudo follow-up outcomes
    data.IPD2$ytmp3 <- data.IPD2$ytmp1*data.IPD2$correlation + sqrt(1-data.IPD2$correlation^2)*data.IPD2$resid/sqrt(1-data.IPD2$cor.ytmp^2)
    # generate pseudo baseline and pseudo follow-up outcomes
    data.IPD2$y1    <- data.IPD2$ytmp1*data.IPD2$sdBaseline + data.IPD2$meanBaseline
    data.IPD2$y2    <- data.IPD2$ytmp3*data.IPD2$sdPost + data.IPD2$meanPost
    
    # make new dataset, with only relevant variables
    data.pseudoIPD <- data.IPD2[,c("study", "group", "y1", "y2")]
    #View(data.pseudoIPD) # final pseudo IPD dataset 
    rm(data.IPD2,data.IPD)
    
    # Check the mean and sd of y1 and y2, and correlation y1, y2
    check <-cbind(aggregate(y1~group+study, data=data.pseudoIPD, mean), 
                  aggregate(y2~group+study, data=data.pseudoIPD, mean)[3],
                  aggregate(y1~group+study, data=data.pseudoIPD, sd)[3],
                  aggregate(y2~group+study, data=data.pseudoIPD, sd)[3],
                  as.vector(cbind(by(data.pseudoIPD, data.pseudoIPD[,c("group","study")], function(x) {cor(x$y1,x$y2)}))))
    
    colnames(check)<- c(colnames(check)[1:2], "meany1", "meany2","sdy1", "sdy2","cory1y2")
    check
    rm(check)
    
    # Pre-step to calculate centered baseline values by study
    data.pseudoIPD$meany1bystudy <- round(ave(data.pseudoIPD$y1, data.pseudoIPD$study), 4)
    data.pseudoIPD$y1center      <- round(data.pseudoIPD$y1 - data.pseudoIPD$meany1bystudy, 4)
    data.pseudoIPD$groupcenter   <- data.pseudoIPD$group - 0.5
    data.pseudoIPD$arm           <- 1000*data.pseudoIPD$study + data.pseudoIPD$group
    data.pseudoIPD$y2            <- round(data.pseudoIPD$y2, 4)
    data.pseudoIPD$y1            <- round(data.pseudoIPD$y1, 4)
    
    data.pseudoIPD
    
  })
  
  output$pseudoData <- DT::renderDataTable({
    pseudoIPD()
    if (is.null(pseudoIPD())){return(NULL)}
    DT::datatable(pseudoIPD(),
                  filter = 'top',
                  selection = list(mode = 'single', selected = 1),
                  options = list(search = list(caseInsensitive = TRUE),
                                 searchHighlight = TRUE,
                                 scrollX = TRUE,
                                 pageLength = 5))
    
  })
  
  # Output one-stage pseudo IPD main effect-------------------------------------------------------------------------------------------------------------------------------------
  
  output$one <- DT::renderDataTable(
    DT::datatable({
      
      if (is.null(pseudoIPD())){return(NULL)}
      pseudoIPD()
      df3 <- pseudoIPD()
      
      ctrl <- lmeControl(opt="optim", msMaxIter=100)
      # arm and study specific variances estimated  
      FRstudyarm <- lme(fixed=y2 ~ y1center + group + as.factor(study) + y1center*as.factor(study), random= ~ -1 + groupcenter|study,  weights =varIdent(form=~study|arm), control=ctrl, data=df3, method='REML')
      
      # study-specific variance estimates 
      FRstudy    <- lme(fixed=y2 ~ y1center+ group + as.factor(study) + y1center*as.factor(study) , random= ~ -1 + groupcenter|study, weights =varIdent(form=~1|study), control=ctrl, data=df3, method='REML')
      
      # gruop specific variance estimated 
      FRgroup    <- lme(fixed=y2 ~ y1center + group+ as.factor(study) + y1center*as.factor(study) , random= ~ -1 + groupcenter|study, weights =varIdent(form=~1|group), control=ctrl, data=df3, method='REML')
      
      #one residual variance estimated
      FRone      <- lme(fixed=y2 ~ y1center + group + as.factor(study) + y1center*as.factor(study) , random= ~-1 + groupcenter|study, control=ctrl, data=df3, method='REML')
      
      
      arm_study_specific <- round(summary(FRstudyarm)$tTable["group",1], 3) 
      se_arm_study       <- round(summary(FRstudyarm)$tTable["group",2], 3)
      study_specific     <- round(summary(FRstudy)$tTable["group",1], 3)
      se_study           <- round(summary(FRstudy)$tTable["group",2], 3)
      group_specific     <- round(summary(FRgroup)$tTable["group",1], 3)
      se_group           <- round(summary(FRgroup)$tTable["group",2], 3)
      one_variance       <- round(summary(FRone)$tTable["group",1], 3)
      se_one             <- round(summary(FRone)$tTable["group",2], 3)
      CIarm              <- data.frame(intervals(FRstudyarm, which="fixed")$fixed)
      lower_arm          <- round(CIarm["group",]$lower, 3)
      upper_arm          <- round(CIarm["group",]$upper, 3)
      CIstudy            <- data.frame(intervals(FRstudy, which="fixed")$fixed)
      lower_study        <- round(CIstudy["group",]$lower, 3)
      upper_study        <- round(CIstudy["group",]$upper, 3)
      CIgroup            <- data.frame(intervals(FRgroup, which="fixed")$fixed)
      lower_group        <- round(CIgroup["group",]$lower, 3)
      upper_group        <- round(CIgroup["group",]$upper, 3)
      CIone              <- data.frame(intervals(FRone, which="fixed")$fixed)
      lower_one          <- round(CIone["group",]$lower, 3)
      upper_one          <- round(CIone["group",]$upper, 3)
      
      
      
      table1 <- data.frame(
        Estimate = rbind(arm_study_specific,study_specific, group_specific, one_variance),
        SE       = rbind(se_arm_study, se_study, se_group, se_one),
        Lower    = rbind(lower_arm, lower_study, lower_group, lower_one),
        Upper    = rbind(upper_arm, upper_study, upper_group, upper_one)
      )
      
      names(table1)[c(1,2,3,4)] <- c("Effect estimate", "Standard error", "Lower bound of 95% CI", "Upper bound of 95% CI") 
      table1},
      
      extensions = c("Buttons", "Scroller"),
      
      options = list(
        paging =       TRUE,
        searching =    TRUE,
        fixedColumns = TRUE,
        autoWidth =    TRUE,
        ordering =     TRUE,
        dom = 'tB',
        buttons = c('copy', 'pdf', 'print')
      ),
      class="display"
    )
  )
  
  
  # Output one-stage pseudo IPD interaction effect------------------------------------------------------------------------------------------------------------------------
  
  output$oneINT <- DT::renderDataTable(
    DT::datatable({
      
      if (is.null(pseudoIPD())){return(NULL)}
      pseudoIPD()
      df3 <- pseudoIPD()
      
      ctrl <- lmeControl(opt="optim", msMaxIter=100)
      # arm and study specific variances estimated  
      FRstudyarmInt <-lme(fixed=y2 ~ y1center*as.factor(study) + y1center*group + group:meany1bystudy, random= ~ -1 + groupcenter|study,  weights =varIdent(form=~study|arm), control=ctrl, data=df3, method='REML')
      
      # study-specific variance estimates 
      FRstudyInt    <- lme(fixed=y2 ~ y1center*as.factor(study) + y1center*group + group:meany1bystudy, random= ~ -1 + groupcenter|study, weights =varIdent(form=~1|study), control=ctrl, data=df3, method='REML')
      
      # gruop specific variance estimated 
      FRgroupInt    <- lme(fixed=y2 ~ y1center*as.factor(study) + y1center*group + group:meany1bystudy, random= ~ -1 + groupcenter|study,  weights =varIdent(form=~1|group), control=ctrl, data=df3, method='REML')
      
      #one residual variance estimated
      FRoneInt      <- lme(fixed=y2 ~ y1center*as.factor(study) + y1center*group + group:meany1bystudy , random= ~ -1 + groupcenter|study, control=ctrl, data=df3, method='REML')
      
      arm_study_specificINT <- round(summary(FRstudyarmInt)$tTable["y1center:group",1], 3) 
      se_arm_studyINT       <- round(summary(FRstudyarmInt)$tTable["y1center:group",2], 3)
      study_specificINT     <- round(summary(FRstudyInt)$tTable["y1center:group",1], 3)
      se_studyINT           <- round(summary(FRstudyInt)$tTable["y1center:group",2], 3)
      group_specificINT     <- round(summary(FRgroupInt)$tTable["y1center:group",1], 3)
      se_groupINT           <- round(summary(FRgroupInt)$tTable["y1center:group",2], 3)
      one_varianceINT       <- round(summary(FRoneInt)$tTable["y1center:group",1], 3)
      se_oneINT             <- round(summary(FRoneInt)$tTable["y1center:group",2], 3)
      CIarm_INT             <- data.frame(intervals(FRstudyarmInt, which="fixed")$fixed)
      lower_armINT          <- round(CIarm_INT["y1center:group",]$lower, 3)
      upper_armINT          <- round(CIarm_INT["y1center:group",]$upper, 3)
      CIstudy_INT           <- data.frame(intervals(FRstudyInt, which="fixed")$fixed)
      lower_studyINT        <- round(CIstudy_INT["y1center:group",]$lower, 3)
      upper_studyINT        <- round(CIstudy_INT["y1center:group",]$upper, 3)
      CIgroup_INT           <- data.frame(intervals(FRgroupInt, which="fixed")$fixed)
      lower_groupINT        <- round(CIgroup_INT["y1center:group",]$lower, 3)
      upper_groupINT        <- round(CIgroup_INT["y1center:group",]$upper, 3)
      CIone_INT             <- data.frame(intervals(FRoneInt, which="fixed")$fixed)
      lower_oneINT          <- round(CIone_INT["y1center:group",]$lower, 3)
      upper_oneINT          <- round(CIone_INT["y1center:group",]$upper, 3)
      
      
      table1 <- data.frame(
        Estimate = rbind(arm_study_specificINT,study_specificINT, group_specificINT, one_varianceINT),
        SE       = rbind(se_arm_studyINT, se_studyINT, se_groupINT, se_oneINT),
        Lower    = rbind(lower_armINT, lower_studyINT, lower_groupINT, lower_oneINT),
        Upper    = rbind(upper_armINT, upper_studyINT, upper_groupINT, upper_oneINT)
      )
      
      names(table1)[c(1,2,3,4)] <- c("Interaction estimate", "Standard error", "Lower bound of 95% CI", "Upper bound of 95% CI") 
      table1},
      
      extensions = c("Buttons", "Scroller"),
      
      options = list(
        paging =       TRUE,
        searching =    TRUE,
        fixedColumns = TRUE,
        autoWidth =    TRUE,
        ordering =     TRUE,
        dom = 'tB',
        buttons = c('copy', 'pdf', 'print')
      ),
      class="display"
    )
  )
  
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Output two-stage pseudo IPD main effect 
  
  twostage_ME.FE <- reactive({
    
    if (input$type == "ce") {
      
      if (is.null(pseudoIPD())){return(NULL)}
      pseudoIPD()
      df3 <- pseudoIPD()
      
      # ANCOVA per study on pseudo IPD for subsequent two-stage MA
      coef_ancova <- NULL
      se_ancova   <- NULL
      
      for (i in unique(df3$study))
      {         fit <- lm(y2~ y1 + group, df3[df3$study==i,])
      coef_ancova   <- rbind(coef_ancova,fit$coefficients) 
      se_ancova     <- rbind(se_ancova,sqrt(diag(vcov(fit))))
      }
      
      # Prepare data for two stage MA
      two_stageMA <- data.frame(study=unique(df3$study), coef_group=coef_ancova[,"group"], secoef_group = se_ancova[,"group"])
      # Run aggregate meta-analysis 
      MA_twostageME  <- rma(yi=coef_group, sei=secoef_group, slab=study, method="FE", data=two_stageMA)
      list(MA_twostageME = MA_twostageME)
      
    }
    
  })
  
  fe.twostage_ME <- reactive({
    
    if (input$type == "ce")  {
      
      MA_twostageME  <- twostage_ME.FE()$MA_twostageME 
      
      cat("--- Mean differences based on two-stage ANCOVA estimates under the CE model ---","\n")
      
      MA_twostageME 
    }
    
  }) 
  
  output$twostageME_FE.out<- renderPrint({
    fe.twostage_ME()
  })
  
  
  twostage_ME.RE <- reactive({
    
    if (input$type == "re") {
      
      if (is.null(pseudoIPD())){return(NULL)}
      pseudoIPD()
      df3 <- pseudoIPD()
      
      # ANCOVA per study on pseudo IPD for subsequent two-stage MA
      coef_ancova <- NULL
      se_ancova   <- NULL
      
      for (i in unique(df3$study ))
      {         fit <- lm(y2~ y1 + group, df3[df3$study==i,])
      coef_ancova   <- rbind(coef_ancova,fit$coefficients) 
      se_ancova     <- rbind(se_ancova,sqrt(diag(vcov(fit))))
      }
      
      # Prepare data for two stage MA
      two_stageMA <- data.frame(study=unique(df3$study), coef_group=coef_ancova[,"group"], secoef_group = se_ancova[,"group"])
      # Run aggregate meta-analysis 
      MA_twostageMEre  <- rma(yi=coef_group, sei=secoef_group, slab=study, method="REML", data=two_stageMA, knha=input$HK)
      list(MA_twostageMEre = MA_twostageMEre)
      
    }
    
  })
  
  re.twostage_ME <- reactive({
    
    if (input$type == "re")  {
      
      MA_twostageMEre  <- twostage_ME.RE()$MA_twostageMEre 
      
      cat("--- Mean differences based on two-stage ANCOVA estimates under the RE model ---","\n")
      
      MA_twostageMEre 
    }
    
  }) 
  
  output$twostageME_RE.out<- renderPrint({
    re.twostage_ME()
  })
  
  
  # Forest plot of two stage approach for the main effect ------------------
  
  forest_twostageME = function(){
    
    if (input$type == "ce") {
      MA_twostageME <- twostage_ME.FE()$MA_twostageME
      forest(MA_twostageME)
      
    }
    
    if (input$type == "re") {
      
      MA_twostageMEre <- twostage_ME.RE()$MA_twostageMEre
      forest(MA_twostageMEre)
      
    }
  }
  
  output$forest_twoME<- renderPlot(
    {
      withProgress(message = 'Rendering', detail = 'Forest plot - Two-stage analysis', value = 0, {
        for (i in 1:10) {
          incProgress(1/10)
          Sys.sleep(0.05)
        }
      })
      
      forest_twostageME()
      
    }) 
  
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("Twostage_main.effect", Sys.Date(), sep='')
    },
    content = function(file){
      if(input$format == "png")
        png(file)
      if(input$format == "pdf")
        pdf(file)
      print(forest_twostageME())
      dev.off()
    }
  )
  
  # Funnel plot of two stage approach for the main effect ------------------
  
  funnel_twostageME = function(){
    
    if (input$type == "ce") {
      MA_twostageME <- twostage_ME.FE()$MA_twostageME
      funnel(MA_twostageME, main="Standard Error", level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), legend=TRUE, back="cadetblue")
      
    }
    
    if (input$type == "re") {
      
      MA_twostageMEre <- twostage_ME.RE()$MA_twostageMEre
      funnel(MA_twostageMEre, main="Standard Error", level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), legend=TRUE, back="cadetblue")
      
    }
  }
  
  output$funnel_twoME<- renderPlot(
    {
      withProgress(message = 'Rendering', detail = 'Funnel plot - Two-stage analysis', value = 0, {
        for (i in 1:10) {
          incProgress(1/10)
          Sys.sleep(0.05)
        }
      })
      
      funnel_twostageME()
      
    })
  
  
  output$downloadFunnel<- downloadHandler(
    filename = function() {
      paste("Funnel main.effect", Sys.Date(), sep='')
    },
    content = function(file){
      if(input$format == "png")
        png(file)
      if(input$format == "pdf")
        pdf(file)
      print(funnel_twostageME())
      dev.off()
    }
  )
  
  
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Output two-stage pseudo IPD interaction effect 
  
  twostage_ME.FEint<- reactive({
    
    if (input$type == "ce") {
      
      if (is.null(pseudoIPD())){return(NULL)}
      pseudoIPD()
      df3 <- pseudoIPD()
      
      # ANCOVA per study on pseudo IPD for subsequent two-stage MA
      coef_ancova_int <- NULL
      se_ancova_int   <- NULL
      
      for (i in unique(df3$study))
      {         fit   <- lm(y2 ~ y1center + group + y1center*group, df3[df3$study==i,])
      coef_ancova_int <- rbind(coef_ancova_int,fit$coefficients) 
      se_ancova_int   <- rbind(se_ancova_int,sqrt(diag(vcov(fit))))
      }
      
      # Prepare data for two stage MA
      two_stageMA_int <- data.frame(study=unique(df3$study), coef_group=coef_ancova_int[,"y1center:group"], secoef_group = se_ancova_int[,"y1center:group"])
      # Run aggregate meta-analysis 
      MA_int  <- rma(yi=coef_group, sei=secoef_group, slab=study, method="FE", data=two_stageMA_int)
      list(MA_int = MA_int)
      
    }
    
  })
  
  fe.twostage_MEint <- reactive({
    
    if (input$type == "ce")  {
      
      MA_int <- twostage_ME.FEint()$MA_int 
      
      cat("--- Interaction effect based on two-stage estimates under the CE model ---","\n")
      
      MA_int
    }
    
  }) 
  
  output$twostageME_FEint.out<- renderPrint({
    fe.twostage_MEint()
  })
  
  
  twostage_ME.REint<- reactive({
    
    if (input$type == "re") {
      
      if (is.null(pseudoIPD())){return(NULL)}
      pseudoIPD()
      df3 <- pseudoIPD()
      
      # ANCOVA per study on pseudo IPD for subsequent two-stage MA
      coef_ancova_int <- NULL
      se_ancova_int   <- NULL
      
      for (i in unique(df3$study))
      {         fit   <- lm(y2 ~ y1center + group + y1center*group, df3[df3$study==i,])
      coef_ancova_int <- rbind(coef_ancova_int,fit$coefficients) 
      se_ancova_int   <- rbind(se_ancova_int,sqrt(diag(vcov(fit))))
      }
      
      # Prepare data for two stage MA
      two_stageMA_int <- data.frame(study=unique(df3$study), coef_group=coef_ancova_int[,"y1center:group"], secoef_group = se_ancova_int[,"y1center:group"])
      # Run aggregate meta-analysis 
      MA_int.re  <- rma(yi=coef_group, sei=secoef_group, slab=study, method="REML", data=two_stageMA_int,  knha=input$HK)
      list(MA_int.re = MA_int.re)
      
    }
    
  })
  
  re.twostage_MEint <- reactive({
    
    if (input$type == "re")  {
      
      MA_int.re <- twostage_ME.REint()$MA_int.re 
      
      cat("--- Interaction effect based on two-stage estimates under the RE model ---","\n")
      
      MA_int.re
    }
    
  }) 
  
  output$twostageME_REint.out<- renderPrint({
    re.twostage_MEint()
  })
  
  
  
  # Forest plot of two stage approach for the interaction effect -----------------
  
  forest_twostageME.int = function(){
    
    if (input$type == "ce") {
      MA_int <- twostage_ME.FEint()$MA_int 
      forest(MA_int)
    }
    
    if (input$type == "re") {
      MA_int.re <- twostage_ME.REint()$MA_int.re
      forest(MA_int.re)
    }
  }
  
  output$forest_twoMEint<- renderPlot(
    {
      withProgress(message = 'Rendering', detail = 'Forest plot - Two-stage interaction', value = 0, {
        for (i in 1:10) {
          incProgress(1/10)
          Sys.sleep(0.05)
        }
      })
      
      forest_twostageME.int()
      
    }) 
  
  
  output$downloadPlotInt <- downloadHandler(
    filename = function() {
      paste("Twostage_interaction", Sys.Date(), sep='')
    },
    content = function(file){
      if(input$format == "png")
        png(file)
      if(input$format == "pdf")
        pdf(file)
      print(forest_twostageME.int())
      dev.off()
    }
  )
  
  # Funnel plot of two stage approach for the interaction effect ------------------
  
  funnel_twostageME.int = function(){
    
    if (input$type == "ce") {
      MA_int <- twostage_ME.FEint()$MA_int 
      funnel(MA_int, main="Standard Error", level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), legend=TRUE, back="cadetblue")
    }
    
    if (input$type == "re") {
      MA_int.re <- twostage_ME.REint()$MA_int.re
      funnel(MA_int.re, main="Standard Error", level=c(90, 95, 99), shade=c("white", "gray55", "gray75"), legend=TRUE, back="cadetblue")
    }
  }
  
  output$funnel_twoMEint<- renderPlot(
    {
      withProgress(message = 'Rendering', detail = 'Forest plot - Two-stage interaction', value = 0, {
        for (i in 1:10) {
          incProgress(1/10)
          Sys.sleep(0.05)
        }
      })
      
      funnel_twostageME.int()
      
    }) 
  
  output$downloadFunnelInt <- downloadHandler(
    filename = function() {
      paste("Twostage_Funnel", Sys.Date(), sep='', input$format_funnelINT)
    },
    content = function(file){
      if (input$format_funnelINT=="PDF"){pdf(file=file)}
      else {png(file=file)}
      print(funnel_twostageME.int())
      dev.off()
    }
  ) 
  # output$downloadFunnelInt <- downloadHandler(
  #   filename = function() {
  #     paste("Twostage_Funnel", Sys.Date(), sep='')
  #   },
  #   content = function(file){
  #     if(input$format == "png")
  #       png(file)
  #     if(input$format == "pdf")
  #       pdf(file)
  #     print(funnel_twostageME.int())
  #     dev.off()
  #   }
  # )
  
  
  
  
  
  
}
)