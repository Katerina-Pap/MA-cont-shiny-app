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
    h3("How to use the tool? Watch the video!", align="center", tags$video(type = "video/mp4", src = "instructions.mp4", width="350px", height="350px", controls = "controls",  style="display: block; margin-left: auto; margin-right: auto;"))
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
  
#rv <- reactiveValues()
  
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
    DT::datatable(df_upload(),
                  filter = 'top',
                  selection = list(mode = 'single', selected = 1),
                  options = list(search = list(caseInsensitive = TRUE),
                                 searchHighlight = TRUE,
                                 scrollX = TRUE,
                                 pageLength = 10))
  })
  
  # Print data structure ---------------------------------------------------------------------------------------------------------------------------------
  output$structure <- renderPrint({
                      req(df_upload())
                      Dataset <- df_upload() # renaming dataset to appear better in the table
                      skim(Dataset)
  })
  
  
# Need to build multiple reactive dataframes to make the final imputed one because of over-writing variable names

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
                   mutate (MeanFU     = ifelse(is.na(rv$MeanFU), rv$MeanCFB + rv$MeanBaseline, rv$MeanFU),
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
  
  # Dataset 3: Assume SDs equal pre and post ---------------------------------------------------------------------------------------------------------------------
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
    #coalsce(df3(),df1())
  })
  
  
  # Dataset 4: Calculate Correlations from SDs-------------------------------------------------------------------------------------------------------------------
  df4 <- eventReactive(input$correl, {
    df3() %>%
      mutate ( Correlation = ifelse(is.na(Correlation), (sdBaseline^2+sdFU^2-sdCFB^2)/(2*sdBaseline*sdFU), Correlation)
      )
  })
  
  # Output table of correlation actiobutton   
  output$correl_output<-renderTable({
    if(is.null(df4()))
    {return(NULL)}
    df4()
    #coalsce(df4(),df1())
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
  
  # Output table of final calculations  - if missing data 
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
  
 # The output table with the final dataset for analysis     
   output$final_data<-renderTable({
     if(is.null(analysis_data()))
         {return(NULL)}
          analysis_data()
   })
#----------------------------------------------------------------------------------------------------------------------------------------------------------------    
# Output Standard AD analyses
 
  # Output final scores analysis ----------------------------------------------------------------
  
  # FE analysis results 
  final.FE <- reactive({
    
    if (input$type == "ce") {
      
      if (is.null(analysis_data())){return(NULL)}
        analysis_data()
        dd <- analysis_data()
        # For the first three methods the data need to be in wide format
        drop         <- which(colnames( dd) %in% "Study")
        dd           <- dd[,-drop]
        data.AD_wide <- dcast(melt(dd, id.vars=c("ID", "group")), ID~variable+group)

        MA.fixed.final <- rma(m1i=MeanFU_1, m2i=MeanFU_0, sd1i=sdFU_1, sd2i=sdFU_0, n1i=NCFB_1, n2i=NCFB_0, data=data.AD_wide, measure="MD", method="FE")
        #list(MA.fixed.final=MA.fixed.final) 
        MA.fixed.final
    }
    
  })
  
  output$final_fe.out <- renderPrint({
    cat("--- Mean Differences of Final scores ---", "\n")
        final.FE()
  })
  
  # RE analysis results 
   final.RE <- reactive({
    
    if (input$type == "re")  {
      
      if (is.null(analysis_data())){return(NULL)}
      analysis_data()
      dd <- analysis_data()
      # For the first three methods the data need to be in wide format
      drop         <- which(colnames( dd) %in% "Study")
      dd           <-  dd[,-drop]
      data.AD_wide <- dcast(melt(dd, id.vars=c("ID", "group")), ID~variable+group)
      
      MA.random.final <- rma(m1i=MeanFU_1, m2i=MeanFU_0, sd1i=sdFU_1, sd2i=sdFU_0, n1i=NCFB_1, n2i=NCFB_0, 
                             data=data.AD_wide, measure="MD", method="REML", knha=input$HK)
      # list(MA.random.final=MA.random.final) 
      MA.random.final
      
    }
    
  })
  
  output$final_re.out<- renderPrint({
      cat("--- Mean Differences of Final scores ---","\n")
          final.RE()
  })
  
  # Output change scores analysis --------------------------------------------------------------------------------------------------------------------------------

  # FE analysis results 
  change.FE <- reactive({
    
    if (input$type == "ce") {
      
      if (is.null(analysis_data())){return(NULL)}
      analysis_data()
      dd <- analysis_data()
      # For the first three methods the data need to be in wide format
      drop         <- which(colnames( dd) %in% "Study")
      dd           <-  dd[,-drop]
      data.AD_wide <- dcast(melt(dd, id.vars=c("ID", "group")), ID~variable+group)
      
      MA.fixed.change <- rma(m1i=MeanCFB_1, m2i=MeanCFB_0, sd1i=sdCFB_1, sd2i=sdCFB_0, n1i=NCFB_1, n2i=NCFB_0,
                                                             data=data.AD_wide, measure="MD", method="FE")
      list(MA.fixed.change=MA.fixed.change) 
      
    }
    
  })
  
  output$change_fe.out <- renderPrint({
    change.FE()
  })
  
  # RE analysis results 
  change.RE <- reactive({
    
    if (input$type == "re") {
      
      if (is.null(analysis_data())){return(NULL)}
      analysis_data()
      dd <- analysis_data()
      # For the first three methods the data need to be in wide format
      drop         <- which(colnames( dd) %in% "Study")
      dd           <-  dd[,-drop]
      data.AD_wide <- dcast(melt(dd, id.vars=c("ID", "group")), ID~variable+group)
      
      MA.random.change <- rma(m1i=MeanCFB_1, m2i=MeanCFB_0, sd1i=sdCFB_1, sd2i=sdCFB_0, n1i=NCFB_1, n2i=NCFB_0,
                             data=data.AD_wide, measure="MD", method="REML", knha=input$HK)
      #list(MA.random.change=MA.random.change) 
      MA.random.change
      
    }
    
  })
  
  output$change_re.out <- renderPrint({
    change.RE()
  })
  
  # Output recovered ANCOVA approach -------------------------------------------------------------------------------------------------------------------------------
  
  # FE analysis results 
  ancova.FE <- reactive({
    
    if (input$type == "ce") {
      
      if (is.null(analysis_data())){return(NULL)}
      analysis_data()
      dd <- analysis_data()
      
     # calculate pooled standard deviations of baseline and follow-up values
     # For the first three methods the data need to be in wide format
        drop         <- which(colnames( dd) %in% "Study")
        dd           <-  dd[,-drop]
        data.AD_wide <- dcast(melt( dd, id.vars=c("ID", "group")), ID~variable+group)

        sdpooledB <- with(data.AD_wide, sqrt((((NCFB_1 - 1)*(sdBaseline_1^2)) + (NCFB_0 - 1)*(sdBaseline_0^2))/((NCFB_1+NCFB_0)-2)))
        sdpooledF <- with(data.AD_wide, sqrt((((NCFB_1 - 1)*(sdFU_1^2)) + (NCFB_0 - 1)*(sdFU_0^2))/((NCFB_1+NCFB_0)-2)))

        # Calculate ancova estimate using formula from Senn et al. 2007
        # using the pooled correlation

        ripooled <- with(data.AD_wide, ((NCFB_1*Correlation_1*sdBaseline_1*sdFU_1 +  NCFB_0*Correlation_0 *sdBaseline_0*sdFU_0) )
                         /((NCFB_1+NCFB_0)*sdpooledB*sdpooledF))

        # ripooled <- with(data.AD_wide, ((NCFB_1*Correlation_1*sdBaseline_1*sdFU_1 +  NCFB_0*Correlation_0 *sdBaseline_0*sdFU_0) )
        #                  /((NCFB_1+NCFB_0)*sdpooledB*sdpooledF))

        ancova_est      <- with(data.AD_wide, (MeanFU_1-MeanFU_0)-ripooled*(sdpooledF/sdpooledB)*(MeanBaseline_1-MeanBaseline_0))

        var_ancova_est  <- with(data.AD_wide, sdpooledF^2*(1/NCFB_1)+sdpooledF^2*(1/NCFB_0))*(1-ripooled^2) # for different sample sizes from McKenzie and from Senn

        se_ancovas_est  <- with(data.AD_wide,sqrt(var_ancova_est))

        MA.fixed.ANCOVA <- rma(yi=ancova_est, sei=se_ancovas_est, method="FE")
        #list(MA.fixed.ANCOVA=MA.fixed.ANCOVA) 
        MA.fixed.ANCOVA
      
    }
    
  })
  
  output$ancova_fe.out <- renderPrint({
    ancova.FE()
  })
  
  
  # RE analysis results 
  ancova.RE <- reactive({
    
    if (input$type == "re") {
      if (is.null(analysis_data())){return(NULL)}
      analysis_data()
      dd <- analysis_data()
      # calculate pooled standard deviations of baseline and follow-up values
      # For the first three methods the data need to be in wide format
      drop         <- which(colnames( dd) %in% "Study")
      dd           <-  dd[,-drop]
      data.AD_wide <- dcast(melt( dd, id.vars=c("ID", "group")), ID~variable+group)
      
      sdpooledB <- with(data.AD_wide, sqrt((((NCFB_1 - 1)*(sdBaseline_1^2)) + (NCFB_0 - 1)*(sdBaseline_0^2))/((NCFB_1+NCFB_0)-2)))
      sdpooledF <- with(data.AD_wide, sqrt((((NCFB_1 - 1)*(sdFU_1^2)) + (NCFB_0 - 1)*(sdFU_0^2))/((NCFB_1+NCFB_0)-2)))
      
      # Calculate ancova estimate using formula from Senn et al. 2007
      # using the pooled correlation
      
      ripooled <- with(data.AD_wide, ((NCFB_1*Correlation_1*sdBaseline_1*sdFU_1 +  NCFB_0*Correlation_0 *sdBaseline_0*sdFU_0) )
                       /((NCFB_1+NCFB_0)*sdpooledB*sdpooledF))
      
      # ripooled <- with(data.AD_wide, ((NCFB_1*Correlation_1*sdBaseline_1*sdFU_1 +  NCFB_0*Correlation_0 *sdBaseline_0*sdFU_0) )
      #                  /((NCFB_1+NCFB_0)*sdpooledB*sdpooledF))
      
      ancova_est      <- with(data.AD_wide, (MeanFU_1-MeanFU_0)-ripooled*(sdpooledF/sdpooledB)*(MeanBaseline_1-MeanBaseline_0))
      
      var_ancova_est  <- with(data.AD_wide, sdpooledF^2*(1/NCFB_1)+sdpooledF^2*(1/NCFB_0))*(1-ripooled^2) # for different sample sizes from McKenzie and from Senn
      
      se_ancovas_est  <- with(data.AD_wide,sqrt(var_ancova_est))
      
      MA.random.ANCOVA <- rma(yi=ancova_est, sei=se_ancovas_est, method="REML", knha=input$HK)
      #list(MA.random.ANCOVA=MA.random.ANCOVA) 
      MA.random.ANCOVA
      
    }
    
  })
  
  output$ancova_re.out <- renderPrint({
    ancova.RE()
  })
  
  # Print window of standard AD results -----------------------------------------------------------------------------------------------------------------------------
  observeEvent(input$print,{
    js$winprint()
  })
  
  #------------------------------------------------------------------------------------------------------------------------------------------------------------------
 
  # Make pseudo IPD as reactive data 
  
  # Output one-stage pseudo IPD main effect--------------------------------------------------------------------------------------------------------------------------
  
  output$one <- DT::renderDataTable(
    DT::datatable(
      {
        #renderPrint({
    if (is.null(analysis_data())){return(NULL)}
     analysis_data()
     dd2 <- analysis_data()
    # Generate the pseudo baselines and outcomes
    data.IPD <- data.frame(study         = rep(dd2$ID, dd2$NCFB),
                           group         = rep(dd2$group, dd2$NCFB),
                           meanBaseline  = rep(dd2$MeanBaseline, dd2$NCFB),
                           sdBaseline    = rep(dd2$sdBaseline, dd2$NCFB),
                           meanPost      = rep(dd2$MeanFU, dd2$NCFB),
                           sdPost        = rep(dd2$sdFU, dd2$NCFB),
                           correlation   = rep(dd2$Correlation,dd2$NCFB))
    
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
    #coef         <- coef(lm(ytmp2 ~ ytmp1 - 1 , data = datatmp))
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
    data.pseudoIPD$meany1bystudy <- ave(data.pseudoIPD$y1, data.pseudoIPD$study)
    data.pseudoIPD$y1center      <- data.pseudoIPD$y1 - data.pseudoIPD$meany1bystudy
    data.pseudoIPD$groupcenter   <- data.pseudoIPD$group - 0.5
    data.pseudoIPD$arm           <- 1000*data.pseudoIPD$study + data.pseudoIPD$group
    
    
    ctrl <- lmeControl(opt="optim", msMaxIter=100)
    # arm- and study-specific variances estimated  
    FRstudyarm    <- lme(fixed=y2 ~ y1center + group + as.factor(study) + y1center*as.factor(study), random= ~ -1 + groupcenter|study,
                         weights =varIdent(form=~study|arm), control=ctrl,
                         data=data.pseudoIPD, method='REML')
    
    # study-specific variance estimates 
    FRstudy       <-  lme(fixed=y2 ~ y1center+ group + as.factor(study) + y1center*as.factor(study) , random= ~ -1 + groupcenter|study,
                          weights =varIdent(form=~1|study), control=ctrl, data=data.pseudoIPD, method='REML')
    
    summary(FRstudy)$tTable["group",1]
    
    # gruop specific variance estimated 
    FRgroup      <-   lme(fixed=y2 ~ y1center + group+ as.factor(study) + y1center*as.factor(study) , random= ~ -1 + groupcenter|study,
                          weights =varIdent(form=~1|group), control=ctrl, data=data.pseudoIPD, method='REML')
    
    # one residual variance estimated
    FRone        <-   lme(fixed=y2 ~ y1center + group + as.factor(study) + y1center*as.factor(study) , random= ~-1 + groupcenter|study,
                          control=ctrl, data=data.pseudoIPD, method='REML')
    
    
    arm_study_specific <- round(summary(FRstudyarm)$tTable["group",1], 3) 
    se_arm_study       <- round(summary(FRstudyarm)$tTable["group",2], 3)
    study_specific     <- round(summary(FRstudy)$tTable["group",1], 3)
    se_study           <- round(summary(FRstudy)$tTable["group",2], 3)
    group_specific     <- round(summary(FRgroup)$tTable["group",1], 3)
    se_group           <- round(summary(FRgroup)$tTable["group",2], 3)
    one_variance       <- round(summary(FRone)$tTable["group",1], 3)
    se_one             <- round(summary(FRone)$tTable["group",2], 3)
    
      
    table1 <- data.frame(
      Estimate = rbind(arm_study_specific,study_specific, group_specific, one_variance),
      SE       = rbind(se_arm_study, se_study, se_group, se_one)
    )
    
    table1},
    
    extensions = c("Buttons", "Scroller"),

       options = list(
         paging = TRUE,
         searching = TRUE,
         fixedColumns = TRUE,
         autoWidth = TRUE,
         ordering = TRUE,
         dom = 'tB',
         buttons = c('copy', 'pdf', 'print')
         ),
       class="display"
       ))

  #})
  
# Output one-stage pseudo IPD main effect--------------------------------------------------------------------------------------------------------------------------
  
  output$oneINT <- DT::renderDataTable(
    DT::datatable(
      {
        #renderPrint({
        if (is.null(analysis_data())){return(NULL)}
        analysis_data()
        dd2 <- analysis_data()
        # Generate the pseudo baselines and outcomes
        data.IPD <- data.frame(study         = rep(dd2$ID, dd2$NCFB),
                               group         = rep(dd2$group, dd2$NCFB),
                               meanBaseline  = rep(dd2$MeanBaseline, dd2$NCFB),
                               sdBaseline    = rep(dd2$sdBaseline, dd2$NCFB),
                               meanPost      = rep(dd2$MeanFU, dd2$NCFB),
                               sdPost        = rep(dd2$sdFU, dd2$NCFB),
                               correlation   = rep(dd2$Correlation,dd2$NCFB))
        
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
        data.pseudoIPD$meany1bystudy <- ave(data.pseudoIPD$y1, data.pseudoIPD$study)
        data.pseudoIPD$y1center      <- data.pseudoIPD$y1 - data.pseudoIPD$meany1bystudy
        data.pseudoIPD$groupcenter   <- data.pseudoIPD$group - 0.5
        data.pseudoIPD$arm           <- 1000*data.pseudoIPD$study + data.pseudoIPD$group
        
        
        ctrl <- lmeControl(opt="optim", msMaxIter=100)
        # arm and study specific variances estimated  
        FRstudyarmInt <- lme(fixed=y2 ~ y1center*as.factor(study) + y1center*group + group:meany1bystudy, random= ~ -1 + groupcenter|study,
                             weights =varIdent(form=~study|arm), control=ctrl,
                             data=data.pseudoIPD, method='REML')
        
        # study-specific variance estimates 
        FRstudyInt   <-   lme(fixed=y2 ~ y1center*as.factor(study) + y1center*group + group:meany1bystudy, random= ~ -1 + groupcenter|study,
                              weights =varIdent(form=~1|study), control=ctrl,
                              data=data.pseudoIPD, method='REML')
        
        
        # gruop specific variance estimated 
        FRgroupInt   <-   lme(fixed=y2 ~ y1center*as.factor(study) + y1center*group + group:meany1bystudy, random= ~ -1 + groupcenter|study,
                              weights =varIdent(form=~1|group), control=ctrl,
                              data=data.pseudoIPD, method='REML')
        
        #one residual variance estimated
        FRoneInt     <-   lme(fixed=y2 ~ y1center*as.factor(study) + y1center*group + group:meany1bystudy , random= ~ -1 + groupcenter|study,
                              control=ctrl,data=data.pseudoIPD, method='REML')
        
        
        arm_study_specificINT <- round(summary(FRstudyarmInt)$tTable["y1center:group",1], 3) 
        se_arm_studyINT       <- round(summary(FRstudyarmInt)$tTable["y1center:group",2], 3)
        study_specificINT     <- round(summary(FRstudyInt)$tTable["y1center:group",1], 3)
        se_studyINT           <- round(summary(FRstudyInt)$tTable["y1center:group",2], 3)
        group_specificINT     <- round(summary(FRgroupInt)$tTable["y1center:group",1], 3)
        se_groupINT           <- round(summary(FRgroupInt)$tTable["y1center:group",2], 3)
        one_varianceINT       <- round(summary(FRoneInt)$tTable["y1center:group",1], 3)
        se_oneINT             <- round(summary(FRoneInt)$tTable["y1center:group",2], 3)
        
        
        table1 <- data.frame(
          Estimate = rbind(arm_study_specificINT,study_specificINT, group_specificINT, one_varianceINT),
          SE       = rbind(se_arm_studyINT, se_studyINT, se_groupINT, se_oneINT)
        )
        
        table1}
      
      # extensions = c("Buttons", "Scroller"),
      # 
      # options = list(
      #   paging = TRUE,
      #   searching = TRUE,
      #   fixedColumns = TRUE,
      #   autoWidth = TRUE,
      #   ordering = TRUE,
      #   dom = 'tB',
      #   buttons = c('copy', 'pdf', 'print')
      # ),
      # class="display"
    ))
  
  #---------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Output two-stage pseudo IPD
  
  twostage_ME.FE <- reactive({
    
    if (input$type == "ce") {
      
      if (is.null(analysis_data())){return(NULL)}
      analysis_data()
      dd2 <- analysis_data()
      # Generate the pseudo baselines and outcomes
      data.IPD <- data.frame(study         = rep(dd2$Study, dd2$NCFB),
                             group         = rep(dd2$group, dd2$NCFB),
                             meanBaseline  = rep(dd2$MeanBaseline, dd2$NCFB),
                             sdBaseline    = rep(dd2$sdBaseline,   dd2$NCFB),
                             meanPost      = rep(dd2$MeanFU, dd2$NCFB),
                             sdPost        = rep(dd2$sdFU,   dd2$NCFB),
                             correlation   = rep(dd2$Correlation, dd2$NCFB))
      
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
      #coef         <- coef(lm(ytmp2 ~ ytmp1 - 1 , data = datatmp))
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
      data.pseudoIPD$meany1bystudy <- ave(data.pseudoIPD$y1, data.pseudoIPD$study)
      data.pseudoIPD$y1center      <- data.pseudoIPD$y1 - data.pseudoIPD$meany1bystudy
      data.pseudoIPD$groupcenter   <- data.pseudoIPD$group - 0.5
      data.pseudoIPD$arm           <- 1000*data.pseudoIPD$study + data.pseudoIPD$group
      
      # ANCOVA per study on pseudo IPD for subsequent two-stage MA
      
      coef_ancova <- NULL
      se_ancova   <- NULL
      
      for (i in unique(data.pseudoIPD$study ))
      {         fit <- lm(y2~ y1 + group, data.pseudoIPD[data.pseudoIPD$study==i,])
      coef_ancova   <- rbind(coef_ancova,fit$coefficients) 
      se_ancova     <- rbind(se_ancova,sqrt(diag(vcov(fit))))
      }
      
      # Prepare data for two stage MA
      two_stageMA <- data.frame(study=unique(data.pseudoIPD$study), coef_group=coef_ancova[,"group"], secoef_group = se_ancova[,"group"])
      
      # Run aggregate meta-analysis 
      MA_twostageME  <- rma(yi=coef_group, sei=secoef_group, slab=study, method="FE", data=two_stageMA, knha=FALSE)
      list(MA_twostageME = MA_twostageME)
      
    }
    
  })
  
  output$twostageME_FE.out<- renderPrint({
    twostage_ME.FE()
  })
  
  
  twostage_ME.RE <- reactive({
    
    if (input$type == "re") {
      
      if (is.null(analysis_data())){return(NULL)}
      analysis_data()
      dd2 <- analysis_data()
      # Generate the pseudo baselines and outcomes
      data.IPD <- data.frame(study         = rep(dd2$Study, dd2$NCFB),
                             group         = rep(dd2$group, dd2$NCFB),
                             meanBaseline  = rep(dd2$MeanBaseline, dd2$NCFB),
                             sdBaseline    = rep(dd2$sdBaseline, dd2$NCFB),
                             meanPost      = rep(dd2$MeanFU, dd2$NCFB),
                             sdPost        = rep(dd2$sdFU, dd2$NCFB),
                             correlation   = rep(dd2$Correlation,dd2$NCFB))
      
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
      data.pseudoIPD$meany1bystudy <- ave(data.pseudoIPD$y1, data.pseudoIPD$study)
      data.pseudoIPD$y1center      <- data.pseudoIPD$y1 - data.pseudoIPD$meany1bystudy
      data.pseudoIPD$groupcenter   <- data.pseudoIPD$group - 0.5
      data.pseudoIPD$arm           <- 1000*data.pseudoIPD$study + data.pseudoIPD$group
      
      # ANCOVA per study on pseudo IPD for subsequent two-stage MA
      
      coef_ancova <- NULL
      se_ancova   <- NULL
      
      for (i in unique(data.pseudoIPD$study ))
      {         fit <- lm(y2 ~ y1 + group, data.pseudoIPD[data.pseudoIPD$study==i,])
      coef_ancova   <- rbind(coef_ancova,fit$coefficients) 
      se_ancova     <- rbind(se_ancova,sqrt(diag(vcov(fit))))
      }
      
      # Prepare data for two stage MA
      two_stageMA <- data.frame(study=unique(data.pseudoIPD$study), coef_group=coef_ancova[,"group"], secoef_group = se_ancova[,"group"])
      
      # Run aggregate meta-analysis 
      MA_twostageME  <- rma(yi=coef_group, sei=secoef_group, slab=study, method="REML", data=two_stageMA, knha=input$HK)
      list(MA_twostageME = MA_twostageME)
      
    }
    
  })
  
  output$twostageME_RE.out<- renderPrint({
    twostage_ME.RE()
  })
  
  
  forest_twostageME = function(){
    
    if (input$type == "ce") {
      MA_twostageME <- twostage_ME.FE()$MA_twostageME
      forest(MA_twostageME)
      
    }
    
    
    if (input$type == "re") {
      
      MA_twostageME <- twostage_ME.RE()$MA_twostageME
      forest(MA_twostageME)
      
    }
  }
  
  output$forest_twoME<- renderPlot(
    {
      withProgress(message = 'Rendering', detail = 'Forest plot - Two-stage analysis', value = 0, {
        for (i in 1:2) {
          incProgress(1/2)
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
  
  
  
  # output$downloadtwoMEPlot <- downloadHandler(
  #   filename = function() {
  #     paste('Twostage_main.effect', Sys.Date(), '.pdf', sep='')
  #   },
  #   content = function(FILE=NULL) {
  #     pdf(file=FILE)
  #     print(forest_twostageME())
  #     dev.off()
  #   }
  # )
  # 
  # output$downloadtwoMEPlotpng <- downloadHandler(
  #   filename = function() {
  #     paste('forest_twoME', Sys.Date(), '.png', sep='')
  #   },
  #   content = function(file) {
  #     png(file=FILE)
  #     print(forest_twostageME())
  #     dev.off()
  #   },
  #   contentType = 'image/png'
  # )
  
  
}
)