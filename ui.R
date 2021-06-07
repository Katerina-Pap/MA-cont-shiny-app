
#UI functions #-----------------------------------------------------------------------------------------------------------------------------------------------------

source("library.R")

# function to print table of results 
jsCode <- 'shinyjs.winprint = function(){
window.print();
}'

# Define UI --------------------------------------------------------------------------------------------------------------------------------------------------------
shinyUI(

  
  navbarPage(
    id    = "mytabsetpanel",
    theme = shinytheme("flatly"), 
    title = "MA-cont: pre/post effect size",
    # Begin overview of tabs-------------------------------------------------------------------------------
    tabPanel("Home", icon = icon("far fa-home"),
             h1(strong("MA-cont: pre/post effect size")),               
             h3("Run meta-analysis of continuous outcomes measured at baseline and follow-up"),
             br(),
             fluidRow(
               column(
                 8,
                 title = "About",
                 id    = "home_about",
                 sidebarLayout(
                   sidebarPanel(
                     h3(strong("About")),
                     p(
                       em("MA-cont: pre/post effect size"),
                       "enables the meta-analysis of continuous data using standard AD methods, and pseudo IPD methods, accounting for the baseline measurements. Treatment-by-baseline effects can be
                       additionally explored."
                       ),
                    # br(),
                     hr(),
                   
                     h3(strong("Request a new feature")),
                     p(
                       "Please ",
                       a(href = "mailto:katerina.papadimitropoulou@gmail.com", "email me")
                     ),
                     p(strong("OR")),
                     p(
                       "Open an issue on",
                       a(href = "https://github.com/Katerina-Pap/MA-cont-shiny-app/issues", "GitHub")
                     ),
                    
                    br(),
                    h4(textOutput("counter")) # to include number of users 
                  
                     
                   ),
                   mainPanel(
                     fluidRow(
                       column(width = 6,  h3(strong("Getting started")) ),
                      
                       column(
                         width = 6,
                         align = "right",
                         br(),
                    
                         actionButton("gotodata", "Upload your data", icon("paper-plane"), 
                                      style="color: #FFFFFF; background-color:#91d1c2ff; border-color: #91d1c2ff")
                       ),
                       br(),
                       br(),
                       br(),
                  includeMarkdown("overview.md"), # to uncomment out later
                  )
               )
            )
          ),  # ends column after fluidRow
               column(
                 4,
                 align="right", 
                 br(),
                 h3(strong("How to use the tool? Watch the video !"), align="center"),
                 uiOutput("video")
               )
            )
             
             
                     
    ),
    

 # Tab 2: Data upload with -----------------------------------------------------------------------------
    tabPanel("Data upload & pre-processing", icon = icon("table"), value="data",
             
           #tabsetPanel(type = "tabs",
             tabPanel("",
                      fluidPage(tags$style(HTML('body {font-family:"arial",Georgia,Serif}')),
                                # tags$head(
                                #   tags$style(HTML("hr {border-top: 1px solid #696969;}"))
                                # ),
                        sidebarLayout(
                          sidebarPanel(
                            h4(strong("Select an excel file to upload")),
                            h5(tags$div(
                              HTML(paste("", tags$span(icon("fas fa-exclamation-triangle"), style = "background-color:#DCDCDC", "Your file needs to have exactly the same structure as shown in the data preview table"), sep = ""))
                            )),
                            h5(tags$a(href = 'template.xlsx', class = "btn", icon("download"), style='background-color:#91d1c2ff; color: #fdfbfb',
                                      'Download data template')),
                            #p(HTML("<b><div style='background-color:#91d1c2ff;border:1px solid black;'>Your file needs to have exactly the same format as shown in the data input table.</div></b>")),
                            fileInput('data_upload',
                                      '',
                                      accept = c(".xlsx")
                                     
                            ),
                            
                            radioButtons("display", 
                                         "Display:", 
                                         choices  = c("preview"="preview", "summary"="summary"),
                                         selected = "preview",
                                         inline   = TRUE,  
                            ),
                            
                            # h5(tags$a(href = 'template.xlsx', class = "btn", icon("download"), style='background-color:#91d1c2ff; color: #fdfbfb',
                            #           'Download data template')),
                            
                            # br(),
                            
                            # p(  Alternative data download handler
                            #   downloadButton("downloadtemp", "Download data template")
                            #   
                            # ),
                            #style="color:#DCDCDC",
                            
                            
                            
                            
                            hr(),
                            
                            h4("Perform algebraic calculations and imputation"),
                            
                            fluidRow(column(2,  actionButton(inputId = "SEfromSD", class = "SEfromSD", label = "Step 1: Fill-in SD from SE"))),
                            br(),
                            fluidRow(column(2,  actionButton(inputId = "sameSD", class = "sameSD", label = "Step 2: Equal SDs at pre/post"))), 
                            br(),
                            fluidRow(column(2,  actionButton(inputId = "correl", class = "correl", label = "Step 3: Calculate within-group correlations"))),
                            br(),
                            fluidRow(column(2,  actionButton(inputId = "final_calc", class = "final_calc", label = "Step 4: Make final calculations")))
                          ),  
                          
                          
                          mainPanel(
                            tabsetPanel(
                              tabPanel('Load data', 
                                       
                                       tags$style(type="text/css",
                                                  ".shiny-output-error { visibility: hidden; }",
                                                  ".shiny-output-error:before { visibility: hidden; }"
                                       ),
                                       
                                       # output data table
                                       # h2("Data Input"),
                                       # DT::dataTableOutput("input_table"),
                                       
                                       conditionalPanel("input.display == 'preview'", h2("Data Preview"), DT::dataTableOutput("input_table")),
                                       conditionalPanel("input.display == 'summary'", h2("Data Summary"),  verbatimTextOutput("structure")),
                                       textOutput("warning"), # Warning if wrong file format is uploaded 
                                       
                                       tags$head(tags$style("#warning {color: red;
                                                                      font-size: 20px;
                                                                      font-style: italic;
                                                                      }" )
                                                 
                                       ),
                                       
                                       h3("Description"),
                                       p("A long format dataset comprised of 14 columns. The variables are as follows:"),
                                       
                                       h3("Variables"),
                                       
                                       includeMarkdown("variables.md"),
                                       
                                       # h2("Data Structure"),
                                       # verbatimTextOutput("structure"),
                              ),
                              
                              tabPanel(value="missing",
                                       'Fill-in data', 
                                       fluidRow(column(12, style = "background-color:#cfcfcf",
                                       h3("Calculate SD from SE"),
                                       DT::dataTableOutput('SDfromSE')  )),
                                       br(),
                                       fluidRow(column(12, style = "background-color:#dcdcdc",
                                       h3("Assume equal SDs at baseline and follow-up"),
                                       DT::dataTableOutput('sameSDboth') )),
                                       br(),
                                       fluidRow(column(12, style = "background-color:#e9e9e9",
                                       h3("Calculate correlations from SDs from baseline, follow-up and change scores"),
                                       tableOutput('correl_output') )),
                                       br(),
                                       fluidRow(column(12, style = "background-color:#f6f6f6",
                                       h3("Final calculations"),
                                       h4("Impute correlations from median correlation values per group and SDs of change scores from correlations"),
                                       tableOutput('full_data') ))
                                       
                                       
                                       
                                       
                              ),
                              tabPanel('View final data', 
                                       tableOutput('final_data')
                              )
                              
                              
                            )
                          ) # end of main panel of tab: Data upload
                        )
                     )
             )
           
    ), # end of tabpanel :data upload
    
    # Tab 2: Data upload with -----------------------------------------------------------------------------
    tabPanel("Meta-Analysis", icon = icon("bar-chart-o"),
             
                         tabPanel("",
                                  fluidPage(
                                    sidebarLayout(
                                      sidebarPanel(
                                        h3(strong("Perform meta-analysis of pre/post effect size")),
                                        prettyRadioButtons(inputId = "es",  icon=icon("check"), # input is effect size - for now only MD, extension to SMD possible
                                                           h4('Select the effect size:'),
                                                           choices =   c('Mean difference'),
                                                           selected = "Mean difference"),
                                        
                                       
                                        
                                        radioButtons("type", h4("Select the MA model:"),
                                                     list("Random-effects (RE)" = "re",
                                                          "Common(fixed)-effect (CE)" = "ce"
                                                     ),
                                        ),
                                        checkboxInput("HK", label = "Hartung-Knapp Adjustment", value = FALSE),
                                        helpText("Only applicable under RE model")
                                        
                                       # br(),
                                       # yeah4("Final Scores analysis"), 
                                       # withMathJax(includeMarkdown("methods.md")),
                                       # helpText("The fields are pre-populated for you. Be patient for a few seconds and the results will be printed on the right panel."),
                                       # sliderInput("cor1", "Correlation coefficient in treatment group:", min = -1, max = 1, value = 0.5, step =0.001),
                                       # sliderInput("cor2", "Correlation coefficient in control group:", min = -1, max = 1, value = 0.5, step =0.001),
                                       # sliderInput("sd1", "SD of treatment group at follow-up:", min = 0, max = 60, value = 2, step= 0.01),
                                       # numericInput("N1","Sample size treatment group:", value = 10, min = 1, max=1000),
                                       # numericInput("mu2", "Mean of control group at follow-up:", min = 0, max = 1000, value = 10),
                                       # sliderInput("sd2", "SD of control group at follow-up:", min = 0, max = 60, value = 2, step= 0.01),
                                       # numericInput("N2", "Sample size of control group:", 10, min = 1, max = 1000)

                                      ),  
                                      
                                      
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel('Standard AD', 
                                                   
                                                   tabsetPanel(      
                                                      tabPanel("Final (follow-up) Scores",
                                                               
                                                               tags$style(type="text/css",
                                                                          ".shiny-output-error { visibility: hidden; }",
                                                                          ".shiny-output-error:before { visibility: hidden; }"
                                                               ),     
                                                               h2("Results"),
                                                               verbatimTextOutput('final_fe.out'),
                                                               verbatimTextOutput('final_re.out'),
                                                               br(),
                                                               
                                                               fluidRow(
                                                               column(width=6, align="left",
                                                               h3("Forest plot"),
                                                               downloadButton('downloadfinalForest', 'Download the plot as pdf'),
                                                               plotOutput("final.forest",  height = "550px", width = "550px")),
                                                               column(width=6,
                                                               h3("Funnel plot"),
                                                               downloadButton('downloadfinalFunnel', 'Download the plot as pdf'),
                                                               plotOutput("final.funnel",  height = "550px", width = "550px")),
                                                               )

                                                               ),

                                                      tabPanel("Change Scores",
                                                     
                                                               h2("Results"),
                                                               verbatimTextOutput('change_fe.out'),
                                                               verbatimTextOutput('change_re.out'),
                                                               br(),
                                                               
                                                               fluidRow(
                                                               column(width=6, align="left", 
                                                                         h3("Forest plot"),
                                                                         downloadButton('downloadchangeForest', 'Download the plot as pdf'),
                                                                         plotOutput("change.forest",  height = "550px", width = "550px") ),
                                                               column(width=6, 
                                                                         h3("Funnel plot"),
                                                                         downloadButton('downloadchangeFunnel', 'Download the plot as pdf'),
                                                                         plotOutput("change.funnel",  height = "550px", width = "550px") ),
                                                               )
                                                               
                                                               ),
                                                      
                                                      tabPanel("ANCOVA Recovered effect estimates",
                                                               h2("Results"),
                                                               verbatimTextOutput('ancova_fe.out'),
                                                               verbatimTextOutput('ancova_re.out'),
                                                               br(),
                                                               fluidRow(
                                                               column(width=6, align="left",
                                                                        h3("Forest plot"),
                                                                        downloadButton('downloadANCOVAForest', 'Download the plot as pdf'),
                                                                        plotOutput("ancova.forest",  height = "550px", width = "550px") ),
                                                               column(width=6, 
                                                                       h3("Funnel plot"), 
                                                                       downloadButton('downloadANCOVAFunnel', 'Download the plot as pdf'),
                                                                       plotOutput("ancova.funnel", height = "550px", width = "550px")
                                                                     )
                                                               
                                                                     )
                                                               ) 
                                                     
                                                     ),
                                                   
                                                  
                                                   # output data table
                                                   # h2("Final Scores"),
                                                   # verbatimTextOutput('final'),
                                                   # verbatimTextOutput('final_fe.out'),
                                                   #verbatimTextOutput('final_re.out'),
                                                   #textOutput('final')%>% withSpinner(type = getOption("spinner.type",5), color="#88BDBC"),
                                                   
                                                   # h2("Change Scores"),
                                                   # #verbatimTextOutput("change"),
                                                   # verbatimTextOutput('change_fe.out'),
                                                   # verbatimTextOutput('change_re.out'),
                                                   # h2("ANCOVA"),
                                                   # #verbatimTextOutput("ancova"),
                                                   # verbatimTextOutput('ancova_fe.out'),
                                                   # verbatimTextOutput('ancova_re.out'),
                                                   # br(),
                                                   useShinyjs(),
                                                   extendShinyjs(text = jsCode, functions = c("winprint")),
                                                   actionButton("print", "PRINT", style="color: #F2F2F2; background-color: #254e58; border-color: #254e58") 
                                          ),
                                          
                                          tabPanel('One-stage pseudo IPD', 
                                                   
                                                   tabsetPanel(      
                                                     tabPanel("Treatment effect",
                                                              
                                                              tags$style(type="text/css",
                                                                         ".shiny-output-error { visibility: hidden; }",
                                                                         ".shiny-output-error:before { visibility: hidden; }"
                                                              ), 
                                                              fluidRow(column(10, style = "background-color:#D1E8E2", 
                                                                              h3("Pseudo IPD"),
                                                                              DT::dataTableOutput("pseudoData") %>% withSpinner(type = getOption("spinner.type", 5), color="#88BDBC")
                                                                              )),
                                                                              br(), 
                                                                              br(),
                                                                       
                                                              fluidRow(column(10, style = "background-color:#DCDCDC",       
                                                                              h3("Results: Study-stratified model, options for the residual variances"),
                                                                              DT::dataTableOutput("one") %>% withSpinner(type = getOption("spinner.type", 5), color="#88BDBC")
                                                                              )
                                                                       )
                                                              
                                                     ),
                                                
                                                     tabPanel("Treatment-by-baseline interaction effect",
                                                                fluidRow(column(10, style = "background-color:#DCDCDC",  
                                                                                h3("Results: Study-stratified model, options for the residual variances"),
                                                                                DT::dataTableOutput("oneINT") %>% withSpinner(type = getOption("spinner.type", 1), color="#88BDBC")
                                                                                )
                                                                         
                                                                         )
                                                             
                                                             
                                                     ) 
                                                     
                                                   ),
                                                   
                                                  
                                                   
                                          ),
                                       
                                          
                                          tabPanel('Two-stage pseudo IPD', 
                                                   
   
                                                   tabsetPanel(      
                                                     tabPanel("Treatment effect",
                                                              h3('Results'),
                                                              tags$style(type="text/css",
                                                                         ".shiny-output-error { visibility: hidden; }",
                                                                         ".shiny-output-error:before { visibility: hidden; }"
                                                              ), 
                                                              
                                                              verbatimTextOutput("twostageME_FE.out"),
                                                              verbatimTextOutput("twostageME_RE.out"),
                                                              br(), 
                                                              
                                                              fluidRow(
                                                                column(width=6, align="left",
                                                                       h3("Forest plot"),
                                                                       selectInput("format_forestME","Choose file format",
                                                                           choices = list("pdf","png")),
                                                                       downloadButton("downloadForest","Download Here"),
                                                                       plotOutput("forest_twoME",  height = "550px", width = "550px") ),
                                                                
                                                                column(width=6, 
                                                                       h3("Funnel plot"),
                                                                       selectInput("format_funnelME","Choose file format",
                                                                                   choices = list("pdf","png")),
                                                                       downloadButton("downloadFunnel","Download Here"),
                                                                       plotOutput("funnel_twoME",  height = "550px", width = "550px")
                                                                       )
                                                              
                                                              )
                                                              ),
                                                     
                                                     tabPanel("Treatment-by-baseline interaction effect",
                                                              h3('Results'),
                                                              verbatimTextOutput("twostageME_FEint.out"),
                                                              verbatimTextOutput("twostageME_REint.out"),
                                                              br(),
                                                              
                                                              fluidRow(
                                                                column(width=6, align="left",
                                                                      h3('Forest plot'),
                                                                      selectInput("format_forestINT","Choose file format",
                                                                                    choices = list("pdf","png")),
                                                                      downloadButton("downloadForestInt","Download Here"),
                                                                      plotOutput("forest_twoMEint",  height = "550px", width = "550px") ),
                                                                
                                                                column(width=6,
                                                                       h3("Funnel plot"),
                                                                       selectInput("format_funnelINT","Choose file format",
                                                                                    choices = list("pdf","png")),
                                                                       downloadButton("downloadFunnelInt","Download Here"),
                                                                       plotOutput("funnel_twoMEint",  height = "550px", width = "550px")
                                                                )
                                                              
                                                              )
                                                              ) 
                                                     
                                                   ),
                                                   
                                                   

                                                  
                                          )
                                          
                                          
                                       # )
                                      ) # end of main panel of tab: meta-analysis 
                                      
                                    )
                                  )
                         )
             )
    ), # end of tabpanel : meta-analysis 
    
    tabPanel("About", icon = icon("fas fa-book-reader"),
             withMathJax(includeMarkdown("technical_notes.md"))
    ),
 
 
    #hr(),   
    footer = titlePanel( 
      div(
        column(width = 6,   
               h5(strong("Meta-analysis of continuous outcomes: pre/post effect size v0.1")), 
               em(), 
               h6("Developed by : Katerina Papadimitropoulou, katerina.papadimitropoulou@gmail.com"),
               em(),
               h6("Contributors : Saskia le Cessie, Theo Stijnen, Richard D. Riley and Olaf M. Dekkers")
        )
      )
    )
    
    
    
  )
)
