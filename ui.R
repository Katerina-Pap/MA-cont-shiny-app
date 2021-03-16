library(shiny)
library(shinythemes)
library(DT)
library(MASS)
library(dplyr)
library(skimr)
library(shinyjs)
library(shinyalert)
library(shinyWidgets)
library(shinycssloaders)
library(reshape2)
library(nlme)
library(metafor)
library(meta)


# function to print table of results 
jsCode <- 'shinyjs.winprint = function(){
window.print();
}'

# Define UI --------------------------------------------------------------------------------------------------------------------------------------------------------
shinyUI(

  
  navbarPage(
    id    = "mytabsetpanel",
    theme = shinytheme("flatly"), 
    title = div("MA-cont: pre/post effect size"),
    # Begin overview of tabs-------------------------------------------------------------------------------
    # Tab1: Main or home tab; include only an RMD file 
    tabPanel("Home", icon = icon("far fa-home"),
             h1(strong("MA-cont: pre/post effect size")),               
             h3("Run meta-analysis of continuous outcome data measured at baseline and follow-up"),
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
                     h3(strong("Citation")),
                     p(
                       "If you use",
                       em("MA-cont: pre/post effect size"),
                       "to perform your analyses, please remember to cite the tool."
                     ),
                     hr(),
                     
                     h3(strong("Found a bug?")),
                     p(
                       "Please ",
                       a(href = "mailto:katerina.papadimitropoulou@gmail.com", "email me")
                     ),
                     p(strong("OR")),
                     p(
                       "Log an issue on",
                       a(href = "https://github.com/Katerina-Pap/MA-cont-pre-post-ES-shiny-app/issues", "GitHub")
                     )
                    #,
                     #width = 4
                     
                   ),
                   mainPanel(
                     fluidRow(
                       column(width = 6, align="left", h3("Getting started")),
                       column(
                         width = 6,
                         align = "right",
                         br(),
                         actionButton("gotodata", "Upload your data")
                       ),
                  includeMarkdown("overview.md"), # to uncomment out later
                  )
               )
            )
          ),  # ends column after fluidRow
               column(
                 4,
                 align="right", 
                 br(),
                 uiOutput("video")
               )
            )
             
             
                     
    ),
    

 # Tab 2: Data upload with -----------------------------------------------------------------------------
    tabPanel("Data upload & pre-processing", icon = icon("table"), value="data",
             
           #tabsetPanel(type = "tabs",
             tabPanel("",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            h4("Select a data file (.csv) to upload"),
                            h6(tags$div(
                              HTML(paste("", tags$span(icon("fas fa-exclamation-triangle"), style = "background-color:#DCDCDC", "Excel files should be saved in .csv (comma delimited) format"), sep = ""))
                            )),
                            p(HTML("<b><div style='background-color:#91d1c2ff;border:1px solid black;'>Your data needs to have exactly the same header (variable names) in the first row.</div></b>")),
                            fileInput('data_upload',
                                      '',
                                      accept = c('text/csv',
                                                 'text/comma-separated-values',
                                                 '.csv')
                                      #accept = c(".xlsx", ".xls", ".csv")
                            ),
                            
                            # p(
                            # downloadButton("downloadData", "RoB2.0 dataset")
                            # ),
                            h5(tags$a(href = 'example.csv', class = "btn", icon("download"), style='background-color:#91d1c2ff; color: #fdfbfb',
                                      'Download and use the data template to prepare your file')),
                            #style="color:#DCDCDC",
                            # Input: Checkbox if file has header ---------
                            checkboxInput("header", "Header", TRUE),
                            
                            # Input: Select separator --------------------
                            radioButtons( "separator",
                                          "Separator: ",
                                          choices   = c(";", ",", ":"),
                                          selected  = ";",
                                          inline    = TRUE
                            ),
                            
                            # Input: Select decimals ---------------------------------------------------------------------------------
                            radioButtons("dec","Decimal",
                                         choices = c(Point = ".",Comma = ","),                                
                                         selected  = '.'),
                            br(),
                            fluidRow(column(2,  actionButton(inputId = "SEfromSD", class = "SEfromSD", label = "Fill-in SD from SE"))),
                            br(),
                            fluidRow(column(2,  actionButton(inputId = "sameSD", class = "sameSD", label = "Equal SDs at pre/post"))), 
                            br(),
                            fluidRow(column(2,  actionButton(inputId = "correl", class = "correl", label = "Calculate within-group correlations"))),
                            br(),
                            fluidRow(column(2,  actionButton(inputId = "final_calc", class = "final_calc", label = "Make final calculations")))
                          ),  
                          
                          
                          mainPanel(
                            tabsetPanel(
                              tabPanel('Load data', 
                                       
                                       tags$style(type="text/css",
                                                  ".shiny-output-error { visibility: hidden; }",
                                                  ".shiny-output-error:before { visibility: hidden; }"
                                       ),
                                       # output data table
                                       h2("Data Input"),
                                       DT::dataTableOutput("input_table"),
                                       h2("Data Structure"),
                                       verbatimTextOutput("structure"),
                              ),
                              
                              tabPanel(value="missing",
                                       'Fill-in data', 
                                       fluidRow(column(12, style = "background-color:#cfcfcf",
                                       h3("Calculate SD from SE"),
                                       tableOutput('SDfromSE')  )),
                                       br(),
                                       fluidRow(column(12, style = "background-color:#dcdcdc",
                                       h3("Assume equal SDs at baseline and follow-up"),
                                       tableOutput('sameSDboth') )),
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
             
             #tabsetPanel(type = "tabs",
                         tabPanel("",
                                  fluidPage(
                                    sidebarLayout(
                                      sidebarPanel(
                                        h2("Perform meta-analysis of pre/post effect size"),
                                        h3("Effect size: Mean difference"),
                                        #p("The goal ...."),
                                        
                                        radioButtons("type", h3("Select model:"),
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
                                                      tabPanel("Final Scores",
                                                               
                                                               tags$style(type="text/css",
                                                                          ".shiny-output-error { visibility: hidden; }",
                                                                          ".shiny-output-error:before { visibility: hidden; }"
                                                               ),     
                                                               h2("Final Scores"),
                                                               verbatimTextOutput('final_fe.out'),
                                                               verbatimTextOutput('final_re.out'),
                                                               
                                                               ),
                                                      tabPanel("Change Scores",
                                                     
                                                               h2("Change Scores"),
                                                               verbatimTextOutput('change_fe.out'),
                                                               verbatimTextOutput('change_re.out')
                                                               
                                                               ),
                                                      
                                                      tabPanel("ANCOVA Recovered effect estimates",
                                                               h2("ANCOVA"),
                                                               #verbatimTextOutput("ancova"),
                                                               verbatimTextOutput('ancova_fe.out'),
                                                               verbatimTextOutput('ancova_re.out')   
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
                                                     tabPanel("Treatment Effect",
                                                              
                                                              tags$style(type="text/css",
                                                                         ".shiny-output-error { visibility: hidden; }",
                                                                         ".shiny-output-error:before { visibility: hidden; }"
                                                              ), 
                                                              fluidRow(column(6, style = "background-color:#DCDCDC", 
                                                                              h3("Mean difference effect estimates"),
                                                                              h4("Study-stratified results: options for within-study residual variance"),
                                                                              #tags$style(css),
                                                                              #textInput("name", "Enter the nutrient's name", " "),
                                                                              #textOutput('one')%>% withSpinner(type = getOption("spinner.type",5), color="#88BDBC")
                                                                              #dataTableOutput("one") %>% withSpinner(color="#88BDBC") ))),
                                                                              DT::dataTableOutput("one") %>% withSpinner(type = getOption("spinner.type", 5), color="#88BDBC") ))
                                                              
                                                          
                                                              
                                                     ),
                                                
                                                     tabPanel("Treatment-by-baseline interaction effect",
                                                              h4("Study-stratified results: options for within-study residual variance"),
                                                              DT::dataTableOutput("oneINT") %>% withSpinner(type = getOption("spinner.type", 1), color="#88BDBC")
                                                              #div(DT::dataTableOutput("table"), style = "font-size: 75%; width: 75%")
                                                     ) 
                                                     
                                                   ),
                                                   
                                                  
                                                   
                                                   ),
                                          #textOutput('one')%>% withSpinner(type = getOption("spinner.type",5), color="#88BDBC"),
                                          
                                          tabPanel('Two-stage pseudo IPD', 
                                                   
   
                                                   tabsetPanel(      
                                                     tabPanel("Treatment Effect",
                                                              
                                                              tags$style(type="text/css",
                                                                         ".shiny-output-error { visibility: hidden; }",
                                                                         ".shiny-output-error:before { visibility: hidden; }"
                                                              ), 
                                                              
                                                              verbatimTextOutput("twostageME_FE.out"),
                                                              verbatimTextOutput("twostageME_RE.out"),
                                                              # fluidRow(column(6, style = "background-color:#DCDCDC", 
                                                              h2("Forestplot of Mean difference"), 
                                                              div(style = "margin-top: -10px"),
                                                              plotOutput("forest_twoME", height = "550px", width= "450px"), #)), 
                                                              br(),
                                                              selectInput("format","Choose file format",
                                                                          choices = list("pdf","png")),
                                                              downloadButton("download","Download Here")
                                                              
                                                              #downloadButton('twoMEPlot', 'Download the plot as pdf'),
                                                              #downloadButton('twoMEPlotpng', 'Download the plot as png')
                                                              
                                                              
                                                     ),
                                                     
                                                     tabPanel("Treatment-by-baseline interaction effect",
                                                              
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
    
    tabPanel("Technical Details", icon = icon("fas fa-book-reader"),
             withMathJax(includeMarkdown("technical_notes.md"))
    ),

    
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
