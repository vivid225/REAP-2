#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  ########################################
  # TITLE
  # headerPanel(tags$strong('REAP-2'),
  #             tags$span(class="small", ' Robust and Efficient Assessment of drug Potency')),
  headerPanel(
    div(class="col-sm-12 lato", id="pageTitle",
        # tags$span('curve',tags$strong('Fitter')),
        tags$strong('REAP-2'),
        tags$span(class="small", ' Robust and Efficient Assessment of drug Potency')
    )
  ),
  
  ########################################
  # SIDEBAR PANEL
  sidebarPanel(
    
    ########################################
    # IMPORT GOOGLE FONTS
    
    tags$link(
      rel = "stylesheet",
      href="https://fonts.googleapis.com/css?family=Lato:400,100,100italic,300,300italic,400italic,700,700italic,900,900italic",
      type="text/css"
    ),
    
    tags$link(
      rel = "stylesheet",
      href="https://fonts.googleapis.com/css?family=Calligraffitti",
      type="text/css"
    ),
    
    ########################################
    # LOAD CSS FILE
    includeCSS("reap2.css"),
    
    ########################################

    # INPUT FILE -----
    withTags(
      div(class="col-sm-12 section-title",
          h3("Choose a file", em(id="supportedFiles", "(.csv, .tsv, .txt)"))
      )
    ),
    withTags(
      # div(
      # class="row",
      # div(class="col-xs-12 btn-input",
      #     div(class="col-xs-6 checkboxText", "File with headers"),
      #     div(class="col-xs-6", checkboxInput('header', 'yes', TRUE))
      # ),
      div(class="col-xs-12", id="inputFile",
          fileInput('file1', '',
                    accept=c('text/csv',
                             'text/tab-separated-values,text/comma-separated-values,text/plain',
                             '.csv', '.txt', '.tsv'
                    )
          )
      )
      # )
    ),
    
    
    # Model features -----
    withTags(div(class="col-sm-12 section-title", h3("Model Feature"))),
    
    withTags(
      div(class="row",
          div(class="col-xs-12 btn-input",
              div(class="col-xs-9 checkboxText", "Log transform dose"),
              div(class="col-xs-3", checkboxInput('cbox_logconc', 'yes', TRUE))
          )
          # div(class="col-xs-12 btn-input",
          #     div(class="col-xs-9 checkboxText", "Extrapolate"),
          #     div(class="col-xs-3", checkboxInput('cbox_extrap', 'yes', FALSE))
          # )
      )
    ),
    
    withTags(
      div(class='col-xs-12 btn-input', numericInput(inputId = "effectpct", "Add effect estimation",value=50, min=0, max=100))
    ),
    
    
    # Model comparison -----
    withTags(div(class="col-sm-12 section-title", h3("Model Comparison"))),
    
    withTags(
      div(class="row",
          div(class="col-xs-12 btn-input",
              div(class="col-xs-9 checkboxText", "Effect estimations"),
              div(class="col-xs-3", checkboxInput('cbox_effectest', 'yes', FALSE))
          )
      )
    ),
    withTags(
      div(class="row",
          div(class="col-xs-12 btn-input",
              div(class="col-xs-9 checkboxText", "Slopes"),
              div(class="col-xs-3", checkboxInput('cbox_slopes', 'yes', FALSE))
          )
      )
    ),
    
    
    

    # Plot specifics -----
    withTags(div(class="col-sm-12 section-title", h3("Plot Specifics"))),
    
    withTags(
      div(class='col-sm-12',
          
          div(class = "row",
              div(class="col-xs-12 radioText", "Show values"),
              div(class="col-xs-12 btn-input",
                  div(class="col-xs-4", checkboxInput(inputId = "Points", label = "Points", value=FALSE)),
                  div(class="col-xs-4", checkboxInput(inputId = "Means", label = "Means", value=TRUE)),
                  div(class="col-xs-4", checkboxInput(inputId = "SDerr", label = "StdDev", value=TRUE))
              )
          ),
          
          # Show x-axis as Log
          div(class="row",
              div(class="col-xs-12 btn-setting",
                  div(class="col-xs-9 checkboxText", "Log10(dose)"),
                  div(class="col-xs-3", checkboxInput('showAsLog', 'yes', TRUE))
              )
          ),
         
          # Name axes
          div(class="row",
              div(class='col-xs-6', textInput("xlabel", 'x-axis name', "Log10(Conc.)")),
              div(class='col-xs-6', textInput("ylabel", 'y-axis name', 'Response (Vs. control)'))
          )
      )
    ),
    
    ########################################
    # SUBMIT BUTTON
    # withTags(div(class="col-sm-12 section-title", h3("Submit"))),
    withTags(
      div(class="row",
          # div(class="col-sm-12", id="saving-subtitle", h4("Filename (without extension)")),
          # div(class="col-sm-12", id="fileName", textInput("fname", '', 'myResults')),
          div(class='col-sm-12',
              # div(class='col-sm-6 submit-btn', submitButton("submitSetting", "Submit"))
              div(class='col-sm-6 submit-btn', 
                  shinyWidgets::actionBttn(inputId = "submitbutton",
                                           label = "Submit",
                                           color = "primary",
                                           style = "pill"
                                           ))
              # div(class='col-sm-6 save-btn', downloadButton("downloadData", class="btn-lg btn-success", "Save Results"))
          )
      )
    ),
    
    ########################################
    # DOWNLOAD BUTTONS
    withTags(div(class="col-sm-12 section-title", h3("Save"))),
    withTags(
      div(class="row",
          div(class="col-sm-12", id="saving-subtitle", h4("Filename (without extension)")),
          div(class="col-sm-12", id="fileName", textInput("fname", '', 'myResults')),
          div(class='col-sm-12',
              div(class='col-sm-6 save-btn', downloadButton("downloadReport", class="btn-lg btn-success", "Download report"))
              # div(class='col-sm-6 save-btn', downloadButton("downloadData", class="btn-lg btn-success", "Save Results"))
          )
      )
    )
  ),
  # end sidebarPanel
  
  ########################################
  # OUTPUTS PANEL
  mainPanel(
    tabsetPanel(
      
      tabPanel("Introduction",
               
               withTags(					
                 div(class = "col-sm-12",
                     h3(id="model-summary", "Objective", align="center"),
                     p(id="paragraph","The Robust and Efficient Assessment of drug Potency (REAP) is 
                             developed for convenient application of the robust dose-response 
                             estimation to real-world data analysis. It presents a straightforward 
                             analytic environment for robust estimation of dose-response curve 
                             and assessment of key statistics, including implementation of 
                             statistical comparisons and delivery of customized output for 
                             graphic presentation.", align = "justify")
                     # tableOutput('summary')
                 )
               ),
               
               withTags(					
                 div(class = "col-sm-12",
                     h3(id="model-summary", "Reference", align="center"),
                     p(id="paragraph","Zhou, S*, Liu, X*, Fang, X*, Chinchilli, VM, Wang, M, Wang, HG, Dokholyan, NV, Shen, C, Lee, JJ. (2021) Robust and Efficient Assessment of Potency (REAP): A Quantitative Tool for Dose-response Curve Estimation. doi:10.1101/2021.11.20.469388"),
                     p(id="paragraph","Fang, X, Zhou, S. (2022) A Comparative Simulation Study of In Vitro Dose-response Estimation under Extreme Observations. In review."),
                     p(id="paragraph","Shiny app designed based on: Common F.  https://github.com/fredcommo/curveFitter")
                 )
               )
               
      ), # tabpanel introduction
      
      tabPanel("Curve",
               
               # Plot setting Sliders
               withTags(
                 div(class="row sliders-panel",
                     
                     # Points size slider
                     div(class="col-xs-4",
                         div(class="col-xs-12 radioText", "Points size"),
                         div(class="col-xs-12 slider-input",
                             sliderInput("pSize", label="", min=0, max=5, value=1, step=.1))
                     ),
                     
                     # Line width slider
                     div(class="col-xs-4",
                         div(class="col-xs-12 radioText", "StdDev width"),
                         div(class="col-xs-12 slider-input",
                             sliderInput("lWidth", label="", min=0, max=0.1, value=.01, step=.01))
                     ),
                     
                     # Legend size slider
                     div(class="col-xs-4",
                         div(class="col-xs-12 radioText", "Line size"),
                         div(class="col-xs-12 slider-input",
                             sliderInput("lineSize", label="", min=0, max=1, value=0.5, step=.01))
                     )
                     
                 )
               ), # withTags
               
               withTags(					
                 div(class = "col-xs-12",
                     span(textOutput("text_warning1"), style="color:red"),
                     span(textOutput("text_warning2"), style="color:red")
                 )
               ),
               
               plotOutput("plot123"), # Plot output
               
               
               # h6(verbatimTextOutput("checkFile"),
               #    style="visibility: collapse; height: 0px;"),
               # 
               # conditionalPanel(
               #   condition = "output.checkFile == '0'",
               #   div(class="col-sm-12",
               #       uiOutput('message'),
               #       imageOutput("welcomeImage")
               #   )
               # ),
               # 
               # conditionalPanel(
               #   "output.checkFile == '1'",
               #   plotOutput("plot", width = "100%", height = "100%")
               # )
               
               # withTags(
               # 	div(class='row',
               # 		uiOutput('message'),
               # 		div(class = "col-sm-12", plotOutput('plot'))
               # 		)
               # 	)
               
      ), # tabpanel Curve
      
      tabPanel("Summary",
               withTags(					
                 div(class = "col-sm-12",
                     h3(id="model-summary", "Model(s) summary", align="center"),
                     htmlOutput('modelsummary'),
                     
                     h3(id="model-summary", "Summary", align="center"),
                     htmlOutput('summary'),

                     h3(id="model-summary", "Model comparison", align="center"),
                     htmlOutput('modelcomparison')
                     
                 )
               )
               # withTags(					
               #   div(class = "col-sm-12",
               #       h3(id="model-summary", "Summary", align="center"),
               #       htmlOutput('summary')
               #   )
               # ),
               # withTags(					
               #   div(class = "col-sm-12",
               #       h3(id="model-summary", "Model comparison", align="center"),
               #       htmlOutput('modelcomparison')
               #   )
               # )
      ) # tabpanel summary
      
    ) #tabsetPanel
  ) #main panel
  
  
))
