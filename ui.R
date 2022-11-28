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
          h3("Choose a file", em(id="supportedFiles", "(.csv, .tsv, .txt)")))
    ),
    
    # withTags(
    #   div(class="col-xs-12 radioText", 
    #       a(href='www/31780660_F1B_exampledata.csv', 
    #         target='blank', 'Example Data', download = 'www/31780660_F1B_exampledata.csv'))
    # ),
    
    withTags(
      
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
    
    ########################################
    # SUBMIT BUTTON
    # withTags(div(class="col-sm-12 section-title", h3("Submit"))),
    # withTags(
    #   div(class="row",
    #       # div(class="col-sm-12", id="saving-subtitle", h4("Filename (without extension)")),
    #       # div(class="col-sm-12", id="fileName", textInput("fname", '', 'myResults')),
    #       div(class='col-sm-12',
    #           # div(class='col-sm-6 submit-btn', submitButton("submitSetting", "Submit"))
    #           div(class='col-sm-6 submit-btn', 
    #               shinyWidgets::actionBttn(inputId = "submitbutton",
    #                                        label = "Submit data",
    #                                        color = "primary",
    #                                        style = "pill"
    #               ))
    #           # div(class='col-sm-6 save-btn', downloadButton("downloadData", class="btn-lg btn-success", "Save Results"))
    #       )
    #   )
    # ),
    
    
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
      div(class='col-xs-12 btn-input', numericInput(inputId = "effectpct", "Add potency estimation",value=50, min=0, max=100))
    ),
    
    
    # Model comparison -----
    withTags(div(class="col-sm-12 section-title", h3("Model Comparison"))),
    
    withTags(
      div(class="row",
          div(class="col-xs-12 btn-input",
              div(class="col-xs-9 checkboxText", "Potency estimations"),
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
              div(class='col-xs-6', textInput("xlabel", 'x-axis name', '')),
              div(class='col-xs-6', textInput("ylabel", 'y-axis name', ''))
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
                     h3(id="model-summary", "Welcome to REAP-2!", align="left"),
                     p(id="paragraph","REAP-2 is an updated version of REAP, the Robust and Efficient Assessment of drug Potency.
                             It is developed for convenient application of the robust dose-response
                             estimation to real-world data analysis. ", align = "justify"),
                     p(id="paragraph","The previous version REAP can be reached at ",
                       a(href = 'https://xinying-fang.shinyapps.io/REAP/', 'REAP Shiny app', .noWS = "outside"), '.', .noWS = c("after-begin", "before-end")),
                     p(id="paragraph","Check here to obtain the user guide for REAP-2: ",
                       a(href = 'https://github.com/vivid225/REAP-2/blob/main/REAP-2%20User%20Guide.pdf', 'REAP-2 User Guide', .noWS = "outside"), '.', .noWS = c("after-begin", "before-end"))
                 )
               ),
               
               withTags(					
                 div(class = "col-sm-12",
                     h3(id="model-summary", "Reference", align="left"),
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
                             sliderInput("pSize", label="", min=0, max=5, value=2, step=.1))
                     ),
                     
                     # StdDev interval width slider
                     div(class="col-xs-4",
                         div(class="col-xs-12 radioText", "StdDev interval width"),
                         div(class="col-xs-12 slider-input",
                             sliderInput("lWidth", label="", min=0, max=0.1, value=.03, step=.01))
                     ),
                     
                     # Line size slider
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
               
               
      ), # tabpanel Curve
      
      tabPanel("Summary",
               withTags(					
                 div(class = "col-sm-12",
                     h3(id="model-summary", "Statistical methods", align="center"),
                     p(id="paragraph","REAP-2 updates REAP by changing the underlying method 
                     with the penalized beta regression. The estimation of the penalized beta regression is accomplished by 
                       the mgcv package (Wood, 2017). It was developed to estimate the penalized 
                       generalized linear models by adding the L2 ridge penalty on the log-likelihood 
                       function with a tuning parameter. The parameter estima-tions are obtained 
                       through maximizing the penalized likelihood with the penalized iteratively 
                       re-weighted least squares (P-IRLS) by mgcv.", align = "justify"),
                     
                     h3(id="model-summary", "Model(s) summary", align="center"),
                     htmlOutput('modelsummary'),
                     
                     h3(id="model-summary", "Summary", align="center"),
                     htmlOutput('summary'),

                     h3(id="model-summary", "Model comparison", align="center"),
                     htmlOutput('modelcomparison')
                     
                 )
               )
      ) # tabpanel summary
      
    ) #tabsetPanel
  ) #main panel
  
  
))
