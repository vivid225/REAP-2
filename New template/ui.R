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
    # LINK TO NPLR
    # h5(
    #   em(
    #     a("...using R package 'nplr'",
    #       href="http://cran.r-project.org/web/packages/nplr/index.html",
    #       target="_blank")
    #   ),
    #   align="right"
    # ),
    
    ########################################
    # CHOOSE FILE
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
    
    ########################################
    
    # Model features -----
    withTags(div(class="col-sm-12 section-title", h3("Model Feature"))),
    
    withTags(
      div(class="row",
          div(class="col-xs-12 btn-input",
              div(class="col-xs-9 checkboxText", "Dose-dependent precision"),
              div(class="col-xs-3", checkboxInput('cbox_dose_dep', 'yes', FALSE))
          )
      )
    ),
    
    withTags(
      div(class='col-xs-12 btn-input', numericInput(inputId = "effectpct", "Add effect estimation",value=50, min=0, max=100))
    ),
    
    
    # # Name axes
    # div(class="row",
    #     div(class='col-xs-6', textInput("xlabel", 'x-axis name', "Log10(Conc.)")),
    #     div(class='col-xs-6', textInput("ylabel", 'y-axis name', 'Response (Vs. control)'))
    # ),
    # 
    # tags$div(class='col-xs-12', numericInput(inputId = "effectpct",
    #                                     tags$span("Add effect estimation",
    #                                          tags$i(
    #                                            id = "effectpct",
    #                                            class = "glyphicon glyphicon-info-sign",
    #                                            style = "color:#0072B2; ",
    #                                            title = "Input percentage of effect, from 0 to 100; Effect estimates are shown with triangles in the dose-response curve plot"  
    #                                          )), 
    #                                     value=50, min=0, max=100)),
    # 
    # numericInput(inputId = "effectpct",
    #              tags$span("Add effect estimation",
    #                        tags$i(
    #                          id = "effectpct",
    #                          class = "glyphicon glyphicon-info-sign",
    #                          style = "color:#0072B2; ",
    #                          title = "Input percentage of effect, from 0 to 100; Effect estimates are shown with triangles in the dose-response curve plot"  
    #                        )), 
    #              value=50, min=0, max=100),
    
    
    
    
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
    
 
    
    ########################################
    # Plot specifics -----
    withTags(div(class="col-sm-12 section-title", h3("Plot Specifics"))),
    
    withTags(
      div(class='col-sm-12',
          
          div(class = "row",
              div(class="col-xs-12 radioText", "Show values"),
              div(class="col-xs-12 btn-input",
                  div(class="col-xs-4", checkboxInput(inputId = "points", label = "Points", value=FALSE)),
                  div(class="col-xs-4", checkboxInput(inputId = "Means", label = "Means", value=TRUE)),
                  div(class="col-xs-4", checkboxInput(inputId = "SDerr", label = "SDerr", value=TRUE))
              )
          ),
          
          # Show x-axis as Log
          div(class="row",
              div(class="col-xs-12 btn-setting",
                  div(class="col-xs-9 checkboxText", "Log10(Conc.)"),
                  div(class="col-xs-3", checkboxInput('showAsLog', 'yes', TRUE))
              )
          ),
          
          # Show legend
          div(class="row",
              div(class="col-xs-12 btn-setting",
                  div(class="col-xs-9 checkboxText", "Show legend"),
                  div(class="col-xs-3", checkboxInput('showLegend', 'yes', TRUE))
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
    # # Settings
    # withTags(div(class="col-sm-12 section-title", h3("Visualize"))),
    # 
    # # HIGHLIGHT
    # uiOutput('modelNames'),
    # 
    # # SAVE OPTIONS
    # withTags(
    #   div(class='col-sm-12',
    #       
    #       div(class = "row",
    #           div(class="col-xs-12 radioText", "Show values"),
    #           div(class="col-xs-12 btn-input",
    #               div(class="col-xs-4", checkboxInput(inputId = "points", label = "Points", value=FALSE)),
    #               div(class="col-xs-4", checkboxInput(inputId = "Means", label = "Means", value=TRUE)),
    #               div(class="col-xs-4", checkboxInput(inputId = "SDerr", label = "SDerr", value=TRUE))
    #           )
    #       ),
    #       
    #       # Show x-axis as Log
    #       div(class="row",
    #           div(class="col-xs-12 btn-setting",
    #               div(class="col-xs-6 checkboxText", "Conc. in Log10"),
    #               div(class="col-xs-6", checkboxInput('showAsLog', 'yes', TRUE))
    #           )
    #       ),
    #       
    #       # Show legend
    #       div(class="row",
    #           div(class="col-xs-12 btn-setting",
    #               div(class="col-xs-6 checkboxText", "Show legend"),
    #               div(class="col-xs-6", checkboxInput('showLegend', 'yes', TRUE))
    #           )
    #       ),
    #       
    #       # Name axes
    #       div(class="row",
    #           div(class='col-xs-6', textInput("xlabel", 'x-axis name', "Log10(Conc.)")),
    #           div(class='col-xs-6', textInput("ylabel", 'y-axis name', 'Response (Vs. control)'))
    #       )
    #   )
    # ),
    ########################################
    # SUBMIT BUTTON
    withTags(div(class="col-sm-12 section-title", h3("Submit"))),
    withTags(
      div(class="row",
          div(class="col-sm-12", id="saving-subtitle", h4("Filename (without extension)")),
          div(class="col-sm-12", id="fileName", textInput("fname", '', 'myResults')),
          div(class='col-sm-12',
              div(class='col-sm-6 save-btn', submitButton("submitSetting", class="btn-lg btn-success", "Submit"))
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
              div(class='col-sm-6 save-btn', downloadButton("downloadPLot", class="btn-lg btn-success", "Download report"))
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
               h3("Objective", id = "intro"),
               p("The Robust and Efficient Assessment of drug Potency (REAP) is 
                             developed for convenient application of the robust dose-response 
                             estimation to real-world data analysis. It presents a straightforward 
                             analytic environment for robust estimation of dose-response curve 
                             and assessment of key statistics, including implementation of 
                             statistical comparisons and delivery of customized output for 
                             graphic presentation."),
               
               h3("Illustrative Example"),
               h4("Dataset Input Requirements", id = "exp"),
               tags$ul(
                 tags$li("The input dataset should be in a csv file"),
                 tags$li("The input dataset contains three columns: Concentration, Effect and Agent"),
                 tags$li("Columns in the input dataset should follow the order of Concentration, Effect and Agent")
               ),
               p("It is recommended that users normalize the response variable to the range of (0,1) by 
                               themselves. Otherwise, REAP will automatically truncate the values exceeding 
                               the boundaries to (0,1) using a truncation algorithm"),
               p("Below is an example dataset illustrating the format of the input dataset"),
               tags$a(href='31780660_F1B_exampledata.csv', 
                      target='blank', 'Sample Data', download = '31780660_F1B_exampledata.csv'),
               tableOutput("table1"),
               
               h4("Output"),
               p("We upload the example data and obtain the following results in the output tab:"),
               # tags$img(src='shinydemo.png', height="60%", width="60%", align="center"),
               br(),
               p("Under the output tab, we will see a dose-response curve plot, along with 
                         tabulation for effect and model estimations. We also enable hypothesis testing 
                         for comparisons of effect estimations, slopes and models (i.e., comparing both 
                         intercepts and slopes). By default, the x-axis of the dose-response plot is 
                         log-scaled. In the plot, users can choose to add mean values and confidence 
                         intervals for data points under the same agent and dose level. Triangles indicate 
                         effect estimations. Both plots and 
                         estimation tables are downloadable on REAP to plug in presentations and 
                         manuscripts for result dissemination."),
               
               h4("Reference", id= "ref"),
               p("Zhou, S*, Liu, X*, Fang, X*, Chinchilli, VM, Wang, M, Wang, HG, Dokholyan, NV, Shen, C, Lee, JJ. (2021) Robust and Efficient Assessment of Potency (REAP): A Quantitative Tool for Dose-response Curve Estimation. doi:10.1101/2021.11.20.469388"),
               p("Fang, X, Zhou, S. (2022) A Comparative Simulation Study of In Vitro Dose-response Estimation under Extreme Observations. In review."),
               p("Shiny app designed based on: Common F.  https://github.com/fredcommo/curveFitter"),
               
               withTags(					
                 div(class = "col-sm-12",
                     h3(id="model-summary", "Model(s) summary", align="center"),
                     tableOutput('summary')
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
                             sliderInput("pSize", label="", min=0, max=1, value=.3, step=.01))
                     ),
                     
                     # Line width slider
                     div(class="col-xs-4",
                         div(class="col-xs-12 radioText", "Lines width"),
                         div(class="col-xs-12 slider-input",
                             sliderInput("lWidth", label="", min=0, max=1, value=.5, step=.01))
                     ),
                     
                     # Legend size slider
                     div(class="col-xs-4",
                         div(class="col-xs-12 radioText", "Legend size"),
                         div(class="col-xs-12 slider-input",
                             sliderInput("legendSize", label="", min=0, max=1, value=.5, step=.01))
                     )
                     
                 )
               ), # withTags
               
               # Plot output
               h6(verbatimTextOutput("checkFile"),
                  style="visibility: collapse; height: 0px;"),
               
               conditionalPanel(
                 condition = "output.checkFile == '0'",
                 div(class="col-sm-12",
                     uiOutput('message'),
                     imageOutput("welcomeImage")
                 )
               ),
               
               conditionalPanel(
                 "output.checkFile == '1'",
                 plotOutput("plot", width = "100%", height = "100%")
               )
               
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
                     tableOutput('summary')
                 )
               )
      ) # tabpanel summary
      
    ) #tabsetPanel
  ) #main panel
  

))
