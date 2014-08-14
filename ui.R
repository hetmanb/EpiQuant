
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinysky)
#  theme = "united.css",

shinyUI(navbarPage( fluid = T, title = shiny::a("EpiQuant", href = "https://github.com/hetmanb/EpiQuant/wiki"), inverse = T, footer=a(href="mailto:hetmanb@gmail.com", "Questions? Email Me"),
                   
######################## *******************************  ************************************** ################
#                                            NavTab for Source-Matrix                                           #
######################## *******************************  ************************************** ################                   
                  tabPanel("SourceMatrix",
                      pageWithSidebar(                        
                        # Application title
                        headerPanel("Source Analysis using Epi-Matrix"),   
                        # Sidebar with a slider input for number of observations
                        sidebarPanel(
                          tags$head(
                          tags$script(src="lib/d3.js"),
                          tags$script(src="lib/underscore.js"),
                          tags$script(src="js/mapper.js"),
                          tags$script(src = "js/chordtest2.js")),
                          
                          h4("Epi-Matrix"),
                                     shinysky::shinyalert(id ="alert1", click.hide = T, auto.close.after = 5),                                    
                                     p("Epi-matrix is a method of coming up with pairwise similarity indices based solely on a subjective scoring matix"),
                                     p("First, you'll need to download the", shiny::a("template file.", href= "https://www.dropbox.com/s/4p1xa8fx5myxq45/epi-score.txt?dl=1")),
                                     br(), 
                                     p("Once you've downloaded the file, open it in your favorite spreadsheet software and start filling in the boxes using the following rules."),
                                     code("1 = Strongly Correlated"),
                                     code("0 = Strongly Uncorrelated"),
                                     code("7 = Possibly Correlated (Wildcard)"),
                                     br(),
                                     br(),
                                     p("Upload your similarity scoring matrix here:"),
                                     fileInput(inputId="source_scores",label="Upload",multiple=FALSE,accept=".txt"),
                                     h4("These sliders pertain to the epi-matrix using a summation approach:"),
                                     sliderInput(inputId="mod7",label="Sum Modifier for 7-0 match", min=0, max=1.0, value=0.15, step=0.05),
                                     sliderInput(inputId="mod8",label="Sum Modifier for 7-1 match", min=0, max=1.0, value=0.35, step=0.05),
                                     submitButton("Create Matrix", icon = NULL)
                        ),
                        
                        # Show a table of the uploaded data: 
                        mainPanel(
                          tabsetPanel(
                            tabPanel(title="Epi-Table",
                                     br(),
                                     shinysky::hotable("scoretable")
                            ),                            
                            tabPanel(title="Sum Heatmap",
                                     h3("Heatmap based on the source scorings and the penalty sliders from the sidebar"),
                                     downloadButton("downloadSourceHeatmap", "Download Heatmap"),
                                     downloadButton("downloadSourceMatrix", "Download Full Matrix File"),
                                     downloadButton("downloadSourcePairwise", "Download Pairwise File"),
                                     br(),
                                     plotOutput("source_heatmap", width=750, height=750)
                                     
                            ),
                            tabPanel(title = "Chord Diagram", 
                                      h4("This diagram shows the relationships between the sources that are at least 80% similar"), 
                                      br(),
                                      div(id = 'jschord', class = 'jschord')
#                                       column(2, includeHTML(path = "hair.html")),
                                     
                                      )
                                )
                          ))),
######################## *******************************  ************************************** ################
#                                            NavTab for Epi-Matrix                                              #
######################## *******************************  ************************************** ################
                   tabPanel("EpiMatrix",
                            pageWithSidebar(                              
                              # Application title
                              headerPanel("Epi-Matrix: Similarity Scoring using Epidemiological Data"),
                              
                              # Sidebar with a slider input for number of observations
                              sidebarPanel(h4("Epi-Matrix: Similarity Scoring using Epidemiological Data"), 
                                           p("This app will take the source, temporal, and geographical data from your input dataset and compute
                                             numerical similarity coefficients for each strain"),
                                           p("Here are some template files to get started:"),
                                           shiny::a("Strain Data", href= "https://www.dropbox.com/s/6v5ka88lhtyrt5x/strain_data.txt?dl=1"),
                                           br(),
                                           shiny::a("Source Reference", href= "https://www.dropbox.com/s/hl3kiov5d97dt3a/source_data.txt?dl=1"),
                                           br(),br(),
                                           fileInput(inputId="strain_data",label="Upload Strain Data Here:",multiple=FALSE,accept=".txt"),
                                           fileInput(inputId="source_data",label="Upload Source Reference Here:",multiple=FALSE,accept=".txt"), 
                                           h4("Make the following sliders add up to 1.0"),
                                           sliderInput(inputId="source_coeff", label="Coefficient for Source Factor", min=0.0, max=1.0, value=0.5, step=0.05),
                                           sliderInput(inputId="temp_coeff", label="Coefficient for Temporal Factor", min=0.0, max=1.0, value=0.3, step=0.05),
                                           sliderInput(inputId="geog_coeff", label="Coefficient for Geographical Factor", min=0.0, max=1.0, value=0.2, step=0.05)
                                           ,submitButton("Create Matrix", icon = NULL)
                                           ),
                              
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                title="Epi-Matrix",
                                           downloadButton("downloadEpiData", "Download Similarity Data"),
                                           downloadButton("downloadEpiTable", "Download Similarity Table"),
                                           downloadButton("downloadEpiHeatmap", "Download Heatmap"),
                                           busyIndicator("Processing...", wait = 500),
                                           plotOutput("EpiHeatmap", width=1000, height=1000)
                                  )
                             )
                            ),
######################## *******************************  ************************************** ################
#                                            NavTab for CGF-Matrix                                              #
######################## *******************************  ************************************** ################
                   tabPanel("CGFMatrix",
                            pageWithSidebar(
                              
                              # Application title
                              headerPanel("CGF Analysis and Similarity Calculator"),   
                              # Sidebar with a slider input for number of observations
                              sidebarPanel(h4("CGF Matrix"),
                                           p("CGF Matrix will take the CGF data from your dataset and calculate the similarity scores for each pairing of strains"),
                                           br(),
                                           p("Upload your CGF data here:"),
                                           fileInput(inputId="cgf",label="Upload CGF file",multiple=FALSE,accept=".txt"),
                                           submitButton("Create Matrix", icon = NULL)
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                tabsetPanel(
                                  tabPanel(title="CGF Heatmap",
                                           h3("Heatmap based on the similarity scorings of your CGF Data"),
                                           downloadButton("downloadCGFHeatmap", "Download CGF Heatmap"),
                                           downloadButton("downloadCGFTable", "Download CGF Similarity Data"),
                                           br(),
                                           busyIndicator("Please wait... for datasets greater than 100, this can take a while", wait = 500),
                                           plotOutput("cgf_heatmap", width=1000, height=1000))
                                  
                                )
                              )
                            )),
######################## *******************************  ************************************** ################
#                                            NavTab for Comparisons                                             #
######################## *******************************  ************************************** ################
                   tabPanel("Compare",
                            pageWithSidebar(
                              
                              # Application title
                              headerPanel("Compare CGF and Epi Similarity Scorings"),   
                              # Sidebar to upload data from CGF and Epi Matrix Generators 
                              sidebarPanel(h4("Compare Epi with CGF Data"),
                                           p("Input the matrix data from the Epi-Matrix and CGF-Matrix Apps to compare the results"),
                                           p("Strains that are more closely related via their epi-relationships will be shown in", 
                                             span("green",style="color:green"), 
                                             "and those that are more strongly associated genetically will be shown in",
                                             span("blue.",style="color:blue")),
                                           br(),
                                           p("Upload your Epi data here:"),
                                           fileInput(inputId="epi_data",label="Upload Epi file",multiple=FALSE,accept=".txt"),
                                           p("Upload your Genetic Similarity (CGF) data here:"),
                                           fileInput(inputId="cgf_data",label="Upload Similarity (CGF) file",multiple=FALSE,accept=".txt"),
                                           submitButton("Create Matrix", icon = NULL)
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                tabsetPanel(
                                  tabPanel(title="Comparison Heatmap",
                                           h3("Heatmap based on the similarity scorings of your CGF Data versus the Epi Data"),
                                           br(),
                                           busyIndicator("Processing...", wait = 500),
                                           plotOutput("compare_heatmap", width=1000, height=1000)
                                           )))))
))
