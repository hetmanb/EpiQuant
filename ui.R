# Shiny UI page

# Load required libraries for UI
library(shiny)
library(shinysky)
library(rCharts)
library(d3heatmap)
library(shinydashboard)
library(knitr)
library(shinyforms)

#Create an empty container for housing the Shiny alerts 

shiny_alert_container <- function(id) {
  tagList(
    tags$head(
      singleton(tags$script(src="js/shinyalert.js"))
    ),
    div(id=paste(id), class='shinyalert')
  )
}


# create the ShinyDashboard header: 

d_header <- dashboardHeader( 
  title= div(img(src = "favicon-32x32.png", " epiQuant")),
  # load javascript files
  dropdownMenu( type = "messages",
                messageItem(
                  from = "Server",
                  message = "Welcome to epiQuant!"
                )
  )
)

d_sidebar <- dashboardSidebar(
  width = "300px",
  sidebarMenu(id = "tabs",
              sidebarMenuOutput("menu")

    )
)

d_body <- dashboardBody(
  tags$head(
    tags$script(src="lib/underscore.js"),
    tags$script(src="js/mapper.js"),
    tags$script(src ="js/chord.js"),
    tags$script(src ="js/chord2.js"),
    tags$script(src ="www/lib/leaflet.js"),
    tags$script('!function(d,s,id){var js,fjs=d.getElementsByTagName(s)    [0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");')
    ,tags$style(HTML("
                               body {
                    width: 100% !important;
                    max-width: 100% !important;
                    }

                    ")
               )
  ),
  tabItems(
    tabItem(tabName = "dashboard",
            shiny_alert_container('welcome_alert'),
            h2("Welcome to epiQuant"),
            p("epiQuant is a webapp for quantifying epidemiological features of bacterial isolates for use with genomic data in public health."),
            p("See the publication", a(href="http://jcm.asm.org/content/early/2017/02/09/JCM.01945-16", "here!")), 
            fluidRow(
              column(width = 8,
                     box(# Abstract from EQ Paper
                       width = NULL, 
                       title = "Abstract from the manuscript",
                       status = 'info',
                       includeMarkdown("abstract.md")
                       ),
                     box(
                       width = NULL, 
                       title = "Projects by my colleagues:",
                       status = 'danger',
                       infoBox(title = "SISTR:", href = "http://lfz.corefacility.ca/sistr-app", icon = icon("spinner"), color = "orange", fill = T, width = 6, 
                               subtitle = "The Salmonella in-silico Typing Resource"),
                       infoBox(title = "SuperPhy:", href = "http://lfz.corefacility.ca/superphy/", icon = icon("rocket"), color = "purple", fill = T, width = 6, 
                               subtitle = "Phylogenetic and epidemiological analysis of pathogens")
                       )
                     ),
              
              column(width = 4,
                     box(# twitter feed:
                       width = NULL, 
                       title = "Top Rstats Tweets:",
                       status = "info",
                       a("#RStats:",
                         class="twitter-timeline",
                         href="https://twitter.com/hashtag/Rstats",
                         "data-widget-id" ="864864988397260800")
                       )
                     )
                    )
            ),
      tabItem(tabName = "source",
              p("Use this area for scoring your sources!"),
              fluidRow(
                box("Placeholder for scoring table!!", 
                    br(),
                    "some more text!",
                    sliderInput("slider", "Slider Input:", 1, 100, 50),
                    textInput("text1", "Text Input:")
                    )
                )
              ),
      tabItem(tabName = "epimatrix",
              p("Use this area for getting your epimatrix to work!")
              ),
      tabItem(tabName = "genmatrix",
              p("Use this area for computing your distance matrix of genetic data!")
              ),
      tabItem(tabName = "comparematrix",
              p("Use this area for analysing your results!")
              )
      )
  )
  

# Run content
dashboardPage(skin = "red", 
              d_header, 
              d_sidebar, 
              d_body)

