# Shiny UI page

# Load required libraries for UI
library(shiny)
library(shinysky)
library(rCharts)
library(d3heatmap)
library(shinydashboard)
library(knitr)

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
  title= div(img(src = "favicon-32x32.png"), "epiQuant"),
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
    tags$style(HTML("
                               body {
                    width: 100% !important;
                    max-width: 100% !important;
                    }
                    
                    "))
  ),
  tabItems(
    tabItem(tabName = "dashboard",
            h2("Welcome to EpiQuant"),
            p("Epiquant is a webapp for quantifying epidemiological features of bacterial isolates for use with genomic data in public health."),
            p("See the publication", a(href="http://jcm.asm.org/content/early/2017/02/09/JCM.01945-16", "here!")), 
            fluidRow(
              box(
                width = 8,
                title = "Abstract from the manuscript",
              uiOutput('abstract')
              ),
              box(
                width = 4, status = "info",
                title = "Top #RStats tweets:",
                tableOutput("tweetTable")
              )
            ),
    tabItem(tabName = "source",
            p("Use this area for scoring your sources!"),
            fluidRow(
              box(
                "Placeholder for scoring table!!", br(), "some more text!",
                sliderInput("slider", "Slider Input:", 1, 100, 50),
                textInput("text1", "Text Input:")
              ),
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

