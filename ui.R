# Shiny UI page

# Load required libraries for UI
library(shiny)
library(shinysky)
library(rCharts)
library(d3heatmap)
library(shinydashboard)
library(knitr)
library(crosstalk)

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
    # tags$script(src="lib/underscore.js"),
    # tags$script(src="js/mapper.js"),
    # tags$script(src ="js/chord.js"),
    # tags$script(src ="js/chord2.js"),
    # tags$script(src ="www/lib/leaflet.js"),
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
    source(file.path("ui", "tab_welcome.R"),  local = TRUE)$value,
    source(file.path("ui", "tab_source.R"),  local = TRUE)$value,
    source(file.path("ui", "tab_epimatrix.R"),  local = TRUE)$value,
    source(file.path("ui", "tab_genmatrix.R"),  local = TRUE)$value,
    source(file.path("ui", "tab_compare.R"),  local = TRUE)$value
    )
  )
  

# Run content
dashboardPage(skin = "red", 
              title = "epiQuant",
              d_header, 
              d_sidebar, 
              d_body)

