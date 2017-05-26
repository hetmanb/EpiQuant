# Server code for Shiny App


# load required libraries
library(shiny)
library(gplots)
library(reshape2)
library(fossil)
library(shinysky)
library(RColorBrewer)
library(markdown)
library(rCharts)
library(dendextend)
library(d3heatmap)
library(networkD3)
library(ape)
library(DT)
source("helpers/cgf-helper.R", local = T)
source("helpers/chord_helper.R", local = T)
source("helpers/compare-helper.R", local = T)
source("helpers/epi-helper.R", local = T)
source("helpers/source-helper.R", local = T)
source("helpers/alert-helper.R", local = T)
source("helpers/map_helper.R", local = T)
source("helpers/tangle_helper.R", local = T)
source("helpers/wallace-helper.R", local=T)
source("helpers/source-helper2.R", local = T)

shinyServer(function(input, output, session) {
  
  # Create reactive values 
  rv <- reactiveValues()
  
  # Shiny alerts 
  isolate(prepend_shiny_alert(session,
                              'welcome_alert',
                              alert_shiny_start_msg,
                              alert_level='danger'))
  isolate(prepend_shiny_alert(session,
                              'source_alert',
                              alert_shiny_source_msg,
                              alert_level='info'))
  
  # create a dashboard sidebar menu
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Welcome", icon = icon("hand-spock-o"), tabName = "dashboard"),
      menuItem("Source Analysis", tabName = "source", icon = icon("cutlery")),
      # file input conditional on active sidebar item
      conditionalPanel("input.tabs === 'source'",
        icon = icon("file-excel-o"), 
        fileInput("file1", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain.csv")
      ), 
      sliderInput("mod70", label = "Weighting for 7-0:", min = 0, max = 1, value = 0.15),
      sliderInput("mod71", label = "Weighting for 7-1:", min = 0, max = 1, value = 0.35),
      sliderInput("mod77", label = "Weighting for 7-7:", min = 0, max = 1, value = 0.15),
      sliderInput("mod00", label = "Weighting for 0-0:", min = 0, max = 1, value = 0.95),
      sliderInput("chord_range1", label = "Thresholds for chordplot", min = 0, max = 1, value = c(0.6,0.95), dragRange = T)
      ),
      menuItem("Epidemiology Summary", icon = icon("share-alt"), tabName = "epimatrix"),
      menuItem("Genomic Similarity Analysis", icon = icon("th"), tabName = "genmatrix"),
      menuItem("Comparison Analysis", icon = icon("th"), tabName = "comparematrix"),
      menuItem("Visit the Repo!", icon = icon("github"), href = "https://github.com/hetmanb/EpiQuant")
    )
  }
  )
  isolate({updateTabItems(session, "tabs", "dashboard")})
  # keep sidebar items active when hidden, to allow carryover between tabs
  outputOptions(output, "menu", suspendWhenHidden = FALSE)


  # include logic for each tab written in separate server files for neatness
  source(file.path("server", "server_source.R"),   local = TRUE)$value
  # source(file.path("server", "tab-settings.R"),  local = TRUE)$value
  # source(file.path("server", "tab-analyze.R"),   local = TRUE)$value
  # source(file.path("server", "tab-results.R"),   local = TRUE)$value
  #  
}
)
