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
library(ape)
library(twitteR)
library(DT)
library(shinyforms)
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
  
  rv <- reactiveValues()
  
  isolate(prepend_shiny_alert(session,
                              'welcome_alert',
                              alert_shiny_start_msg,
                              alert_level='danger'))
  isolate(prepend_shiny_alert(session,
                              'sourcematrix_alert',
                              alert_sourcematrix_start_msg,
                              alert_level='info'))
  isolate(prepend_shiny_alert(session,
                              'source_heat_alert',
                              alert_sourcematrix_heat_msg,
                              alert_level='info'))
  
  isolate(prepend_shiny_alert(session,
                              'source_chord_alert',
                              alert_sourcematrix_chord_msg,
                              alert_level='info'))
  isolate(prepend_shiny_alert(session,
                              'epi_chord_alert',
                              alert_epimatrix_chord_msg,
                              alert_level='info'))

  
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Welcome", icon = icon("hand-spock-o"), tabName = "dashboard"),
      menuItem("Source Analysis", tabName = "source", icon = icon("th")),
      conditionalPanel("input.tabs === 'source'",
        icon = icon("file-excel-o"), 
        fileInput("file1", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain.csv")
      )
      ),
      menuItem("Epidemiology Summary", icon = icon("th"), tabName = "epimatrix"),
      menuItem("Genomic Similarity Analysis", icon = icon("th"), tabName = "genmatrix"),
      menuItem("Comparison Analysis", icon = icon("th"), tabName = "comparematrix"),
      menuItem("Visit the Repo!", icon = icon("github"), href = "https://github.com/hetmanb/EpiQuant")
    )
  }
  )
  isolate({updateTabItems(session, "tabs", "dashboard")})
  outputOptions(output, "menu", suspendWhenHidden = FALSE)


   
   
}
)
