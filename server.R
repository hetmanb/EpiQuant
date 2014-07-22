
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(gplots)
library(reshape2)
library(fossil)
library(shinysky)
source("source-helper.R")
source("epi-helper.R")
source("cgf-helper.R")
source("compare-helper.R")


shinyServer(function(input, output, session) {

##################################################################################################
############################ Server functions for Source Matrix ##################################
  observe({
  showshinyalert(session, id= "alert1", HTMLtext = "hey man!", styleclass = 'warning' )
    })
# This code generates a table that changes depending on what is uploaded in the sidebar  
  output$scoretable <- renderHotable({  
      inFile <- input$source_scores
      if (is.null(inFile)) {        
        return(read.table("data/source_scorings.txt", header=T, sep='\t'))
      }
      read.table(inFile$datapath, header=T, sep='\t')
    }, readOnly = F)
   
  
# Reactive variable for source scores that updates with the hotable input:
  scoreDL <- reactive({ hot.to.df(input$scoretable)})
  

#  Generates a heatmap displaying source similarities  ####

 
  output$source_heatmap <- renderPlot({
    inFile <- scoreDL()
    if (is.null(inFile)) {
      return(NULL)
    }
    source_heatmap(SourceMatrix(source_data=inFile, mod8=input$mod8, mod7=input$mod7))
  })

  
############ Download Handlers: #############################

output$downloadSourceMatrix <- downloadHandler( 
  filename = c("SourceMatrix.txt"),
  content = function(file){
    write.table(SourceMatrix(source_data = scoreDL(), mod8=input$mod8, mod7=input$mod7), file)
  })
output$downloadSourcePairwise <- downloadHandler( 
  filename = c("SourcePairwise.txt"),
  content = function(file){
    write.table(melt(SourceMatrix(scoreDL(), mod8=input$mod8, mod7=input$mod7)), sep='\t', file)
  })  
output$downloadSourceHeatmap <- downloadHandler( 
  filename = c("SourceHeatmap.pdf"),
  content = function(file){
    pdf(file, width=15, height=15)
    source_heatmap(SourceMatrix(scoreDL(), mod8=input$mod8, mod7=input$mod7))
    dev.off()
  })

##################################################################################################
############################ Server functions for EpiMatrix ######################################  
  
### Calculate the temporal and geographical relations based upon the epi-input dataset: ##########  
temporal <- reactive({
  if (is.null(input$strain_data)){
    return(NULL)
  }
  temp_calc(read.table(input$strain_data$datapath, header=TRUE, sep='\t'))
  })      
geography <- reactive({
  if (is.null(input$strain_data)){
    return(NULL)
  }
  geog_calc(read.table(input$strain_data$datapath, header=TRUE, sep='\t'))
  })  

######## Calculate the epi relations based upon the epi-input and source datasets: ######
table <- reactive({
  if ((is.null(input$strain_data))|is.null(input$source_data)){
    return(NULL)
  }
  EpiTable(main_input=read.table(input$strain_data$datapath, header=TRUE, sep='\t'), source_input=read.table(input$source_data$datapath, header=TRUE, sep='\t'), geog_input=geography(), temp_input=temporal(), source_coeff=input$source_coeff, temp_coeff=input$temp_coeff, geog_coeff=input$geog_coeff)
  })  

######## Calculate the similarity matrix based on the EpiTable results: ######
matrix <- reactive({
  if ((is.null(input$strain_data))|is.null(input$source_data)){
    return(NULL)
  }
  EpiMatrix(table())
  }) 


######## Generate a heatmap of the results and display on the Main Output : #############
heatmap <- reactive({
  if ((is.null(input$strain_data))|is.null(input$source_data)) {
    return(NULL)
  }
  EpiHeatmap((matrix()))
  })  
  
output$EpiHeatmap <- renderPlot({
  heatmap()
  })

#### Download Handlers for Data and Heatmaps: #####

output$downloadEpiData <- downloadHandler( 
  filename = c("Epi_Sim_Data.txt"),
  content = function(file){
    write.table(matrix(), file, sep="\t")
  })
output$downloadEpiTable <- downloadHandler( 
  filename = c("Epi_Table.txt"),
  content = function(file){
    write.table(table(), file, sep="\t")
  })
output$downloadEpiHeatmap <- downloadHandler( 
  filename = c("Epi_Heatmap.pdf"),
  content = function(file){
    pdf(file, width=25, height=25)
    EpiHeatmap(matrix())
    dev.off()
  })

##################################################################################################
############################ Server functions for CGF-Matrix ###################################### 

###########     Generate the CGF Matrix #####################   
cgf_matrix <- reactive({
  if (is.null(input$cgf)) {
    return(NULL)
  }
  cgf_calc(data=read.table(input$cgf$datapath, header=T, sep='\t'))
  })

##########  Generate the Heatmap and display   ##################### 

cgfheatmap <- reactive({
  if (is.null(input$cgf)) {
    return(NULL)
  }
  cgf_heatmap(cgf_matrix())
  })

output$cgf_heatmap <- renderPlot({
  cgfheatmap()
  })


########## Create the download button handlers for use with EpiMatrix ######

#Download handler to download the heatmap as a .pdf file:
output$downloadCGFHeatmap <- downloadHandler( 
  filename = c("CGF-Heatmap.pdf"),
  content = function(file){
    pdf(file, width=15, height=15)
    cgf_heatmap(cgf_matrix())
    dev.off()
  })
output$downloadCGFTable <- downloadHandler( 
  filename = c("CGF-SimTable.txt"),
  content = function(file){
    write.table(cgf_matrix(), file, sep='\t') 
  })  

##################################################################################################
#################  Server functions for the Comparison App #######################################



###########   Generate the Heatmap and display  ##################### 

compareheatmap <- reactive({
  if (is.null(input$cgf_data)|is.null(input$epi_data)) {
    return(NULL)
    }
  cgf_in <- read.table(input$cgf_data$datapath, header = T, sep='\t')
  epi_in <- read.table(input$epi_data$datapath, header = T, sep='\t')
  CompareMatrix(cgf_data=cgf_in, epi_data = epi_in)
  })

# output$compare_heatmap <- renderTable({
#     
#     if (is.null(input$cgf_data)|is.null(input$epi_data)) {
#       return(NULL)
#     }
#     compareheatmap()
#   })


output$compare_heatmap <- renderPlot({
  if (is.null(input$cgf_data)|is.null(input$epi_data)) {
    return(NULL)
    }
  CompareDisplay(compareheatmap())
  })




})
