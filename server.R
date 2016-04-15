
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
library(RColorBrewer)
library(markdown)
library(rCharts)
library(dendextend)
library(d3heatmap)
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
  
##################################################################################################
############################ Server functions for Source Matrix ##################################

  
#This code generates a table that changes depending on what is uploaded in the sidebar  
  output$scoretable <- renderHotable({  
      inFile <- input$source_scores
      if ((is.null(inFile)) && (input$source_demo == TRUE)) {        
        return(read.table("pub_data/sourcetest_v2.txt", header=T, sep='\t'))
      }
      read.table(inFile$datapath, header=T, sep='\t')
    }, readOnly = F)
  
# Reactive table for source scores that updates with the hotable input:
  
  scoreDL <- reactive({hot.to.df(input$scoretable)})

#  Generates a heatmap displaying source similarities  ####
  output$source_heatmap <- renderD3heatmap({
    inFile <- scoreDL()
    if (is.null(inFile)) {
      return(NULL)
    }
    m = (SourceMatrix(source_data=inFile, mod8=input$mod8, mod7=input$mod7, mod0=input$mod0, mod14 = input$mod14))
    source_heatmap(m)
  })
  #  ** TEST ** Generates a heatmap displaying source similarities using modified equation ####
  output$source_heatmap2 <- renderD3heatmap({
    inFile <- scoreDL()
    if (is.null(inFile)) {
      return(NULL)
    }
    m = (SourceMatrix2(source_data=inFile, mod8=input$mod8, mod7=input$mod7, mod14=input$mod14))
    source_heatmap(m)
  })
  
############ Download Handlers: #############################
  
  output$downloadSourceMatrix <- downloadHandler( 
    filename = c("SourceMatrix.txt"),
    content = function(file){
      write.table(SourceMatrix(source_data = scoreDL(), mod8=input$mod8, mod7=input$mod7, mod0=input$mod0, mod14=input$mod14), file)
    })
  output$downloadSourcePairwise <- downloadHandler( 
    filename = c("SourcePairwise.txt"),
    content = function(file){
      write.table(melt(SourceMatrix(scoreDL(), mod8=input$mod8, mod7=input$mod7, mod0=input$mod0, mod14=input$mod14)), sep='\t', file)
    })  
  output$downloadSourceHeatmap <- downloadHandler( 
    filename = c("SourceHeatmap.pdf"),
    content = function(file){
      pdf(file, width=16, height=16)
      source_heatmap_pdf(SourceMatrix(scoreDL(), mod8=input$mod8, mod7=input$mod7, mod0=input$mod0, mod14=input$mod14))
      dev.off()
    })
  ############ Download Handlers ** TEST ** 2 : #############################
  
  output$downloadSourceMatrix2 <- downloadHandler( 
    filename = c("SourceMatrix.txt"),
    content = function(file){
      write.table(SourceMatrix2(source_data = scoreDL(), mod8=input$mod8, mod7=input$mod7, mod14=input$mod14), file)
    })
  output$downloadSourcePairwise2 <- downloadHandler( 
    filename = c("SourcePairwise.txt"),
    content = function(file){
      write.table(melt(SourceMatrix2(scoreDL(), mod8=input$mod8, mod7=input$mod7, mod14=input$mod14)), sep='\t', file)
    })  
  output$downloadSourceHeatmap2 <- downloadHandler( 
    filename = c("SourceHeatmap.pdf"),
    content = function(file){
      pdf(file, width=16, height=16)
      source_heatmap_pdf(SourceMatrix2(scoreDL(), mod8=input$mod8, mod7=input$mod7, mod14=input$mod14))
      dev.off()
    })
############ Functions for the Source Chord Diagram JS Output: #############################

  chord_in <- reactive({ 
    melt(SourceMatrix(scoreDL(), mod8=input$mod8, mod7=input$mod7, mod0=input$mod0, mod14 = input$mod14))
  })
  
  chord_file <- reactive({
    chord_table(chord_in(), input$chord_low, input$chord_high)
  })
  
  chord_color <- reactive({
    chordcolor(chord_in(), input$chord_low, input$chord_high)
  })


  output$jschord <- reactive({
    # List of arguments given to the chord.js file     
    list(
      filepath = chord_file(), 
      color = chord_color()
    )
  })



##################################################################################################
############################ Server functions for EpiMatrix ######################################  
  
### Calculate the temporal and geographical relations based upon the epi-input dataset: ##########  
  temporal <- reactive({
    inFile <- input$strain_data
    if ((is.null(inFile)) && (input$epi_demo == TRUE)) {   
#       print("inFile is null")
      return(temp_calc(read.table('pub_data/updated_straindata274.txt', header=TRUE, sep='\t')))
      }
      temp_calc(read.table(inFile$datapath, header=TRUE, sep='\t'))
  })      
  geography <- reactive({
    inFile <- input$strain_data
    if ((is.null(inFile)) && (input$epi_demo == TRUE)) { 
#       print("inFile is null")
      return(geog_calc(read.table('pub_data/updated_straindata274.txt', header=TRUE, sep='\t')))
    }
    geog_calc(read.table(inFile$datapath, header=TRUE, sep='\t'))
  })  

######## Calculate the epi relations based upon the epi-input and source datasets: ######
  table <- reactive({
    if(is.null(input$strain_data)) { 
      inFile <- read.table('pub_data/updated_straindata274.txt', header=T, sep='\t')  
    }     
       else { 
         inFile <- read.table(input$strain_data$datapath, header=T, sep='\t') 
       } 
    if(is.null(input$source_data)) { 
      sinFile <- read.table('pub_data/SourcePairwise_v2.txt', header=T, sep = '\t')  
    }    
        else { 
          sinFile <- read.table(input$source_data$datapath, header=T, sep='\t') 
        }
    return(EpiTable(inFile, sinFile, geography(), temporal(), input$source_coeff, input$temp_coeff, input$geog_coeff))
  })  


######## Generate a heatmap of the results and display on the Main Output : #############
  output$EpiHeatmap <- renderD3heatmap({
    EpiHeatmap(EpiMatrix(table()))
     })  

####### Generate a map with the locations from the strain info file using rCharts and Leaflet:
   output$epiMap <- renderMap({
     if(is.null(input$strain_data)) { inFile <- read.table('pub_data/updated_straindata274.txt', header=T, sep='\t')  }     
      else { inFile <- read.table(input$strain_data$datapath, header=T, sep='\t') } 
       return(EpiMap(inFile))

   })

############ Functions for the Epi-Chord Diagram JS Output: #############################

chord2_in <- reactive({ 
  melt(EpiMatrix(table()))
})

chord2_file <- reactive({
  chord_table(chord2_in(), input$chord2_low, input$chord2_high)
})
chord2_color <- reactive({
  chord2color(chord2_in(), input$chord2_low, input$chord2_high)
})

output$jschord2 <- reactive({
  # List of arguments given to the chord.js file     
  list(
    filepath = chord2_file(), 
    color = chord2_color()
  )
})



#### Download Handlers for epiData and Heatmaps: #####

  output$downloadEpiData <- downloadHandler( 
    filename = c("Epi_Sim_Data.txt"),
    content = function(file){
      write.table(EpiMatrix(table()), file, sep="\t", row.names=T)
    })
  output$downloadEpiTable <- downloadHandler( 
    filename = c("Epi_Table.txt"),
    content = function(file){
      write.table(table(), file, sep="\t", row.names=F)
    })
  output$downloadEpiHeatmap <- downloadHandler( 
    filename = c("Epi_Heatmap.pdf"),
    content = function(file){
      pdf(file, width=25, height=25)
      EpiHeatmap_pdf(EpiMatrix(table()))
      dev.off()
    })

##################################################################################################
############################ Server functions for CGF-Matrix ###################################### 

###########     Generate the CGF Matrix #####################   
  cgf_matrix <- reactive({
    if (is.null(input$cgf) && (input$cgf_demo == TRUE))  {
      return(cgf_calc(data=read.table("data/demo_data/demo_58_hexresults.txt", header=T, sep='\t')))
    }
    cgf_calc(data=read.table(input$cgf$datapath, header=T, sep='\t'))
    })

##########  Generate the Heatmap and display   ##################### 

  cgfheatmap <- reactive({
    if ((input$cgf_demo == FALSE) && is.null(input$cgf)){
      return(NULL)
    }
    cgf_heatmap(cgf_matrix(), input$gen_type)
    })
  
  output$cgf_heatmap <- renderD3heatmap({
    cgfheatmap()
    })


########## Create the download button handlers for use with EpiMatrix ######

#Download handler to download the heatmap as a .pdf file:
  output$downloadCGFHeatmap <- downloadHandler( 
    filename = c("CGF-Heatmap.pdf"),
    content = function(file){
      pdf(file, width=15, height=15)
      cgf_heatmap_pdf(cgf_matrix(), input$gen_type)
      dev.off()
    })
  output$downloadCGFTable <- downloadHandler( 
    filename = c("CGF-SimTable.txt"),
    content = function(file){
      write.table(cgf_matrix(), file, sep='\t', row.names = T) 
    })  

##################################################################################################
#################  Server functions for the Comparison App #######################################



###########   Generate the Heatmap and display  ##################### 

  compareheatmap <- reactive({
    if (input$compare_demo == TRUE){
      CompareMatrix(cgf_data = read.table("data/demo_data/Hex-SimTable_58.txt",header = T, sep = '\t', check.names = F),
                    epi_data = read.table("data/demo_data/Epi_Sim_Data_58.txt", header = T, sep = '\t', check.names = F)
                    )}
      else {
        if(is.null(input$cgf_data)|is.null(input$epi_data)){
          return(NULL) }
            else {
              cgf_in <- read.table(input$cgf_data$datapath, header = T, sep='\t')
              epi_in <- read.table(input$epi_data$datapath, header = T, sep='\t')
              if(input$compare_raw == FALSE){
                  CompareMatrix(cgf_data=cgf_in, epi_data = epi_in)}
                    else {
                       CompareMatrixRaw(cgf_data=cgf_in, epi_data = epi_in)}    
        }
     }
  })
  

output$compare_heatmap <- renderD3heatmap({
  
            if (input$compare_demo == TRUE){
              CompareDisplay(compareheatmap(), 
                             read.table("data/demo_data/Hex-SimTable_58.txt",header = T, sep = '\t', check.names = F),
                             read.table("data/demo_data/Epi_Sim_Data_58.txt", header = T, sep = '\t', check.names = F),
                             type = input$clus_type, 
                             sigma_in = input$sigma)}  
            
            else {
              if(is.null(input$cgf_data)|is.null(input$epi_data)){
                return(NULL) }
              else {
                cgf <- read.table(input$cgf_data$datapath, header = T, sep='\t', check.names = F)
                epi <- read.table(input$epi_data$datapath, header = T, sep='\t', check.names = F)
                CompareDisplay(compareheatmap(), cgf, epi, input$clus_type, input$sigma)}
             }
           })
          
       
#### Server Code for TanglePlots: ####
tangle <- reactive({
  if (input$compare_demo == TRUE){
    gene_data <-  read.table("data/demo_data/Hex-SimTable_58.txt", header = T, sep = '\t', check.names = F)
    epi_data <-  read.table("data/demo_data/Epi_Sim_Data_58.txt", header = T, sep = '\t', check.names = F)
    tangle_helper(gene_data, epi_data, input$cut_epi, input$cut_cgf)
    }
  else {
    if(is.null(input$cgf_data)|is.null(input$epi_data)){
      return(NULL) }
    else {
      gene_data <- read.table(input$cgf_data$datapath, header = T, sep='\t', check.names = F)
      epi_data <- read.table(input$epi_data$datapath, header = T, sep='\t', check.names = F)
      tangle_helper(gene_data, epi_data, input$cut_epi, input$cut_cgf)
      }
  }
})
output$tangle<- renderPlot({
  tangle_plot(d = tangle(), k = input$num_k)
})


#Download handler to download the comparison heatmap as a .pdf file:



output$downloadCompareHeatmap <- downloadHandler( 
  filename = c("CGF-Epi_Heatmap.pdf"),
  content = function(file){
    pdf(file, width=15, height=15)
    if (input$compare_demo == TRUE){
      CompareDisplay_pdf(compareheatmap(), 
                     read.table("data/demo_data/Hex-SimTable_58.txt",header = T, sep = '\t', check.names = F),
                     read.table("data/demo_data/Epi_Sim_Data_58.txt", header = T, sep = '\t', check.names = F),
                     input$clus_type, input$sigma)}  
    
    else {
      if(is.null(input$cgf_data)|is.null(input$epi_data)){
        return(NULL) }
      else {
        cgf <- read.table(input$cgf_data$datapath, header = T, sep='\t', check.names = F)
        epi <- read.table(input$epi_data$datapath, header = T, sep='\t', check.names = F)
        CompareDisplay_pdf(compareheatmap(), cgf, epi, input$clus_type, input$sigma)}
    }
    dev.off()
  })

output$downloadCompareTable <- downloadHandler( 
  filename = c("CGF-Epi_SimTable.txt"),
  content = function(file){
    write.table(melt(compareheatmap()), file, sep='\t', row.names = F) 
  })  

output$DL_tanglegram <- downloadHandler( 
  filename = c("Tanglegram.pdf"),
  content = function(file){
    pdf(file, width=15, height=15)
    tangle_plot(d = tangle(), k = input$num_k)
    dev.off()
  })

})
