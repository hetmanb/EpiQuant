# Server-side code for the source-tab 
library(rhandsontable)
#This code generates a table that changes depending on what is uploaded in the sidebar  
output$scoretable <- renderHotable({  
  # inFile <- input$source_scores
  # if ((is.null(inFile)) && (input$source_demo == TRUE)) {        
  #   return(read.table("../pub_data/sourcetest_v2.txt", header=T, sep='\t'))
  # }
  # read.table(inFile$datapath, header=T, sep='\t')
  read.table("pub_data/sourcetest_v2.txt", header=T, sep='\t')
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
# #  ** TEST ** Generates a heatmap displaying source similarities using modified equation ####
# output$source_heatmap2 <- renderD3heatmap({
#   inFile <- scoreDL()
#   if (is.null(inFile)) {
#     return(NULL)
#   }
#   m = (SourceMatrix2(source_data=inFile, mod8=input$mod8, mod7=input$mod7, mod14=input$mod14))
#   source_heatmap(m)
# })

############ Download Handlers: #############################

output$downloadSourceMatrix <- downloadHandler( 
  filename = c("SourceMatrix.txt"),
  content = function(file){
    # write.table(SourceMatrix(source_data = scoreDL(), mod8=input$mod8, mod7=input$mod7, mod0=input$mod0, mod14=input$mod14), file)
    write.table((SourceMatrix(scoreDL(), mod8=input$mod8, mod7=input$mod7, mod0=input$mod0, mod14=input$mod14)), file, sep = '\t')
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


##################### RHandsonTable ######################

## Handsontable
# observe({
#   if (!is.null(input$hot)) {
#     values[["previous"]] <- isolate(values[["DF"]])
#     DF = hot_to_r(input$hot)
#   } else {
#     if (is.null(values[["DF"]]))
#       DF <- DF
#     else
#       DF <- values[["DF"]]
#   }
#   values[["DF"]] <- DF
# })
# 
# output$hot <- renderRHandsontable({
#   DF <- values[["DF"]]
#   if (!is.null(DF))
#     rhandsontable(DF, useTypes = as.logical(input$useType), stretchH = "all")
# })
# 
# ## Save 
# observeEvent(input$save, {
#   fileType <- isolate(input$fileType)
#   finalDF <- isolate(values[["DF"]])
#   if(fileType == "ASCII"){
#     dput(finalDF, file=file.path(outdir, sprintf("%s.txt", outfilename)))
#   }
#   else{
#     saveRDS(finalDF, file=file.path(outdir, sprintf("%s.rds", outfilename)))
#   }
# }
# )
# 
# ## Cancel last action    
# observeEvent(input$cancel, {
#   if(!is.null(isolate(values[["previous"]]))) values[["DF"]] <- isolate(values[["previous"]])
# })
# 
# ## Add column
# output$ui_newcolname <- renderUI({
#   textInput("newcolumnname", "Name", sprintf("newcol%s", 1+ncol(values[["DF"]])))
# })
# observeEvent(input$addcolumn, {
#   DF <- isolate(values[["DF"]])
#   values[["previous"]] <- DF
#   newcolumn <- eval(parse(text=sprintf('%s(nrow(DF))', isolate(input$newcolumntype))))
#   values[["DF"]] <- setNames(cbind(DF, newcolumn, stringsAsFactors=FALSE), c(names(DF), isolate(input$newcolumnname)))
# })
# 
# 








