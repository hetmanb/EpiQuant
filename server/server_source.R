# Server-side code for the source-tab 
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
scoreDL <- reactive({
  m <- hot.to.df(input$scoretable)
  SourceMatrix(m, mod8=input$mod71, mod7=input$mod70, mod0=input$mod00, mod14 = input$mod77)
})

#  Generates a heatmap displaying source similarities  ####
output$source_heatmap <- renderD3heatmap({
  inFile <- as.matrix(scoreDL())
  if (is.null(inFile)) {
    return(NULL)
  }
  # m = (SourceMatrix(source_data=inFile, mod8=input$mod71, mod7=input$mod70, mod0=input$mod00, mod14 = input$mod77))
  heatcolor<- colorRampPalette(c("orangered2", "goldenrod1", "white"))(512)
  # m <- shared_sourcematrix$data()
  
  d3heatmap(inFile, dendrogram = 'both', colors = rev(heatcolor),
            Rowv = T, cexRow = 0.80, cexCol = 0.80,
            reorderfun = function(d, w) rev(reorder(d, w)),
            revC=TRUE, hclustfun = function(x) hclust(x,method = 'single'))
  
})

output$sourceChord <- renderchordNetwork({
  inFile <- melt(scoreDL())
  inFile <- subset(inFile, inFile$value > input$chord_range1[1] & inFile$value < input$chord_range1[2])
  inFile <- reshape2::recast(inFile, Var1 ~ Var2)
  row.names(inFile) <- inFile[,1]
  inFile <- inFile[,-1]
  if (is.null(inFile)) {
  return(NULL)
  }
  chordNetwork(inFile,
               labels = colnames(inFile),
               fontSize = 10,
               padding = 0.03,
               labelDistance = 50,
               width = 400, 
               height = 400
               )
})







