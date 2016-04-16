###################################### Function 2 SUM-Epimatrix##########################################################

cgf_calc <- function(data){
  d <- dist.gene(as.matrix(data), method = "percentage")
  d <- 1 - as.matrix(d)
  return(d) 
}

#Function 3 ######################################################################################################
#function that calls heatmap.2 to generate a heatmap from the matrix calculations 
cgf_heatmap <- function(m, color){
  
  col_scale <- switch(color,
                      A = colorRampPalette(c("white","lightblue","darkblue"))(512),
                      B = colorRampPalette(c("white","orange","darkorange"))(512),
                      C = colorRampPalette(c("white","red","darkred"))(512),
                      D = colorRampPalette(c("white","forestgreen","darkgreen"))(512),
                      E = colorRampPalette(c("white","lightblue","lightblue","blue","darkblue"))(512))
  d3heatmap(m, dendrogram = 'both', colors=col_scale, revC=TRUE,
            reorderfun = function(d, w) rev(reorder(d, w)),
            hclustfun = function(x) hclust(x,method = 'single'))
}

##### for Download handler static image: 
cgf_heatmap_pdf <- function(m, color){
  
  col_scale <- switch(color,
                      A = colorRampPalette(c("white","lightblue","darkblue"))(512),
                      B = colorRampPalette(c("white","orange","darkorange"))(512),
                      C = colorRampPalette(c("white","red","darkred"))(512),
                      D = colorRampPalette(c("white","forestgreen","darkgreen"))(512),
                      E = colorRampPalette(c("white","lightblue","blue","darkblue"))(512))
  heatmap.2(m, col=col_scale, Rowv = TRUE , trace='none',
            cexRow = 1.1, cexCol = 1.1, srtCol = 45,
            revC=T, margins = c(14,14), keysize = 1,
            hclustfun = function(x) hclust(x,method = 'single'))

}




