###################################### Function 2 SUM-Epimatrix##########################################################

cgf_calc <- function(data){
  d <- dist.gene(as.matrix(data), method = "percentage")
  d <- 1 - as.matrix(d)
  return(d) 
}

#Function 3 ######################################################################################################
#function that calls heatmap.2 to generate a heatmap from the matrix calculations 
cgf_heatmap_D3 <- function(m, color){
  
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
            cexRow = .3, cexCol = .3, srtCol = 45,
            revC=T, margins = c(14,14), keysize = 1,
            hclustfun = function(x) hclust(x,method = 'single'))
}
# Extras for offline analysis
# library(ape)
# library(gplots)
# m <- read.FASTA("~/Dropbox/0 - Publications_bh/EpiQuant_Pt_2/roary_out_1460672486/accessory_binary_genes.fa")
# m2 <- dist.gene(as.matrix(m), "percentage")
# m2 <- as.matrix(m2)
# m2 <- 1 - m2
# plot <- cgf_heatmap_pdf(m2, "E")
# data <- m2[plot$rowInd, plot$colInd]
# write.table(data, "WG_CGF_SimMatrix", sep = '\t', row.names = T)
# 
# pdf("WG_CGF_Heatmap.pdf", height = 15, width = 15)
# cgf_heatmap_pdf(m2, "A")
# dev.off()

