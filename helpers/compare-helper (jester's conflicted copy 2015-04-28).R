
CompareMatrix <- function(cgf_data, epi_data) {
# Import data from shiny 
  cgf<- cgf_data
  epi <- epi_data
  row.names(cgf) <- colnames(cgf)
  row.names(epi) <- colnames(epi)
  
  melt_cgf <- melt(as.matrix(cgf))
  melt_epi <- melt(as.matrix(epi))
  
# Rank the similarities from min - max, to allow for 'fair' comparisons
  melt_cgf$rank <- rank(melt_cgf$value, ties.method = 'random')
  melt_epi$rank <- rank(melt_epi$value, ties.method = 'random')
  
  
  merged_data <- merge(x=melt_cgf, y=melt_epi, by = c("Var1", "Var2"), all = T)
  merged_data$rank.x <- merged_data$rank.x / max(merged_data$rank.x)
  merged_data$rank.y <- merged_data$rank.y / max(merged_data$rank.y)
  
# Subtract the Epi from the CGF data. If result is >0 the similarity was stronger via CGF. If the result
# is <0, then the similarity was stronger via Epi relationships. 
  merged_data$minus <- merged_data$rank.x - merged_data$rank.y
  merged_data <- merged_data[,c(1:2,7)]
  
  
  caster <- acast(merged_data, formula = Var1 ~ Var2)
  return(data.matrix(caster))
}

CompareDisplay <- function(m, cgf_data, epi_data, type){
  
  clus_type <- switch(type, 
                      A = as.dendrogram(hclust(dist(data.matrix(cgf_data))), method = 'single'),
                      B = as.dendrogram(hclust(dist(data.matrix(epi_data))), method = 'single'), 
                      C = as.dendrogram(hclust(dist(m))), method = 'single')
  
#   clus_type <- ifelse(type == 1, as.dendrogram(hclust(dist(cgf_data))),
#                       ifelse(type==2, as.dendrogram(hclust(dist(epi_data))), 
#                                             as.dendrogram(hclust(dist(m)))))
    
heatcolor<- colorRampPalette(c("darkgreen","white","white","white","darkblue"))(512)
# heatcolor<- colorRampPalette(c("black", "red"))(2)
heatmap.2(m, col=heatcolor, Rowv= clus_type, symm=TRUE, Colv ="Rowv", trace='none',keysize=0.6, revC=TRUE)
# Green denotes CGF sim, Blue denotes CGF Sim.   
}
# CompareDisplay <- function(m){
#   heatcolor<- colorRampPalette(c("darkgreen","white","darkblue"))(512)  
#   castermap <- heatmap.2(m, col=heatcolor, Rowv=TRUE, symm=TRUE, Colv ="Rowv", trace='none',keysize=0.6, revC=TRUE)
# #   Green denotes CGF sim, Blue denotes CGF Sim.   
# }


