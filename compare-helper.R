
CompareMatrix <- function(cgf_data, epi_data) {

  cgf<- cgf_data
  epi <- epi_data
  
  
  melt_cgf <- melt(as.matrix(cgf))
  melt_epi <- melt(as.matrix(epi))
  
  merged_data <- merge(x=melt_cgf, y=melt_epi, by = c("Var1", "Var2"), all = T)
  merged_data$minus <- merged_data[,3]-merged_data[,4]
  merged_data <- merged_data[,c(1:2,5)]
  
  caster <- acast(merged_data, formula = Var1 ~ Var2)
  return(data.matrix(caster))
}

CompareDisplay <- function(m){
  heatcolor<- colorRampPalette(c("darkgreen","white","darkblue"))(512)  
  castermap <- heatmap.2(m, margins = c(10,10), col=heatcolor, trace='none',keysize=0.6, revC=TRUE)
  
}