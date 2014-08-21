
CompareMatrix <- function(cgf_data, epi_data) {

  cgf<- cgf_data
  epi <- epi_data
  
  
  melt_cgf <- melt(as.matrix(cgf))
  melt_epi <- melt(as.matrix(epi))
  
  # x <- max(melt_cgf$value) - min(melt_cgf$value)
  # y <- max(melt_epi$value) - min(melt_epi$value)
  
  melt_cgf$rank <- rank(melt_cgf$value, ties.method = 'random')
  melt_epi$rank <- rank(melt_epi$value, ties.method = 'random')
  
  
  merged_data <- merge(x=melt_cgf, y=melt_epi, by = c("Var1", "Var2"), all = T)
  merged_data$rank.x <- merged_data$rank.x / max(merged_data$rank.x)
  merged_data$rank.y <- merged_data$rank.y / max(merged_data$rank.y)
  
  merged_data$minus <- merged_data$rank.x - merged_data$rank.y
  merged_data <- merged_data[,c(1:2,7)]
  
  
  caster <- acast(merged_data, formula = Var1 ~ Var2)
  return(data.matrix(caster))
}

CompareDisplay <- function(m){
  heatcolor<- colorRampPalette(c("darkgreen","white","darkblue"))(512)  
  castermap <- heatmap.2(m, margins = c(10,10), col=heatcolor, trace='none',keysize=0.6, revC=TRUE)
  
}