chord_table <- function(data, low, high){

  chord_table <- data
  colnames(chord_table) <- c("has", "prefers", "count")
  chord_table <- unique(chord_table)
  
  values <- chord_table$count
    
  ii <- cut(values, breaks = seq(min(values), max(values), len = length(values)), 
            include.lowest = TRUE)
  
  colors <- colorRampPalette(c("green", "yellow", "orange"))(length(values))[ii]
  
  chord_table$color <- (colors)
  chord_table <- subset(chord_table, subset = (chord_table$count >= low) & (chord_table$count <= high) )  
  palette <- as.vector(as.character(chord_table$color))
  return(chord_table)
}


chordcolor <- function(data, low, high){
  
  chord_table <- data
  colnames(chord_table) <- c("has", "prefers", "count")
  chord_table <- unique(chord_table)
  chord_table <- subset(chord_table, subset = (chord_table$count >= low) & (chord_table$count <= high) )
  
  
  values <- chord_table$count
  
  
  ii <- cut(values, breaks = seq(min(values), max(values), len = length(values)), 
            include.lowest = TRUE)
  
  colors <- colorRampPalette(c("#FFF5EB","#FEE6CE","#FDD0A2","#FDAE6B","#FD8D3C","#F16913","#D94801","#8C2D04"))(length(ii))[ii]
  
  chord_table$color <- (colors)
#   chord_table <- subset(chord_table, subset = (chord_table$count >= low) & (chord_table$count <= high) )
  
  palette <- as.vector(as.character(chord_table$color))
  return(palette)
}  
chord2color <- function(data, low, high){
  
  chord_table <- data
  colnames(chord_table) <- c("has", "prefers", "count")
  chord_table <- unique(chord_table)
  chord_table <- subset(chord_table, subset = (chord_table$count >= low) & (chord_table$count <= high) )
  
  
  values <- chord_table$count
  
  
  ii <- cut(values, breaks = seq(min(values), max(values), len = length(values)), 
            include.lowest = TRUE)
  
  colors <- colorRampPalette(c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#005A32"))(length(ii))[ii]
  
  chord_table$color <- (colors)
  #   chord_table <- subset(chord_table, subset = (chord_table$count >= low) & (chord_table$count <= high) )
  
  palette <- as.vector(as.character(chord_table$color))
  return(palette)
}  