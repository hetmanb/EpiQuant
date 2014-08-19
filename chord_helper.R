chord_table <- function(data, low, high){

  chord_table <- data

#   chord_table <- unique(data.frame(ct[, c(3,4,9)]))
  colnames(chord_table) <- c("has", "prefers", "count")
  chord_table <- unique(chord_table)
  chord_table <- subset(chord_table, subset = (chord_table$count >= low) & (chord_table$count <= high) )
  
    
  # chord_table$color <- max.col(chord_table[1:nrow(chord_table), 3:5], "random")
  # chord_table <- chord_table[,c(1,2,6,7)]
  
  # colnames(chord_table) <- c(as.character("Strain1"), as.character("Strain2"), "1", "2", "3", "4")
  
  
  # chord_melt <- melt(chord_table)
  # chord_melt <- chord_melt[, c(1, 2, 4, 3)]
  
  
  # class(chord_table[,(1)]) #<- "string"
  # class(chord_table[,(2)]) #<- "string"
  return(chord_table)
}


#   write.csv(chord_table, "../EpiQuant/www/data/chordtemp.csv", row.names =F)
  