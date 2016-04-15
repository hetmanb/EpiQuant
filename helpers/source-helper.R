###################################### Function 2 SUM-Epimatrix##########################################################

SourceMatrix <- function(source_data,mod8,mod7, mod0, mod14) {
  # load the epi-matrix that was made in Excel
  data <- as.matrix(source_data)
  #this is the standardized coefficient of differences - any sample will have this much difference in it, inherently
  mod8 <- mod8
  mod7 <- mod7
  mod0 <- mod0
  mod14 <- mod14
  #this generates all the possible non-redundant row pairings available in the matrix (eg 1vs2, 1vs3... 2vs3,etc) 
  #and saves it as a variable 'xselect'
  #Use for full matrix:
  xselect <- expand.grid(1:nrow(data), 1:nrow(data))
  #add together all the pairwise comparisons from the epi-table and hopefully save it as a variable to tweak the nips of:
  
  epitab <- list()
  for (i in 1:nrow(xselect)) {
    
    if(xselect[i,1] == xselect[i,2]){
      y <- 1
    }
    else {
      x1 <- as.numeric(data[xselect[i,2], c(2:ncol(data))])
      x2 <- as.numeric(data[xselect[i,1], c(2:ncol(data))])
      
      
      y <- x1+x2
      y <- ifelse(y == 0, mod0, 
                  ifelse(y==1, 0, 
                         ifelse(y==2, 1, 
                                ifelse(y==7, mod7, 
                                       ifelse(y==8, mod8, mod14)))))
    }
    
    epitab[[i]] <- y
  }
  sum.table <- vector()
  #Distance values = 1 - sum of similarities / number of attributes
  for (i in 1:nrow(xselect)){
      sum.table[i] <- ( ((sum(epitab[[i]]))/(length(epitab[[i]]))))
  }
  sim.matrix <- matrix(data=sum.table, nrow=nrow(data), ncol=nrow(data))
  rownames(sim.matrix) <- data[1:nrow(data),1]
  colnames(sim.matrix) <- data[1:nrow(data),1]
  return(sim.matrix)
}


#Function 3 ######################################################################################################
#function that calls heatmap.2 to generate a heatmap from the matrix calculations 
source_heatmap <- function(m){
  heatcolor<- colorRampPalette(c("orangered2", "goldenrod1", "white"))(512)
  d3heatmap(m, dendrogram = 'both', colors = rev(heatcolor), 
            Rowv = T, cexRow = 0.80, cexCol = 0.80, 
            reorderfun = function(d, w) rev(reorder(d, w)),
            revC=TRUE, hclustfun = function(x) hclust(x,method = 'single'))  
}

source_heatmap_pdf <- function(m){
  heatcolor<- colorRampPalette(c("orangered2", "goldenrod1", "white"))(512)
  heatmap.2(m, col=rev(heatcolor), Rowv = TRUE , trace='none',
            cexRow = 1.1, cexCol = 1.1, srtCol = 45,
            revC=T, margins = c(14,14), keysize = 1,
            hclustfun = function(x) hclust(x,method = 'single'))
}





