# Helper script for use with the shiny-epimatrix app 
epimatrix <- function(epi.,coeff.,mod8.,mod7.) {
  # load the epi-matrix that was made in Excel
  epi <- as.matrix(epi.)
  #this is the standardized coefficient of differences - any sample will have this much difference in it, inherently
  coeff <- coeff.
  mod8 <- mod8.
  mod7 <- mod7.
  #this generates all the possible non-redundant row pairings available in the matrix (eg 1vs2, 1vs3... 2vs3,etc) 
  #and saves it as a variable 'xselect'
  #Use for full matrix:
  xselect <- expand.grid(1:nrow(epi), 1:nrow(epi), include.equals=TRUE)
  
  #add together all the pairwise comparisons from the epi-table and hopefully save it as a variable to tweak the nips of:
  
  epitab <- list()
  for (i in 1:nrow(xselect)) {
    
    x1 <- as.numeric(epi[xselect[i,2], c(3:ncol(epi))])
    x2 <- as.numeric(epi[xselect[i,1], c(3:ncol(epi))])
    y <- x1+x2
    y <- ifelse(y == 0, 1, 
                ifelse(y==1, coeff, 
                       ifelse(y==2, 1, 
                              ifelse(y==7, (((1-coeff)/mod7)+coeff), 
                                     ifelse(y==8, (((1-coeff)/mod8)+coeff), 1)))))
    epitab[[i]] <- y
  }
  prod.table <- vector()
  for (i in 1:nrow(xselect)){
    prod.table[i] <- prod(epitab[[i]])
  }
  sim.matrix <- matrix(data=prod.table, nrow=nrow(epi), ncol=nrow(epi))
  rownames(sim.matrix) <- epi[1:nrow(epi),1]
  colnames(sim.matrix) <- epi[1:nrow(epi),1]
  return(sim.matrix)
}
###################################### Function 2 SUM-Epimatrix##########################################################

epimatrix2 <- function(epi.,mod8_2,mod7_2) {
  # load the epi-matrix that was made in Excel
  epi <- as.matrix(epi.)
  #this is the standardized coefficient of differences - any sample will have this much difference in it, inherently
  mod8 <- mod8_2
  mod7 <- mod7_2
  #this generates all the possible non-redundant row pairings available in the matrix (eg 1vs2, 1vs3... 2vs3,etc) 
  #and saves it as a variable 'xselect'
  #Use for full matrix:
  xselect <- expand.grid(1:nrow(epi), 1:nrow(epi), include.equals=TRUE)
  
  #add together all the pairwise comparisons from the epi-table and hopefully save it as a variable to tweak the nips of:
  
  epitab <- list()
  for (i in 1:nrow(xselect)) {
    
    x1 <- as.numeric(epi[xselect[i,2], c(3:ncol(epi))])
    x2 <- as.numeric(epi[xselect[i,1], c(3:ncol(epi))])
    y <- x1+x2
    y <- ifelse(y == 0, 1, 
                ifelse(y==1, 0, 
                       ifelse(y==2, 1, 
                              ifelse(y==7, mod7_2, 
                                     ifelse(y==8, mod8_2, 1)))))
    epitab[[i]] <- y
  }
  sum.table <- vector()
  for (i in 1:nrow(xselect)){
    sum.table[i] <- ((sum(epitab[[i]]))/(length(epitab[[i]])))
  }
  sim.matrix <- matrix(data=sum.table, nrow=nrow(epi), ncol=nrow(epi))
  rownames(sim.matrix) <- epi[1:nrow(epi),1]
  colnames(sim.matrix) <- epi[1:nrow(epi),1]
  return(sim.matrix)
}
#Function 3 ######################################################################################################
#function that calls heatmap.2 to generate a heatmap from the matrix calculations 
epi_heatmap <- function(m){
  heatcolor<- colorRampPalette(c("white","orange","orangered2"))(512)
  heatmap.2(m, col=heatcolor, margins=c(25, 25), trace='none',keysize=0.8, revC=TRUE)
}