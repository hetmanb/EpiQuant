###################################### Function 2 SUM-Epimatrix##########################################################

cgf_calc <- function(data) {
  # load the epi-matrix that was made in Excel
  cgf <- as.matrix(data)
  #this is the standardized coefficient of differences - any sample will have this much difference in it, inherently
  #this generates all the possible non-redundant row pairings available in the matrix (eg 1vs2, 1vs3... 2vs3,etc) 
  #and saves it as a variable 'xselect'
  #Use for full matrix:
  xselect <- expand.grid(1:nrow(cgf), 1:nrow(cgf), include.equals=TRUE)
  
  #add together all the pairwise comparisons from the cgf-table and hopefully save it as a variable to tweak the nips of:
  
  cgftab <- list()
  for (i in 1:nrow(xselect)) {
    
    x1 <- as.numeric(cgf[xselect[i,2], c(2:ncol(cgf))])
    x2 <- as.numeric(cgf[xselect[i,1], c(2:ncol(cgf))])
    y <- x1+x2
    y <- ifelse(y == 0, 1, 
                ifelse(y==1, 0, 1))
    
    
    cgftab[[i]] <- y
  }
  sum.table <- vector()
  for (i in 1:nrow(xselect)){
    sum.table[i] <- ((sum(cgftab[[i]]))/(length(cgftab[[i]])))
  }
  sim.matrix <- matrix(data=sum.table, nrow=nrow(cgf), ncol=nrow(cgf))
  rownames(sim.matrix) <- cgf[1:nrow(cgf),1]
  colnames(sim.matrix) <- cgf[1:nrow(cgf),1]
  return(sim.matrix)
}


#Function 3 ######################################################################################################
#function that calls heatmap.2 to generate a heatmap from the matrix calculations 
cgf_heatmap <- function(m){
  heatcolor<- colorRampPalette(c("white","lightblue","darkblue"))(512)
  heatmap.2(m, col=heatcolor, trace='none',keysize=0.6, revC=TRUE)
}