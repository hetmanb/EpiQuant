# library(reshape2)
# library(gplots)
# library(fossil)

##########################################################################################
######## Function for generating time data from input datafile ###########################
temp_calc <- function(input_data){
  #### import data from data file ####
  timedata <- as.data.frame(input_data) 
  #timedata <- read.delim(file="datafile.txt", header=TRUE, sep="\t")
  
  #### Create a new column to contain the concatenated temporal data ####
  timedata$date <- NA
  timedata$date <- as.Date(paste(timedata$Year, timedata$Month, timedata$Day, sep = "-"))
  #### Create an empty matrix and populate it with the pairwise distances ####
  time_matrix <- matrix(data = NA, nrow=nrow(timedata), ncol=nrow(timedata))
  time_matrix <- as.matrix(dist(x=timedata$date, diag=TRUE, upper=TRUE, method = 'euclidean'), nrow=nrow(timedata), ncol=nrow(timedata))
  
  #### Convert all the distances to a log value and normalize them based on the max distance ####
  time_log <- log10(time_matrix) 
  time_log[time_log == -Inf ] <- 0
  max_log <- max(time_log)
  norm_time_log <- time_log / max_log
  # norm_time_log <- 1 - norm_time_log
  
  #### Import row and column names from the original datafile and melt data for easy reading ####
  rownames(norm_time_log) <- timedata$Strain
  colnames(norm_time_log) <- timedata$Strain
  time_melt <- melt(norm_time_log)
  return(time_melt)
}  

##########################################################################################
######## Function for generating geography data from input datafile ######################
geog_calc <- function(input_data){
  
  #### Read data table from project folder - this contains locations and their GPS Coordinates ####  
  geogdata <- input_data
  # d <- geogdata  
  
  #### Create a matrix containing the pair-wise distances (in km) between all the locations using the fossil package ####
  geog_matrix <- as.matrix(earth.dist(lats=geogdata[,6:7], dist=TRUE))
  
  #### Calculate the maximum distance in the matrix and divide all values by it to arrive at a max distance of 1 ####
  geog_matrix <- log10(geog_matrix)
  geog_matrix[geog_matrix == -Inf ] <- 0
  max_d <- max(geog_matrix)
  geog_matrix <- geog_matrix / max_d
  # geog_matrix <- 1 - geog_matrix
  
  #### Import row and column names from the original datafile ####
  colnames(geog_matrix) <- geogdata[, 1]
  rownames(geog_matrix) <- geogdata[, 1]
  
  #### Create a melted pairwise distance table for easier readability ####
  geog_melt <- melt(geog_matrix) 
  return(geog_melt)
}

##########################################################################################################
######## Function to return table of all the epi-similarities and final strain similarity ################

EpiTable <- function(main_input, source_input, geog_input, temp_input, source_coeff, temp_coeff, geog_coeff){
  #### Read data into memory from previous outputs ####
  datafile <- main_input
  source_matrix <- source_input
  geog_matrix <- geog_input
  temp_matrix <- temp_input
  
  x <- source_coeff 
  y <- temp_coeff
  z <- geog_coeff
  
  
  #### Create the pairwise table for lookups ####
  d <- expand.grid(1:nrow(datafile), 1:nrow(datafile))
  #### Create Empty matrix ####
  strain_sims <- matrix(ncol=8, nrow=(nrow(d)))
  
  
  #### Start populating matrix with pairwise strain names and their sources ####
  strain_sims[,1] <- as.character(datafile[(d[,1]),1])
  strain_sims[,2] <- as.character(datafile[(d[,2]),1])
  strain_sims[,3] <- as.character(datafile[(d[,1]),2])
  strain_sims[,4] <- as.character(datafile[(d[,2]),2])
  
  
  #### Make concatenated strings from the date and location data for matching to the similarity scores: ####
  datafile$Date <- as.Date(paste(datafile$Year, datafile$Month, datafile$Day, sep = "-"))
  datafile$Location <- as.character(paste(datafile$Country, datafile$Province, datafile$City, sep = "_"))
  
  #### Populate the matrix with the date and location pairwise data and rename columns for readability: ####
  strain_sims[,5] <- as.character(datafile[(d[,1]),11])
  strain_sims[,6] <- as.character(datafile[(d[,2]),11])
  strain_sims[,7] <- as.character(datafile[(d[,1]),12])
  strain_sims[,8] <- as.character(datafile[(d[,2]),12])
  colnames(strain_sims) <- c("Strain.1", "Strain.2", "Source.1", "Source.2", "Date.1", "Date.2", "Location.1", "Location.2")
  
  #### Lookup and merge source data : ####
  strain_sims <- merge.data.frame(strain_sims, source_matrix, by.x = c("Source.1", "Source.2"), by.y= c("Var1", "Var2"))
  strain_sims <- strain_sims[, c(3,4,1,2,5,6,7,8,9)]
  colnames(strain_sims) <- c("Strain.1", "Strain.2", "Source.1", "Source.2", "Date.1", "Date.2", "Location.1", "Location.2", "Source.Dist")
  # strain_sims$Source.Sim <- (1 - strain_sims$Source.Sim) 
  
  #### Lookup and merge temporal data: ####
  strain_sims <- merge.data.frame(strain_sims, temp_matrix, by.x= c("Strain.1", "Strain.2"), by.y = c("Var1", "Var2")) 
  colnames(strain_sims) <- c("Strain.1", "Strain.2", "Source.1", "Source.2", "Date.1", "Date.2", "Location.1", "Location.2", "Source.Dist", "Temp.Dist")
  
  #### Lookup and merge Geography data : ####
  strain_sims <- merge.data.frame(strain_sims, geog_matrix, by.x= c("Strain.1", "Strain.2"), by.y = c("Var1", "Var2"))
  colnames(strain_sims) <- c("Strain.1", "Strain.2", "Source.1", "Source.2", "Date.1", "Date.2", "Location.1", "Location.2", "Source.Dist", "Temp.Dist", "Geog.Dist")
  
  #### Finalize the similarity matrix and calculate the overall similarity between the strains: ####
  str.matrix <- strain_sims
  str.matrix$Total.Dist <- NA
  #str.matrix$total <- ((str.matrix$Source.Sim*x) + (str.matrix$Temp.Sim*y) + (str.matrix$Geog.Sim*z))
  str.matrix$Total.Dist <- sqrt( ((str.matrix$Source.Dist^2)*x) + ((str.matrix$Temp.Dist^2)*y) + ((str.matrix$Geog.Dist^2)*z) ) 
  return(str.matrix)
}  

##########################################################################################################
######## Function to return matrix of just strains and final similarity scores for building graphics #####

EpiMatrix <- function(table){
  epi.matrix <- table
  epi.matrix <- epi.matrix[,c(1,2,12)]
  epi.cast <- dcast(epi.matrix, formula= Strain.1 ~ Strain.2, value.var = "Total.Dist")
  epi.cast <- as.matrix(epi.cast[,2:ncol(epi.cast)]) 
  rownames(epi.cast) <- colnames(epi.cast)
  
  return(epi.cast)
}


##########################################################################################
######## Function to return a heatmap of the final EPIMATRIX function ####################
EpiHeatmap <- function(m){
  heatcolor<- colorRampPalette(c("darkgreen","yellowgreen","white"))(512)
  d3heatmap(m, dendrogram = 'both', colors=heatcolor, Rowv = T, 
            reorderfun = function(d, w) rev(reorder(d, w)),
            revC=TRUE, hclustfun = function(x) hclust(x,method = 'single'))
}

EpiHeatmap_pdf <- function(m){
  heatcolor<- colorRampPalette(c("darkgreen","yellowgreen","white"))(512)
  heatmap.2(m, col=heatcolor, Rowv = TRUE , trace='none', srtCol = 45, margins = c(12,14),
            keysize=0.6, revC=T, hclustfun = function(x) hclust(x,method = 'single'))
}

