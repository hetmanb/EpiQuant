library(reshape2)
library(gplots)
library(fossil)

##########################################################################################
######## Function for generating time data from input datafile ###########################
temp_calc <- function(input_data, temp_window){
  #### import data from data file ####
  timedata <- as.data.frame(input_data) 
  #timedata <- read.delim(file="datafile.txt", header=TRUE, sep="\t")
  
  #### Create a new column to contain the concatenated temporal data ####
  timedata$date <- NA
  timedata$date <- as.Date(paste(timedata$Year, timedata$Month, timedata$Day, sep = "-"))
  #### Create an empty matrix and populate it with the pairwise distances ####
  time_matrix <- matrix(data = NA, nrow=nrow(timedata), ncol=nrow(timedata))
  time_matrix <- as.matrix(dist(x=timedata$date, diag=TRUE, upper=TRUE, method = 'euclidean'), nrow=nrow(timedata), ncol=nrow(timedata))

  #Make 10 days the startpoint for dropping off in similarity - i.e. 10 days = 100% similar still
  time_matrix <- time_matrix + 10
  temp_window <- temp_window + 10
  
  
  # anything less than or equal to temp_window becomes 0 distance
  time_matrix[time_matrix <= temp_window] <- 10
  
  # anything greater than temp_window starts to get distance penalty starting at log10(11) etc 
  time_matrix[time_matrix > temp_window] <- time_matrix[time_matrix > temp_window] - (temp_window - 10)
  
  # logarithmic scale base 10
  time_log <- log(time_matrix, base = 10) 
  time_log[time_log == -Inf ] <- 0
  
  # rescale everything between 0-1
  if(max(time_log) == 0){
    time_log <- 0
  } else {
    time_log <- ((time_log-min(time_log)) / (max(time_log)-min(time_log)))
  }
  
  #### Import row and column names from the original datafile and melt data for easy reading ####
  rownames(time_log) <- timedata$Strain
  colnames(time_log) <- timedata$Strain
  time_melt <- melt(time_log)
  return(time_melt)
}  

##########################################################################################
######## Function for generating geography data from input datafile ######################
geog_calc <- function(input_data){
  
  #### Read data table from project folder - this contains locations and their GPS Coordinates ####  
  geogdata <- input_data

  #### Create a matrix containing the pair-wise distances (in km) between all the locations using the fossil package ####
  geog_matrix <- as.matrix(earth.dist(lats=geogdata[,6:7], dist=TRUE))
  
  #### Calculate the maximum distance in the matrix and divide all values by it to arrive at a max distance of 1 ####
  # geog_matrix[geog_matrix < 10] <- 10
  geog_matrix <- geog_matrix + 10
  geog_matrix <- log10(geog_matrix)
  
  if(max(geog_matrix) == 0){
      geog_matrix <- 0
    } else {
      geog_matrix <- ((geog_matrix-min(geog_matrix)) / (max(geog_matrix)-min(geog_matrix)))
    }
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

  #### Lookup and merge temporal data: ####
  strain_sims <- merge.data.frame(strain_sims, temp_matrix, by.x= c("Strain.1", "Strain.2"), by.y = c("Var1", "Var2")) 
  colnames(strain_sims) <- c("Strain.1", "Strain.2", "Source.1", "Source.2", "Date.1", "Date.2", "Location.1", "Location.2", "Source.Dist", "Temp.Dist")
  
  #### Lookup and merge Geography data : ####
  strain_sims <- merge.data.frame(strain_sims, geog_matrix, by.x= c("Strain.1", "Strain.2"), by.y = c("Var1", "Var2"))
  colnames(strain_sims) <- c("Strain.1", "Strain.2", "Source.1", "Source.2", "Date.1", "Date.2", "Location.1", "Location.2", "Source.Dist", "Temp.Dist", "Geog.Dist")
  
  #### Finalize the similarity matrix and calculate the overall similarity between the strains: ####
  str.matrix <- strain_sims
  str.matrix$Total.Dist <- NA
  str.matrix$Total.Dist <- sqrt( (((str.matrix$Source.Dist^2)*x) + ((str.matrix$Temp.Dist^2)*y) + ((str.matrix$Geog.Dist^2)*z)) )
  str.matrix$Epi.Sym <- 1 - str.matrix$Total.Dist
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
#   epi.sym <- 1 - epi.cast
  return(epi.cast)
}

##########################################################################################
######## Function to return a heatmap of the final EPIMATRIX function ####################
EpiHeatmap_d3 <- function(m){
#   heatcolor<- colorRampPalette(c("darkgreen","yellowgreen","white"))(512)
  heatcolor<- colorRampPalette(c("white","yellowgreen","darkgreen"))(512)
  d3heatmap(m, dendrogram = 'both', colors=rev(heatcolor), Rowv = T, 
            reorderfun = function(d, w) rev(reorder(d, w)),
            revC=TRUE, hclustfun = function(x) hclust(x,method = 'single'))
}

EpiHeatmap_pdf <- function(m){
  heatcolor<- colorRampPalette(c("white","yellowgreen","darkgreen"))(512)
  # heatcolor<- colorRampPalette(c("#efedf5", "#bcbddc", "#756bb1"))(512)
  # heatcolor<- colorRampPalette(c('#eff3ff','#bdd7e7','#6baed6','#3182bd','#08519c'))(512)
  plot <- heatmap.2(m, col=rev(heatcolor), Rowv = T, Colv = 'Rowv', trace='none',
            srtCol = 45, key.title = NA, key.ylab=NA,
            revC=T, margins = c(10,10), keysize = 1.3, key = T,
            xlab=NULL, ylab=NULL, 
            labRow = NA, labCol = NA,
            hclustfun = function(x) hclust(x,method = 'single'))
  data <- m[plot$rowInd, plot$colInd]
  return(list(plot, data))
}
# 
# 
# 
# 
# # Extras for offline analysis - not included in App. 
# # 
# main_input <- read.table("../../2016_03 EpiQuant Manuscript/EpiQuant_Analysis_2/Can654/Can654_straindata.txt", header = T, sep = '\t')
# source_input <- read.table("../../../0 - Publications_bh/2016_03 EpiQuant Manuscript/EpiQuant_Analysis_2/Can654/Pairwise_Source.txt", header = T, sep = '\t', check.names = F)
# 
# s <- .5
# t <- .3
# g <- .2
# 
# d <- EpiTable(main_input, source_input, geog_calc(main_input), temp_calc(main_input, 0), s, t, g)
# 
# d$no.days <- abs(as.Date(d$Date.1) - as.Date(d$Date.2))
# 
# plot(x = d$no.days[d$no.days < 300], y = d$Temp.Dist[d$no.days < 300])
# 
# 
# 
# plot(x = d$no.km, y = d$Geog.Dist)
# plot(x = d2$value, y = d2$Geog.Dist)
# 
# 
# d2 <- EpiMatrix(d)
# d3 <- EpiHeatmap_pdf(d2)
# 
# 
# plot.new()
# tiff("~/Desktop/epi_heat(50_30_20).tiff", width = 20, height = 20, res = 300, units = "cm")
# EpiHeatmap_pdf(d2)
# dev.off()
# 
# # 
# ## too slow to use xlsx - better off to write to txt and import
# write.table(d3[[2]], "~/Dropbox/0 - Publications_bh/2016_03 EpiQuant Manuscript/EpiQuant_Analysis_2/Can654/epi_slice_folders/Epi(50_30_20)/heatdata_epiSim654(50_30_20).txt", sep = '\t')
# 
# 
# 
# dend_df <- d3[[2]]
# hc_dend <- hclust(dist(dend_df)) 
# cut_50 <- cutree(hc_dend, h = .1*max(hc_dend$height))
# 
# 
# 
# 
# write.table(d, "~/Desktop/Epi_Summary_Long_Table(50_30_20).txt", sep = '\t')
# write.xlsx2(x = d3[[2]], file = "epi_heat_data.xlsx", sheetName = "Epi(80_10_10)", append = T)

# 
# 



