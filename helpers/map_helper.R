EpiMap <- function(input_data) {

# map_data <- input_data
# markers <- map_data[,6:7]
# mark2 <- apply(markers, 1, as.list)


map_data <- input_data
# markers <- map_data[,6:7]
# mark2 <- apply(markers, 1, as.list)


# max of 12 cats in colorbrewer, gotta add 6 more
colors <- brewer.pal(12, "Paired")

df2 <- map_data
# Set a color scale dependant on the source of each strain
sources <- unique(df2$Source)
df2$color <- colors[match(df2$Source, sources)]

# Create a column containing the GPS coords for each strain to be used as a lookup 
df2$locs <- paste(df2$Latitude, ",", df2$Longitude)
locs <- unique(df2$locs)

# Use the locs list as a lookup to find all the strains that are at each of the unique locations 
a <- list()
for(i in locs[1:length(locs)]){  
  searchTerm  <- i 
  searchVector <- which(df2$locs==searchTerm)
  searchNames <- as.vector(df2$Strain)[searchVector]
  a[[i]] <- searchNames
}
a_matrix<- (as.matrix(a))
df2$strains <- a_matrix[match(df2$locs, row.names(a_matrix))]


# Use the locs list as a lookup to find all the SOURCES that are at each of the unique locations 
b <- list()
for(i in locs[1:length(locs)]){  
  searchTerm  <- i 
  searchVector <- which(df2$locs==searchTerm)
  searchNames <- as.vector(df2$Source)[searchVector]
  b[[i]] <- searchNames
}
b_matrix<- (as.matrix(b))
df2$sources <- b_matrix[match(df2$locs, row.names(b_matrix))]

#List only the UNIQUE sources for each location point 
for(i in 1:nrow(df2)){
  df2$sources[[i]] <- paste(unique(df2$sources[[i]]), sep = ",")
}

# Count the number of strains in each location and store it as two variables - radius to be used for marker size
for(i in 1:nrow(df2)){
  df2$length[i] <- length(df2$strains[[i]])
  df2$radius[i] <- length(df2$strains[[i]])
  
#Convert the radius into a more manageable scale for the map   
}
for(i in (1:nrow(df2))){
  df2$radius[i] <- (((df2$radius[i] - min(df2$radius)) * 8) / (max(df2$radius)-min(df2$radius)) + 4)
} 

# The items desired to be seen in the popup box:
df2$popup <- paste0("<p><strong>Location:</strong> ", df2$City," ", df2$Province.State,
                    "<br><strong>Number of Strains:</strong> ", df2$length, 
                    "<br><strong>Sources Present:</strong> ", df2$sources)



markers <- apply(df2, 1, as.list)

epimap <- Leaflet$new()
epimap$setView(c(55, -75), zoom = 3)
# epimap$geocsv(as.list(map_data[,6:7]))
# epimap$marker(c(map_data[,6], map_data[,7]), bindPopup = "<p> this is a test </p>")
epimap$geoJson(toGeoJSON(markers, lat = 'Latitude', lon = 'Longitude'),
             onEachFeature = '#! function(feature, layer){
             layer.bindPopup(feature.properties.popup)
           } !#',
             pointToLayer =  "#! function(feature, latlng){
           return L.circleMarker(latlng, {
           radius: feature.properties.radius,
           fillColor: feature.properties.color || 'red', 
           color: '#000',
           weight: 1.1,
           fillOpacity: 0.04
           })
           } !#"             
)
epimap$set(width = 1200, height = 800)
# map3$enablePopover(TRUE)
epimap
}
# read.table(file = "strain_data.txt", header = T, sep = "\t")