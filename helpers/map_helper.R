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
sources <- unique(df2$Source)
df2$color <- colors[match(df2$Source, sources)]
df2$popup <- paste0("<p>Strain:  ", df2$Strain, 
                    "<br>Source:  ", df2$Source, 
                    "<br>Year: ", df2$Year)

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
             radius: 5,
             fillColor: feature.properties.color || 'red', 
             color: '#000',
             weight: 1,
             fillOpacity: 0.8
             })
             } !#"           
             )
epimap$set(width = 1200, height = 800)
# map3$enablePopover(TRUE)
epimap
}
# read.table(file = "strain_data.txt", header = T, sep = "\t")