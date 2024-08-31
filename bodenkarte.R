library("readxl")
library("writexl")
source("gk_zu_gps.R")
source("barycentric.R")
source("color_functions.R")
library(leaflet)
raster <- read_excel("GPS_nFK.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)

#configuration 
colors <- data.frame("r" = c(236, 255, 0, 20, 15), "g" = c(207, 255, 183, 132, 69), "b" = c(204, 255, 255, 255, 129))
resolution <- 50
minimum <- 100.0
maximum <- 250.0
markerFontSize <- 15
showInterpolation <- TRUE # values: TRUE or FALSE
showCircleMarkers <-FALSE # values: TRUE or FALSE
showRectangleMarkers <- TRUE # values: TRUE or FALSE
showLabels <- TRUE

#determine the aabb size of the grid
lat_min = min(raster["lat"])
lat_max = max(raster["lat"])
lon_min = min(raster["lon"])
lon_max = max(raster["lon"])

#size of the rectangles
rect_length <- (lon_max - lon_min) / (resolution)
scaled_rect_length <- rect_length #* (51 / 80)
half_rect_length <- rect_length / 2.0 #* (51 / 80)

#init the map
m <- leaflet()
m <- addTiles(m)
m <- addProviderTiles(m, 'Esri.WorldImagery')

if(showLabels){
# show labels (and circle markers)
for (i in 1:nrow(raster)) {
  value = (as.numeric(raster[i, "nFK"]) - minimum) / (maximum - minimum)
  rgbColor <- getColorFromValue(value, colors)
  rgbString <- paste("rgb(", rgbColor["r"], ",", rgbColor["g"], ",", rgbColor["b"], ")", sep = "")
  m <- addLabelOnlyMarkers(m, labelOptions = labelOptions(style = list(
    "color" = "#000",
    "font-family" = "serif",
    "font-style" = "bold",
    "font-size" = paste(markerFontSize, "px", sep="")
  ), noHide = TRUE, direction = 'center', textOnly = TRUE), lng = as.numeric(raster[i,"lon"]), lat = as.numeric(raster[i,"lat"]), label = paste("", floor(as.numeric(raster[i, "nFK"])))) 
  
  lon <- as.numeric(raster[i,"lon"])
  lat <- as.numeric(raster[i,"lat"])
  
 if (showRectangleMarkers) {
    m <- addRectangles(m, lng1 = lon - half_rect_length, lat1 = lat - half_rect_length, lng2 = lon + half_rect_length, lat2 = lat + half_rect_length, stroke = FALSE, fillOpacity = 1.0, fillColor = rgbString)
  }
  
  if (showCircleMarkers) {
    m <- addCircleMarkers(m, radius = 25, lng = lon, lat = lat, stroke = FALSE, fillColor = rgbString, fillOpacity = 1.0)
  }
  
}
}
# calculate and show interpolation
if(showInterpolation) {
  
  triangles <- get_triangles_from_coordinates(raster[,c("lon", "lat")])
  
  for(i in 0 : (resolution-1))
  {
    # show the progress of the calculation
    print(paste("Fortschritt: ",  (i)/ (resolution) *100, "%", sep=""))
    for (j in 0 : floor((lat_max - lat_min) / scaled_rect_length)) {
      x_middle <- lon_min + (i * rect_length) + (((i + 1) * rect_length) - (i * rect_length)) / 2.0
      y_middle <- lat_min + (j * scaled_rect_length) + (((j + 1) * scaled_rect_length) - (j * scaled_rect_length)) / 2.0
      
      x_start <- lon_min + (i * rect_length)
      x_end <- lon_min + ((i + 1) * rect_length)
      y_start <- lat_min + (j * scaled_rect_length)
      y_end <- lat_min + ((j + 1) * scaled_rect_length)
      
      rgbString <- "#ffffff"
      triangleCornersAndWeights <- get_triangle_corners_and_bary_weights(list("x" = x_middle, "y" = y_middle), raster[,c("lon", "lat")], triangles)
      
      found <- FALSE
      
      if (!is.null(triangleCornersAndWeights)) {
        found = TRUE
        
        index1 <- triangleCornersAndWeights$cornerIndices$p1
        index2 <- triangleCornersAndWeights$cornerIndices$p2
        index3 <- triangleCornersAndWeights$cornerIndices$p3
        
        m1 <- triangleCornersAndWeights$weights$m1
        m2 <- triangleCornersAndWeights$weights$m2
        m3 <- triangleCornersAndWeights$weights$m3
        
        value = as.numeric(raster[index1, "nFK"]) * m1 + as.numeric(raster[index2, "nFK"]) * m2 + as.numeric(raster[index3, "nFK"]) * m3
        value = (value - minimum) / (maximum - minimum)
        rgbColor <- getColorFromValue(value, colors)
        rgbString <- paste("rgb(", rgbColor["r"], ",", rgbColor["g"], ",", rgbColor["b"], ")", sep = "")
      }
      
      if (found) {
        m <- addRectangles(m, lng1 = x_start, lat1 = y_start, lng2 = x_end, lat2 = y_end, stroke = FALSE, fillOpacity = 1.0, fillColor = rgbString)  
      }
      
    }
  } 
}

# add legend to the map
color_palett_string <- ""
if (nrow(colors) > 0) {
  color_palett_string <- getColorHexString(colors[1,])
}
for (i in 2 : (nrow(colors))) {
  color_palett_string <- c(getColorHexString(colors[i,]), color_palett_string)
}

color_pallet <- colorNumeric(color_palett_string, domain = c(minimum, maximum), na.color = "Black") 
m <- addLegend(
  m, "topright", 
  pal = color_pallet, 
  values = c(minimum, maximum), 	
  title = "nFK mm", opacity = 1, 
  labFormat = 
    function(type, cuts, p) { 
      n = length(cuts) 	
      cuts[n] = as.character(minimum) 	
      for (i in 2:(n-1)){
        cuts[i] = "" #as.character(paste(floor((i/n) * 150) + 100))
        #print(as.character(paste(floor((i/n) * 150) + 100)))
      } 	
      cuts[1] = as.character(maximum)
      paste0(cuts[-n], cuts[-1])
    }
  ) 

#finally show the map
m
