library(readxl)
library(writexl)
source("gk_zu_gps.R")
source("barycentric.R")
source("color_functions.R")
source("Utilities.R")
library(leaflet)

raster <- read_excel("data/GPS_nFK.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)


createInterpolatedMap <- function(dataRaster = NULL, resolution = 50, 
                                  vaalueMin = 100.0, valueMax= 250.0,
                                  markerFontSize = 15, 
                                  mapType = "Esri.WorldImagery", 
                                  valueToInterpolate = "nFK",
                                  Label = "Rasterpunkt",labelFloor = FALSE){
  
   
                        "#ECCFCC" "#FFFFFF" "#00B7FF" "#1484FF" "#0F4581" # farben zwischen denen interpoliert wird. 
  colors <- data.frame("r" = c(236, 255, 0, 20, 15), 
                       "g" = c(207, 255, 183, 132, 69), 
                       "b" = c(204, 255, 255, 255, 129))
  
  #determine the aabb size of the grid
  lat_min = min(dataRaster["lat"])
  lat_max = max(dataRaster["lat"])
  lon_min = min(dataRaster["lon"])
  lon_max = max(dataRaster["lon"])
  
  
  #size of the rectangles
  rect_length <- (lon_max - lon_min) / (resolution)
  scaled_rect_length <- rect_length #* (51 / 80)
  half_rect_length <- rect_length / 2.0 #* (51 / 80)
  
  
  #init the map
  m <- leaflet() 
  m <- addTiles(m)
  m <- addProviderTiles(m, mapType)
  print(m)
  
  res1 = showLabel(Data = dataRaster, Label = Label, m= m, labelFloor = labelFloor, showCircle = TRUE)

  
  
  print(res1)
  # print(m)
  #return(m)
  
  
}



createInterpolatedMap(dataRaster = raster, Label = "Rasterpunkt", labelFloor = FALSE)

