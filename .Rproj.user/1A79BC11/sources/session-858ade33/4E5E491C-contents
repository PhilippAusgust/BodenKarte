library(leaflet)
source("color_int.R")



showLabel = function(Data, Label = "Rasterpunkt",m, labelFloor = FALSE, showCircle = TRUE, valueInterpolate = "nFK", 
                     minVal = 100, maxVal = 250, colors = c(  "#ECCFCC" ,"#FFFFFF", "#00B7FF" ,"#1484FF" ,"#0F4581")){
  
  m = m 
  
  
  for (i in 1:nrow(Data)) {
    
    if(labelFloor){
      
      lab = paste("", floor(as.numeric(Data[i, Label])))
    }
    
    else{
      
      lab = paste(Data[i, Label])
    }
    
    print(Data[i, valueInterpolate])
          
    rgb = colorInterpolation(valueToInterpolate = as.numeric(Data[i, valueInterpolate]), 
                             colorspace= colors, 
                             minVal = minVal, maxVal = maxVal)
    
    #value = (as.numeric(Data[i, valueInterpolate]) - minimum) / (maximum - minimum)
    print(rgb)
   # rgbColor <- getColorFromValue(value, colors)
    rgbString <- paste("rgb(", rgb["r"], ",", rgb["g"], ",", rgb["b"], ")", sep = "")
    print(rgbString)
    
    m <- addLabelOnlyMarkers(m, labelOptions = labelOptions(style = list(
      "color" = "#000",
      "font-family" = "serif",
      "font-style" = "bold",
      "font-size" = paste(markerFontSize, "px", sep="")
    ), noHide = TRUE, direction = 'center', textOnly = TRUE), lng = as.numeric(Data[i,"lon"]), lat = as.numeric(Data[i,"lat"]), label = lab) 
    
    lon <- as.numeric(Data[i,"lon"])
    lat <- as.numeric(Data[i,"lat"])
    
    if(showCircle){
      m <- addCircleMarkers(m, radius = 25, lng = lon, lat = lat, stroke = FALSE, fillColor = rgbString, fillOpacity = 1.0)
    }
    
    
  }
  return(m)
}











