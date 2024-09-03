library(leaflet)




showLabel = function(Data, Label = "Rasterpunkt",m, labelFloor = FALSE, showCircle = TRUE, valueInterpolate = "nFK"){
  
  m = m 
  
  
  for (i in 1:nrow(Data)) {
    
    if(labelFloor){
      
      lab = paste("", floor(as.numeric(Data[i, Label])))
    }
    
    else{
      
      lab = paste(Data[i, Label])
    }
    
    value = (as.numeric(Data[i, valueInterpolate]) - minimum) / (maximum - minimum)
    print(value)
    rgbColor <- getColorFromValue(value, colors)
    rgbString <- paste("rgb(", rgbColor["r"], ",", rgbColor["g"], ",", rgbColor["b"], ")", sep = "")
    
    m <- addLabelOnlyMarkers(m, labelOptions = labelOptions(style = list(
      "color" = "#000",
      "font-family" = "serif",
      "font-style" = "bold",
      "font-size" = paste(markerFontSize, "px", sep="")
    ), noHide = TRUE, direction = 'center', textOnly = TRUE), lng = as.numeric(Data[i,"lon"]), lat = as.numeric(Data[i,"lat"]), label = lab) 
    
    lon <- as.numeric(Data[i,"lon"])
    lat <- as.numeric(Data[i,"lat"])
    
    if(showCircle){
      m <- addCircleMarkers(m, radius = 25, lng = lon, lat = lat, stroke = FALSE, fillColor = "white", fillOpacity = 1.0)
    }
    
    
  }
  return(m)
}











