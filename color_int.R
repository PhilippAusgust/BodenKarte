library(tidyverse)
source("color_conversion.R")



colorInterpolation <- function(valueToInterpolate = 170, 
                               colorspace= c( "#ECCFCC" ,"#FFFFFF", "#00B7FF" ,"#1484FF" ,"#0F4581"), 
                               minVal = 100, maxVal = 250){
  
  
  normalized_value <- (as.numeric(valueToInterpolate) - minVal) / (maxVal - minVal) # normalisiere den wert prozentual zwischen min und max
  normalized_value <- max(0, min(1, normalized_value))
  
  colors = hexToHSV(colorspace) # farben zwischen denen interpoliert werden soll 
  #print(colors)
  n <- nrow(colors) # reihen bestimmen
  
  
  if (normalized_value == 1) {
    return(data.frame("h" = colors$h[n], "s" = colors$s[n], "v" = colors$v[n]))
  }
  
  interval_index <- findInterval(normalized_value, seq(0, 1, length.out = n)) # in welchem Intervall liegt der zu interpolierende Wert in dem data frame der colors? 
  left <- interval_index # untere Grenze aus dem df colors 
  right <- min(left + 1, n) # obere Grenze aus dem df colors
  
  percentage <- (normalized_value - (left-1)/(n-1)) / (1/(n-1)) # wie weit it der zu interpolierende Farbwert fortgeschritten in dem Intervall in dem er liegt ? 
  
  h1 <- colors$h[left] # h value in % umrechnen 
  h2 <- colors$h[right] # h value in % umrechnen 
  #print(paste("h1:", h1, "h2:", h2, "percentage:", percentage))

  
  if (abs(h2 - h1) > 180) {
    if (h2 > h1) {
      h1 <- h1 + 360
    } else {
      h2 <- h2 + 360
    }
  }
  
  h <- (h1 + percentage * (h2 - h1)) %% 360
  s <- colors$s[left] + percentage * (colors$s[right] - colors$s[left])
  v <- colors$v[left] + percentage * (colors$v[right] - colors$v[left])
  
  
  df <- data.frame("h" = h, "s" = s, "v" = v)
  
  
  rgb_color <- hsv(df[1]/360, df[2], df[3])
  rgb_values <- col2rgb(rgb_color)
  dfrgb <- data.frame("r" = rgb_values[1], "g" = rgb_values[2], "b" = rgb_values[3])
  #print(paste("hsv(",h,",",s*100,"%",",",v*100,"%)"))

  return(dfrgb)

  
  }

colorInterpolation(valueToInterpolate = 141,
                   colorspace= c( "#ECCFCC" ,"#FFFFFF", "#00B7FF" ,"#1484FF" ,"#0F4581"),
                   minVal = 100, maxVal = 250)




c( "#84FF00" ,"#2AA05F", "#0026FF" ,"#D014FF" ,"#5D0F81")






