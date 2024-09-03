


wert = 154
max_value = 250
min_value = 100

normalized_value <- (as.numeric(wert) - min_value) / (max_value - min_value)
normalized_value <- max(0, min(1, normalized_value))



colors <- data.frame(
  "h" = c(0, 60, 120, 180, 240),    # Hue-Werte (in Grad, normalisiert zwischen 0 und 1)
  "s" = c(1, 1, 1, 1, 1),           # Saturation (immer 100% in diesem Beispiel)
  "v" = c(0.9, 0.8, 0.7, 0.6, 0.5)  # Value (Helligkeit)
)


n <- nrow(colors)
if (normalized_value == 1) {
  h_val = hsv(colors$h[n]/360, colors$s[n], colors$v[n])
  
  return(hsv(colors$h[n]/360, colors$s[n], colors$v[n]))
}



interval_index <- findInterval(normalized_value, seq(0, 1, length.out = n))
left <- interval_index
right <- left + 1

percentage <- (normalized_value - (left-1)/(n-1)) / (1/(n-1))


h1 <- colors$h[left] / 360
h2 <- colors$h[right] / 360


if (abs(h2 - h1) > 0.5) {
  if (h2 > h1) h2 <- h2 - 1 else h1 <- h1 - 1
}

h <- (h1 + percentage * (h2 - h1)) %% 
  
  
  
s <- colors$s[left] + percentage * (colors$s[right] - colors$s[left])
v <- colors$v[left] + percentage * (colors$v[right] - colors$v[left])







hex_to_hsv <- function(hex_code) {
  
  if (length(hex_code) == 1) {
    rgb_color <- hex2RGB(hex_code)
    hsv_color <- as(rgb_color, "HSV")
    return(hsv_color@coords)
  } else {
    df <- data.frame(h = numeric(), s = numeric(), v = numeric())
    
    for (col in hex_code) {
      rgb_color <- hex2RGB(col)
      hsv_color <- as(rgb_color, "HSV")
      
      df1 <- data.frame("h" = hsv_color@coords[1], "s" = hsv_color@coords[2], "v" = hsv_color@coords[3])
      df <- rbind(df, df1)
    }
    
    return(df)
  }
}


cols = c("#ECCFCC","#ECCFCC","#ECCFCC","#ECCFCC","#ECCFCC")

length(cols)

df = hex_to_hsv(cols)



convert_rgb_to_hsv_and_back <- function(colors_df) {
  
  
  
  hex_codes <- apply(colors_df, 1, function(row) {
    # Extrahiere RGB-Werte
    r <- row["r"]
    g <- row["g"]
    b <- row["b"]
    
    # Konvertiere RGB-Werte zu einem Hex-Farbwert
    hex_color <- rgb(r/255, g/255, b/255, maxColorValue = 1)
    
    # Konvertiere Hex zu RGB (Farbraum)
    rgb_color <- hex2RGB(hex_color)
    
    # Konvertiere RGB zu HSV (Farbraum)
    hsv_color <- as(rgb_color, "HSV")

    
    return(hsv_color)
  })
  
  return(hsv_color)
}

colors <- data.frame("r" = c(236, 255, 0, 20, 15), 
                     "g" = c(207, 255, 183, 132, 69), 
                     "b" = c(204, 255, 255, 255, 129))


convert_rgb_to_hsv_and_back(colors)







hex_color <- hsv(h, s, v)

hsv(5.625 ,0.1355932, 0.9254902)

"#6CC100"
"#E60000"



hex_color <- "#ECCFCC"
  
  
# Konvertiere Hex zu HSV
hsv_color <- hex2RGB(hex_color) %>% as("HSV")

# Zeige das Ergebnis
hsv_color@coords


  
# Konvertiere Hex zu RGB
rgb_color <- col2rgb(hex_color) / 255

# Konvertiere RGB zu HSV
hsv_color <- rgb2hsv(rgb_color)

# Zeige das Ergebnis
hsv_color


rgb_converted <- as(hsv_color, "RGB")


library(colorspace)

# Originaler Hex-Farbwert
hex_color <- "#ECCFCC"
  
# Konvertiere Hex zu RGB (Farbraum)
rgb_color <- hex2RGB(hex_color)

# Konvertiere RGB zu HSV (Farbraum)
hsv_color <- as(rgb_color, "HSV")
str = toString(hsv_color)
hsv_color@coords[1]

typeof(hsv_color)
# Rückkonvertiere HSV zu RGB (Farbraum)
rgb_converted <- as(hsv_color, "RGB")

# Konvertiere RGB zurück zu Hex
hex_converted <- hex(rgb_converted)

# Zeige das ursprüngliche und das zurückkonvertierte Ergebnis
original_hex <- hex_color
converted_hex <- hex_converted

original_hex
converted_hex



"#ECCFCC"

"#F6E9E7"

h <- hsv_values[1] / 360
s <- hsv_values[2]
v <- hsv_values[3]
hsv(h,s,v)
"#8B8B8B"
