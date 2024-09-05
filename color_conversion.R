library(colorspace)


hexToHSV <- function(cols) {

  df <- data.frame(h = numeric(), s = numeric(), v = numeric())

  for (hex_color in cols) {
    rgb_color <- hex2RGB(hex_color)
    hsv_color <- as(rgb_color, "HSV")
    df1 <- data.frame("h" = hsv_color@coords[1], "s" = hsv_color@coords[2], "v" = hsv_color@coords[3])
    df <- rbind(df, df1)
  }
  
  return(df)
}



#df = hexToHSV(c("#9FD0EB","#a627a3"))
