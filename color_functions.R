getColorFromValue <- function(value, colors, PRINT = FALSE) {
  interval_width <- 1 / (nrow(colors) - 1)
  k <- floor(value / interval_width)
  percentage <- (value - (k * interval_width)) / interval_width
  
  left <- k + 1 
  right <- left + 1
  
  r <- as.numeric(colors[left, "r"]) - ((as.numeric(colors[left, "r"]) - as.numeric(colors[right, "r"])) * percentage)
  g <- as.numeric(colors[left, "g"]) - ((as.numeric(colors[left, "g"]) - as.numeric(colors[right, "g"])) * percentage)
  b <- as.numeric(colors[left, "b"]) - ((as.numeric(colors[left, "b"]) - as.numeric(colors[right, "b"])) * percentage)
  
  return (list("r" = r, "g" = g, "b" = b))
}

getColorString <- function(color) {
  return (paste("rgb(", color["r"], ",", color["g"], ",", color["b"], ")", sep = ""))
}

getHexWith0 <- function(val) {
  if(val <= 15) {
    return (as.character(paste("0", as.character(as.hexmode(as.numeric(val))), sep="")))
  } else {
    return (as.character(as.hexmode(as.numeric(val))))
  }
}

getColorHexString <- function(color) {
  return (paste("#", getHexWith0(color["r"]), getHexWith0(color["g"]), getHexWith0(color["b"]), sep = ""))
}