library(geometry)

get_triangles_from_coordinates <- function(coordinates) {
  triangles <- delaunayn(coordinates)
  
  #triangles[,]
  #for (i in 1:nrow(triangles)) {
  #  p1 <- list("x" = as.numeric(coordinates[triangles[i,1], "lon"]), "y" = as.numeric(coordinates[triangles[i,1], "lat"]))
  #  p2 <- list("x" = as.numeric(coordinates[triangles[i,2], "lon"]), "y" = as.numeric(coordinates[triangles[i,2], "lat"]))
  #  p3 <- list("x" = as.numeric(coordinates[triangles[i,3], "lon"]), "y" = as.numeric(coordinates[triangles[i,3], "lat"]))
  #}
  
  
  return (triangles)
}

get_triangle_corners_and_bary_weights <- function(s, coordinates, triangles) {
  
  for (i in 1:nrow(triangles)) {
    p1 <- list("x" = as.numeric(coordinates[triangles[i,1], "lon"]), "y" = as.numeric(coordinates[triangles[i,1], "lat"]))
    p2 <- list("x" = as.numeric(coordinates[triangles[i,2], "lon"]), "y" = as.numeric(coordinates[triangles[i,2], "lat"]))
    p3 <- list("x" = as.numeric(coordinates[triangles[i,3], "lon"]), "y" = as.numeric(coordinates[triangles[i,3], "lat"]))
    
    barycentric_weights <- get_barycentric_weights(p1, p2, p3, s)
    
    if (as.numeric(barycentric_weights["m1"]) >= 0.0 && as.numeric(barycentric_weights["m2"]) >= 0.0 && as.numeric(barycentric_weights["m3"]) >= 0.0) {
      cornerIndices <- list("p1" = triangles[i,1], "p2" = triangles[i,2], "p3" = triangles[i,3])
      return (list("weights" = barycentric_weights, "cornerIndices" = cornerIndices)) 
    }
    
  }
  
  return (NULL)
  
}

get_barycentric_weights <- function(p1, p2, p3, s) {
  x1 <- as.numeric(p1["x"])
  y1 <- as.numeric(p1["y"])
  
  x2 <- as.numeric(p2["x"])
  y2 <- as.numeric(p2["y"])
  
  x3 <- as.numeric(p3["x"])
  y3 <- as.numeric(p3["y"])
  
  xs <- as.numeric(s["x"])
  ys <- as.numeric(s["y"])
  
  m1 <- ((x2 - xs) * (y3 - ys) - (x3 - xs) * (y2 - ys)) / ((x2 - x1) * (y3 - y2) - (y2 - y1) * (x3 - x2))
  m2 <- ((x3 - xs) * (y1 - ys) - (x1 - xs) * (y3 - ys)) / ((x2 - x1) * (y3 - y2) - (y2 - y1) * (x3 - x2))
  m3 <- ((x1 - xs) * (y2 - ys) - (x2 - xs) * (y1 - ys)) / ((x2 - x1) * (y3 - y2) - (y2 - y1) * (x3 - x2))
  
  return (list("m1" = m1, "m2" = m2, "m3" = m3))
} 
