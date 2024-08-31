
gk_zu_gps <- function(right, height, use_wgs84) {
  res <- gauss_krueger_transformation(right, height)
  return (seven_parameter_helmert_transf(as.numeric(res["latitude"]), as.numeric(res["longitude"]), use_wgs84))
}

gauss_krueger_transformation <- function(right, height) {
  # Check for invalid Parameters
  if (!((right > 1000000) && (height > 1000000))) {
    print("No valid Gauss-Kruger-Code.")
    return;
  }
  # Variables to prepare the geovalues
  GKRight <- right
  GKHeight <- height
  e2 <- 0.0067192188
  c <- 6398786.849
  rho <- 180 / pi
  bII <- (GKHeight / 10000855.7646) * (GKHeight / 10000855.7646)
  bf <- 325632.08677 * (GKHeight / 10000855.7646) * ((((((0.00000562025 * bII + 0.00022976983) * bII - 0.00113566119) * bII + 0.00424914906) * bII - 0.00831729565) * bII + 1))
  
  bf <- bf / (3600 * rho)
  co <- cos(bf)
  g2 <- e2 * (co * co)
  g1 <- c / sqrt(1 + g2)
  t <- tan(bf)
  fa <- (GKRight - trunc(GKRight / 1000000) * 1000000 - 500000) / g1
  
  GeoDezRight <- ((bf - fa * fa * t * (1 + g2) / 2 + fa * fa * fa * fa * t * (5 + 3 * t * t + 6 * g2 - 6 * g2 * t * t) / 24) * rho)
  dl <- fa - fa * fa * fa * (1 + 2 * t * t + g2) / 6 + fa * fa * fa * fa * fa * (1 + 28 * t * t + 24 * t * t * t * t) / 120
  GeoDezHeight <- dl * rho / co + trunc(GKRight / 1000000) * 3
  
  result <- list("latitude" = GeoDezRight, "longitude" = GeoDezHeight)
  
  return (result)
}

seven_parameter_helmert_transf <- function(right, height, use_wgs84) {
  earthRadius <- 6378137  # Earth is a sphere with this radius
  aBessel <- 6377397.155
  eeBessel <- 0.0066743722296294277832
  ScaleFactor <- 0.00000982
  RotXRad <- -7.16069806998785E-06
  RotYRad <- 3.56822869296619E-07
  RotZRad <- 7.06858347057704E-06
  ShiftXMeters <- 591.28
  ShiftYMeters <- 81.35
  ShiftZMeters <- 396.39
  LatitudeIt <- 99999999
  
  ee <- 0.00669438002290
  
  if (use_wgs84) {
    ee <- 0.0066943799
  }
  
  
  GeoDezRight <- (right / 180) * pi
  GeoDezHeight <- (height / 180) * pi
  
  n <- eeBessel * sin(GeoDezRight) * sin(GeoDezRight)
  n <- 1 - n
  n <- sqrt(n)
  n <- aBessel / n
  
  CartesianXMeters <- n * cos(GeoDezRight) * cos(GeoDezHeight)
  CartesianYMeters <- n * cos(GeoDezRight) * sin(GeoDezHeight)
  CartesianZMeters <- n * (1 - eeBessel) * sin(GeoDezRight)
  
  CartOutputXMeters <- (1 + ScaleFactor) * CartesianXMeters + RotZRad * CartesianYMeters - RotYRad * CartesianZMeters + ShiftXMeters
  CartOutputYMeters <- -1 * RotZRad * CartesianXMeters + (1 + ScaleFactor) * CartesianYMeters + RotXRad * CartesianZMeters + ShiftYMeters
  CartOutputZMeters <- RotYRad * CartesianXMeters - RotXRad * CartesianYMeters + (1 + ScaleFactor) * CartesianZMeters + ShiftZMeters
  
  GeoDezHeight <- atan(CartOutputYMeters / CartOutputXMeters)
  
  Latitude <- (CartOutputXMeters * CartOutputXMeters) + (CartOutputYMeters * CartOutputYMeters)
  Latitude <- sqrt(Latitude)
  Latitude <- CartOutputZMeters / Latitude
  Latitude <- atan(Latitude)
  
  not_accurate_enough <- TRUE
  
  while (not_accurate_enough) {
    LatitudeIt <- Latitude
    
    n <- 1 - ee * sin(Latitude) * sin(Latitude)
    n <- sqrt(n)
    n <- earthRadius / n
    
    Latitude <- CartOutputXMeters * CartOutputXMeters + CartOutputYMeters * CartOutputYMeters
    Latitude <- sqrt(Latitude)
    Latitude <- (CartOutputZMeters + ee * n * sin(LatitudeIt)) / Latitude
    Latitude <- atan(Latitude)
    
    not_accurate_enough <- abs(Latitude - LatitudeIt) >= 0.000000000000001
  }
  
  GeoDezRight <- (Latitude / pi) * 180
  GeoDezHeight <- (GeoDezHeight) / pi * 180
  
  
  result <- list("latitude" = GeoDezRight, "longitude" = GeoDezHeight)
  
  return (result)
  
}
