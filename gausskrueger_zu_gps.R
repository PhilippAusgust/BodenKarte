gk_to_decimal_gps <- function(rechtswert, hochwert) {
  rho <- 180 / pi
  e2 <- 0.0067192188
  c <- 6398786.849
  
  mKen <- floor(rechtswert / 1000000.0)
  
  rm <- rechtswert - mKen * 1000000.0 - 500000.0
  
  bl <- hochwert / 10000855.7646
  
  bll <- bl * bl
  
  bf_1 <- 325632.08677 * bl * ((((((0.00000562025 * bll - 0.0000436398) * bll + 0.00022976983) * bll - 0.00113566119) * bll + 0.00424914906) * bll - 0.00831729565) * bll + 1.0)
  
  bf <- bf_1 / 3600.0 / rho
  
  co <- cos(bf)
  
  g2 <- e2 * co * co
  gl_1 <- c / sqrt(1.0 + g2)
  
  t <- sin(bf) / cos(bf)
  
  fa <- rm / gl_1
  
  gb_1 <- bf - (fa**2 * t * (1.0 + g2) / 2.0) + (fa**4 * t * (5.0 + (3.0 * t**2) + (6.0 * g2) - (6.0 * g2 * t**2)) / 24.0)
  
  lat <- gb_1 * rho
  
  dl <- fa - (fa**3 * (1.0 + (2.0 * t**2) + g2) / 6.0) + (fa**5 * (1.0 + (28.0 * t**2) + (24.0 * t**4)) / 120.0)
  
  lon <- (dl * rho / co) + (mKen * 3.0)
  
  return (list("lat" = lat, "lon" = lon)) 
  
}