library("readxl")
library("writexl")
source("gk_zu_gps.R")
raster <- read_excel("data/nFK_Rasterpunkte.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
raster['lat'] <- 0.0
raster['lon'] <- 0.0
for (i in 1:nrow(raster)){
  #print(gk_zu_gps(2577375.285, 5603974.916, FALSE))
  koordinate <- gk_zu_gps(as.numeric(raster[i,3]), as.numeric(raster[i,4]), FALSE)
  raster[i,5] <- as.numeric(koordinate['latitude'])
  raster[i,6] <- as.numeric(koordinate['longitude'])
  
}

write_xlsx(raster, "/Users/Phil/Desktop/Auswertung/rasterpunkte_mit_gps.xlsx")


# überführen von Gauß-Krüger in GPS Koordinaten 

