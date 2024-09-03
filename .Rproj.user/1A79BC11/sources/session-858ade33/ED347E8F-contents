library(readxl)
library("writexl")

auswertung <- read_excel("data/eingabe_r.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
nfk_vol_percent <- read_excel("data/nFK\ in\ Vol\ B.K.A.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)

auswertung['nfk_volumen_prozent'] <- 0
auswertung['nutzbare_feldkapazität'] <- 0

# gehe durch alle zeilen der tablle 
for (i in 1:nrow(auswertung)) {
  
  # bestimme zugehörigen zeilen und spalten index in der nfk_volumen_prozent - tabelle
  
  row_index <- 0
  col_index <- 0
  
  for (j in 1:length(rownames(nfk_vol_percent))) {
    if (auswertung[i,3] == nfk_vol_percent[j,1]) {
      row_index <- j
      break
    }
  }
  
  for (j in 1:length(colnames(nfk_vol_percent))) {
    if(auswertung[i,8] == colnames(nfk_vol_percent)[j]) {
      col_index <- j
      break
    }
  }
  
  # speichere zugehörigen nfk_volumen_prozent - wert in der neuen spalte nfk_volumen_prozent (spalte 8)
  
  auswertung[i,9] <- as.numeric(nfk_vol_percent[row_index, col_index])
  
  # berechne nutzbare feldkapazität
  #                     Horizontmächtigkeit                Steingehalt                          nFK[%]                    Humusgehalt       
  auswertung[i,10] <- as.numeric(auswertung[i,2]) * (1-as.numeric(auswertung[i,4])) * (as.numeric(auswertung[i,9]) + as.numeric(auswertung[i,6]))
}

rasterpunkte <- character(200)
nFK_Werte <- double(200)

nfk_rasterpunkte = data.frame(Rasterpunkte=rasterpunkte, nFK=nFK_Werte) 
colnames(nfk_rasterpunkte) = c("Rasterpunkt", "nFK")


aktueller_rasterpunkt <- toString(auswertung[1,1])
aktuelle_summe <- 0
aktuelle_zeile <- 1

nfk_rasterpunkte[aktuelle_zeile,1] <- aktueller_rasterpunkt

for (i in 1:nrow(auswertung)) {
  if (aktueller_rasterpunkt == toString(auswertung[i,1])) {
    aktuelle_summe <- aktuelle_summe + auswertung[i,10]
  } else {
    nfk_rasterpunkte[aktuelle_zeile, 2] <- aktuelle_summe
    aktueller_rasterpunkt <- toString(auswertung[i,1])
    aktuelle_summe <- auswertung[i,10]
    aktuelle_zeile <- aktuelle_zeile + 1
    nfk_rasterpunkte[aktuelle_zeile,1] <- aktueller_rasterpunkt
  }
  if(i == nrow(auswertung)) {
    nfk_rasterpunkte[aktuelle_zeile, 2] <- aktuelle_summe
  }
}

write_xlsx(auswertung, "data/ausgabe_r.xlsx")
write_xlsx(nfk_rasterpunkte, "data/nfk_rasterpunkte.xlsx")















