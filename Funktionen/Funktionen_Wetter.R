
# Ziel: Temperaturen eines Zeitraums nach niedrig / mittel / hoch ordnen 
# pro Kathegorie z.B. Durchschnitt bzw. Min. / Max. Temperatur ausrechnen
# alles mit n Kathegorien aufschreiben
# da sich z.B. 31 Tage oder 28 Tage nicht in drei Töpfe aufteilen lassen, kann
# ein Zeitpunkt anteilig zu mehreren Töpfen gehören
#Vorgabe: Anzahl  der Zeitpunkte (z.B. in einem Monate) und Anzahl der Töpfe
#Ergebnis: data frame, enthält Zahlen 1 bis anzahl_zeitpunkte und zu jedem Zeitpunkt ein Vektor 
#der Töpfe, in dem steht, zu welchem Anteil der Zeitpunkt in welchen Topf gehört



#wird von gewichte_pro_anzahl aufgerufen (siehe unten, appply pro Zeie auf Tabelle, die in Funktion gewichte_pro_anzahl bearbeitet wird)
#gibt zu Anzahl der Zeitpunkte, Anzahl der Töpfe und einem Zeitpunkt Vektor der Gewichte (Verteilung auf Töpfe) 
#Ergebnis: Vektor Gewichtug der Beobachtung in  jedem Topf

gewicht_pro_zpkt <- function(x) {
  #erzeuge Vektor der Länge anzahl_toepfe gefüllt mit 0 en
  
  
 # zeitpunkt, anzahl_zeitpkte, anzahl_toepfe
  gewichte <- rep(0,x['anzahl_toepfe'])
  
  
  for(k in 1:x['anzahl_toepfe']) {
    #   if(no_tag > anzahl_toepfe) {gewichte[zeile_gewichte,k+2] <- 0
    #  } else {
    abstand_oben <- (x['anzahl_zeitpkte']*k)/x['anzahl_toepfe'] - x['zeitpunkt']
    abstand_unten <- x['zeitpunkt'] - x['anzahl_zeitpkte']*(k-1) / x['anzahl_toepfe'] 
    
    if (abstand_unten >= 1 && abstand_oben >= 1) {gewichte[k] <- 1
    } else if(abs(abstand_oben) < 1 ) {gewichte[k] <- 1- abs(abstand_oben)
    } else if (abs(abstand_unten) < 1 ) {gewichte[k] <- abs(abstand_unten)
    } else {gewichte[k] <- 0}
    #   }
  }
 return(gewichte)
}

#gewichte <- gewichte_pro_anzahl(anzahl_datenpkte_exp)
#x <- c(zeitpunkt = 1, anzahl_zeitpkte = 30,anzahl_toepfe =3 )
#t1 <- gewicht_pro_zpkt(x)

#gibt zu Anzahl der Zeitpunkte, Anzahl der Töpfe und einem Zeitpunkt Vektor der Gewichte (Verteilung auf Töpfe) 
#Ergebnis: Matrix, eine Zeile entspricht einem Topf, eine Spalte entspricht einer Zeile der Eingabetabelle
#Funktion geht davon aus, dass drei Spalten in Tabelle tab_anzahl_datenpkte_exp Bezeichungen o.ä. enthalten

#später mit Temparaturvektor multiplizieren: ergibt Summe der Temparaturen pro Topf
# Zeilensumme: Summe der Gewichtungen pro Topf 
# Division  ergibt Durchschnittstemperatur pro Topf
gewichte_pro_anzahl <- function(tab_anzahl_datenpkte_exp) {
  #gewichtungen <- apply(tab_anzahl_datenpkte_exp,1,gewichte_pro_anzahl(zpkt=ntertag, anzahl_zeitpkte = anzahl_pro_monat, anzahl_toepfe = anzahl_toepfe_tab))
  tab_anzahl_datenpkte_exp <- as.data.frame(tab_anzahl_datenpkte_exp)
   gewichtungen <- apply(tab_anzahl_datenpkte_exp,1,
                        function( x) {
                          gewicht_pro_zpkt( x)
                        }
   )
   txx_t <- t(gewichtungen )  #Matrix transponieren
   tab_anzahl_datenpkte_gew <- cbind(anzahl_datenpkte_exp,txx_t )
   anzahl_toepfe_1 <- tab_anzahl_datenpkte_gew[1,'anzahl_toepfe']
   for (i in 1:anzahl_toepfe_1) {
     colnames(tab_anzahl_datenpkte_gew)[i+3] <- paste("Topf_",as.character(i))
   }  
   return(tab_anzahl_datenpkte_gew)
}
#txx <- gewichte_pro_anzahl(anzahl_datenpkte_exp)
