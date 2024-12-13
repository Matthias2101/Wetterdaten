library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
#install.packages("tidyverse")
#install.packages("stringi")
#library(tidyverse)
library(stringi)



library(ggridges)
library(viridis)
#library(hrbrthemes)

#Was wird gemacht? Auswertung der Temperaturdaten des Monats Oktober der Wetterstation Fuhlsbüttel, 
#Versuch zu entscheiden, welcher dieser Monate der wärmste ist
#Idee: Monat in drei Drittel aufteilen, Durchschnittstemperatur pro Drittel ausrechnen
#Ridge Diagramme (mit drei Dritteln) zeichnen, sehen, wo die Grenzen sind (niedrige / mittlere/hohe Temperaturen)




#vor 11.11.2022 Importfunktionen
#11.11.2022 Ridge Diagramme mit 4 Perzentilen eingefügt
#26.11.2022 Ridge Line Diagramme: variable Anzahl von Perzentilen, hier auf 3 gestellt, alle Jahreszahlen in Diagramm eingetragen

# Pfade auf Daten setzen
data_path <- '../Daten/Download20221105/gerclimatehourlyairtemp'
data_stationenliste <- 'TU_Stundenwerte_Beschreibung_Stationen.txt'

dir_temp_hist <- 'stundenwerte_TU_01975_19490101_20211231_hist'
file_temp_hist <- 'produkt_tu_stunde_19490101_20211231_01975.txt'

dir_temp_akt <- 'stundenwerte_TU_01975_akt'
file_temp_akt <- 'produkt_tu_stunde_20210503_20221103_01975.txt'

results_path <- 'Ergebnisse/3_Hamburg_Temperatur_Oktober_2022/'

#Pfad auf Ordner mit Funktionen setzen
dir_function <- 'Funktionen'
file_functions <- 'Import_DWD_Daten.R'
df_functions <- str_c(dir_function,file_functions,sep='/')
file_functions2 <- 'Ridge_Diagramme.R'
source(df_functions, local = TRUE)


df_functions2 <- str_c(dir_function,file_functions2,sep='/')
source(df_functions2, local = TRUE)


#Einstellungen: Anzahl Perzentile in Ridgeline Plots
ridgenoperz <- 3

#Daten importieren
df_stationenliste <- str_c(data_path,data_stationenliste,sep = '/')
df_temp_hist      <- str_c(data_path,dir_temp_hist,file_temp_hist,sep = '/')
df_temp_akt      <- str_c(data_path,dir_temp_akt,file_temp_akt,sep = '/')




stationenliste <- Import_Stationenliste_Temperatur(df_stationenliste)
stationenliste_temp_hamburg <- filter(stationenliste, str_detect(Bundesland, "Hamburg"))

temp_akt_HH <- Import_Temperaturen_Stuendlich(df_temp_akt)
temp_hist_HH <- Import_Temperaturen_Stuendlich(df_temp_hist)

#alle Datensätze aus hist Tabelle übernehmen, aus akt Tabelle nur die Datensätze, die in hist Tabelle nicht enthalten sind
#in aktueller Temeraturliste nur die Datensätze behalten, die nicht in hist Tabelle enthalten sind
#R for Data Science 13. Relational Data 13.5 Filtering Joins
temp_akt_HH_notinhist <- anti_join(temp_akt_HH, temp_hist_HH, by=c("STATIONS_ID","MESS_DATUM"))
#Tabellen aneinanderhängen
temp_HH <- rbind(temp_hist_HH,temp_akt_HH_notinhist)

#Daten Oktober herausfiltern
temp_HH_Oktober <- filter(temp_HH, MESS_MONAT == 10)

temp_HH_Oktober_taeglich <- temp_HH_Oktober %>% 
  group_by(MESS_JAHR,MESS_MONAT,MESS_TAG) %>%
  summarise(min_temp = min(TT_TU,na.rm = TRUE),
            max_temp = max(TT_TU,na.rm = TRUE),
            mean_temp = mean(TT_TU,na.rm = TRUE) )

#temp_HH_10j <- filter(temp_HH_Oktober_taeglich, MESS_JAHR >= 2000)

#Ridgeline Plots
#Anleitung unter https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
#Gallery unter https://r-graph-gallery.com/294-basic-ridgeline-plot.html#color

#Skalen vorbereiten
#erstes und letztes Jahr in Zeitreihe für Skala bestimmen
min_jahr_t <- temp_HH_Oktober_taeglich %>%
  ungroup() %>%
  summarise(temp_HH_Oktober_taeglich,min_jahr = min(MESS_JAHR)) %>%
  select(min_jahr) 
min_jahr_1 <- unlist(min_jahr_t[1,1])

max_jahr_t <- temp_HH_Oktober_taeglich %>%
  ungroup() %>%
  summarise(temp_HH_Oktober_taeglich,max_jahr = max(MESS_JAHR)) %>%
  select(max_jahr) 
max_jahr_1 <- unlist(max_jahr_t[1,1])

#Ridge line plot mittlere Tagestemperaturen
Plot_mittleretagestemp <- ggplot(temp_HH_Oktober_taeglich,  aes(x=mean_temp, y=MESS_JAHR, group=MESS_JAHR, fill=factor(stat(quantile))))+
  scale_y_continuous(breaks = seq(min_jahr_1, max_jahr_1, by=1) )+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = ridgenoperz, quantile_lines = TRUE, 
  ) +
  scale_fill_viridis_d(name = "Quartiles")


Plot_mittleretagestempf <- ridgelinediagramm(temp_HH_Oktober_taeglich,'mean_temp','MESS_JAHR','MESS_TAG') 

#Ridge line plot max.Tagestemperaturen  
Plot_maxtagestemp <-ggplot(temp_HH_Oktober_taeglich,  aes(x=max_temp, y=MESS_JAHR, group=MESS_JAHR, fill=factor(stat(quantile))))+
  scale_y_continuous(breaks = seq(min_jahr_1, max_jahr_1, by=1) )+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = ridgenoperz, quantile_lines = TRUE
  ) +
  scale_fill_viridis_d(name = "Quartiles")

#Ridge line plot min.Tagestemperaturen 
Plot_maxtagestemp <- ggplot(temp_HH_Oktober_taeglich,  aes(x=min_temp, y=MESS_JAHR, group=MESS_JAHR, fill=factor(stat(quantile))))+
  scale_y_continuous(breaks = seq(min_jahr_1, max_jahr_1, by=1) )+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = ridgenoperz, quantile_lines = TRUE
  ) +
  scale_fill_viridis_d(name = "Quartiles")






#ab hier Versuch, min, max, durchschnitt pro bucket auszurechnen, eine Beobachtung kann anteilig zu mehreren Buckets gehören

temp_HH_taeglich <- temp_HH %>% 
  group_by(MESS_JAHR,MESS_MONAT,MESS_TAG) %>%
  summarise(min_temp = min(TT_TU,na.rm = TRUE),
            max_temp = max(TT_TU,na.rm = TRUE),
            mean_temp = mean(TT_TU,na.rm = TRUE) )

analysetabelle <- temp_HH_taeglich %>%
  select(MESS_JAHR, MESS_MONAT, MESS_TAG,mean_temp) %>%
  group_by(MESS_JAHR,MESS_MONAT) %>%
  arrange(MESS_JAHR,MESS_MONAT,mean_temp)

analysetabelle <- analysetabelle %>%
  mutate(ntertag =row_number())

anzahl_toepfe <- 3
#jede Anzahl von Tagen in einem Monat heraussuchen
anzahl_datenpkt <- analysetabelle  %>% 
  summarise(anzahl_pro_monat = n()) %>% 
  mutate(Anzahl_Toepfe_Tab = anzahl_toepfe ) %>%
  ungroup() %>%
  select(anzahl_pro_monat,Anzahl_Toepfe_Tab) %>%
  arrange(anzahl_pro_monat,Anzahl_Toepfe_Tab)  %>%
  group_by(anzahl_pro_monat,Anzahl_Toepfe_Tab) %>%
  summarise(f_anzahl = first(anzahl_pro_monat), f_anzahl_toepfe = first(Anzahl_Toepfe_Tab))

anzahl_tage <- anzahl_datenpkt %>%
  group_by() %>%
  summarise(anzahl_gesamt = sum(anzahl_pro_monat)) %>%
  unlist()



gewichte <- data.frame(matrix(0, 
                              nrow=anzahl_tage,
                              ncol=anzahl_toepfe+2))
zeile_gewichte <- 0
for(j in 1:nrow(anzahl_datenpkt)) {
  adp <- unlist(anzahl_datenpkt[j,1])
  anzahl_toepfe <- unlist(anzahl_datenpkt[j,2])
  for(no_tag in 1:adp) {
    zeile_gewichte <- zeile_gewichte + 1
    gewichte[zeile_gewichte,1] <- adp
    gewichte[zeile_gewichte,2] <- no_tag
  
    #über Anzahl Töpfe
    for(k in 1:anzahl_toepfe) {
      abstand_oben <- (adp*k)/anzahl_toepfe - no_tag
      abstand_unten <- no_tag - adp*(k-1) / anzahl_toepfe 
      
      if (abstand_unten >= 1 && abstand_oben >= 1) {gewichte[zeile_gewichte,k+2] <- 1
      } else if(abs(abstand_oben) < 1 ) {gewichte[zeile_gewichte,k+2] <- 1- abs(abstand_oben)
      } else if (abs(abstand_unten) < 1 ) {gewichte[zeile_gewichte,k+2] <- abs(abstand_unten)
      } else {gewichte[zeile_gewichte,k+2] <- 0}
    }
  }
}
#Spaltenüberschriften in gewichte Tabelle setzen
colnames(gewichte)[1] <- "AnzahlTage"
colnames(gewichte)[2] <-"Tag"
for (i in 1:anzahl_toepfe) {
  colnames(gewichte)[i+2] <- paste("Topf_",as.character(i))
}





#ab hier Experiment: gruppierte Summe ausrechnen
#Experiment mit densities
tempmax_densities <- temp_HH_Oktober_taeglich %>%
  group_by(MESS_JAHR) %>%
  group_modify(~ ggplot2:::compute_density(.x$max_temp, NULL)) %>%
  rename(temp_HH_Oktober_taeglich.max_temp = x)




tempmax_densities_1949 <- filter(tempmax_densities, MESS_JAHR == 1949)


tempmax_densities_1949 <- tempmax_densities_1949 %>% mutate(cumdensity = cumsum(density))


summe <- tempmax_densities_1949%>%
  group_by(.) %>%
  summarise(sum_count = sum(count),
            sum_ndensity = sum(ndensity),
            sum_density = sum(density),
            min_temp = min(temp_HH_Oktober_taeglich.max_temp),
            max_temp = max(temp_HH_Oktober_taeglich.max_temp)) %>%
  mutate(temp_diff = max_temp - min_temp)

            


iris_densities <- iris %>%
  group_by(Species) %>%
  group_modify(~ ggplot2:::compute_density(.x$Sepal.Length, NULL)) %>%
  rename(Sepal.Length = x)

# im Beispiel („Anleitung“) wird density aus der Tabelle iris_densities geplottet
# für bedingte Durchschnitte einfach Durchschnitt ausrechnen, bis Dichte summiert gleich Dichte_gesamt / n ist, sollte gehen, da Dichten sehr klein sind bzw. viele Werte vorhanden sind
# Integral ausrechnen

#vergleichen mit Durchschnitt, wobei ggf. bei 31 Tagen Temperaturen gewichtet in zwei Kathegorien fallen

# iris_densities
