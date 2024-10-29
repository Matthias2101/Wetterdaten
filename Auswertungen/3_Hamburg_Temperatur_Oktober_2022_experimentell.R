
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
library(hrbrthemes)
#library(hrbrthemes)

#Was wird gemacht? Auswertung der Temperaturdaten des Monats Oktober der Wetterstation Fuhlsbüttel, 
#Versuch zu entscheiden, welcher dieser Monate der wärmste ist
#Idee: Monat in drei Drittel aufteilen, Durchschnittstemperatur pro Drittel ausrechnen
#Ridge Diagramme (mit drei Dritteln) zeichnen, sehen, wo die Grenzen sind (niedrige / mittlere/hohe Temperaturen)




#vor 11.11.2022 Importfunktionen
#11.11.2022 Ridge Diagramme mit 4 Perzentilen eingefügt
#26.11.2022 Ridge Line Diagramme: variable Anzahl von Perzentilen, hier auf 3 gestellt, alle Jahreszahlen in Diagramm eingetragen
#06.02.2024 Matthias Müller Diagramme durch Funktionsaufruf, alle anderen Ridge Line Diagramme auskommentiert

# Pfade auf Daten setzen

setwd("/home/matthias/Programme/Analyse_Wetter_Klima/R_Projekt/")

data_path <- '../Daten/Download20221105/gerclimatehourlyairtemp'
data_stationenliste <- 'TU_Stundenwerte_Beschreibung_Stationen.txt'

dir_temp_hist <- 'stundenwerte_TU_01975_19490101_20211231_hist'
file_temp_hist <- 'produkt_tu_stunde_19490101_20211231_01975.txt'

dir_temp_akt <- 'stundenwerte_TU_01975_akt'
file_temp_akt <- 'produkt_tu_stunde_20210503_20221103_01975.txt'

results_path <- '../Ergebnisse/3_Hamburg_Temperatur_Oktober_2022/'

#Pfad auf Ordner mit Funktionen setzen
dir_function <- 'Funktionen'
file_functions <- 'Import_DWD_Daten.R'
df_functions <- str_c(dir_function,file_functions,sep='/')
file_functions2 <- 'Ridge_Diagramme.R'

file_functions3 <- 'Funktionen_Wetter.R'

source(df_functions, local = TRUE)

df_functions2 <- str_c(dir_function,file_functions2,sep='/')
source(df_functions2, local = TRUE)

df_functions3 <- str_c(dir_function,file_functions3,sep='/')
source(df_functions3, local = TRUE)

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

#Ridgeline Plots
#Anleitung unter https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
#Gallery unter https://r-graph-gallery.com/294-basic-ridgeline-plot.html#color

#Skalen vorbereiten
#erstes und letztes Jahr in Zeitreihe für Skala bestimmen
# min_jahr_t <- temp_HH_Oktober_taeglich %>%
#   ungroup() %>%
#   summarise(temp_HH_Oktober_taeglich,min_jahr = min(MESS_JAHR)) %>%
#   select(min_jahr) 
# min_jahr_1 <- unlist(min_jahr_t[1,1])
# 
# max_jahr_t <- temp_HH_Oktober_taeglich %>%
#   ungroup() %>%
#   summarise(temp_HH_Oktober_taeglich,max_jahr = max(MESS_JAHR)) %>%
#   select(max_jahr) 
# max_jahr_1 <- unlist(max_jahr_t[1,1])

#Ridge line plot mittlere Tagestemperaturen
# Plot_mittleretagestemp <- ggplot(temp_HH_Oktober_taeglich,  aes(x=mean_temp, y=MESS_JAHR, group=MESS_JAHR, fill=factor(stat(quantile))))+
#   scale_y_continuous(breaks = seq(min_jahr_1, max_jahr_1, by=1) )+
#   stat_density_ridges(
#     geom = "density_ridges_gradient", calc_ecdf = TRUE,
#     quantiles = ridgenoperz, quantile_lines = TRUE, 
#   ) +
#   scale_fill_viridis_d(name = "Quartiles")

#Diagramme in extra Funktion ausgelagert:
Plot_mittleretagestempf <- ridgelinediagramm(daten = temp_HH_Oktober_taeglich,datenspalte = 'mean_temp',gruppierungsspalte = 'MESS_JAHR', 
                                             zeitspalte = 'MESS_TAG', titel = "Temperaturen Oktober in Hamburg Flughafen",beschriftung_y = "Jahre", beschriftung_x ="Grad Celsius", ridgenoperz = 5) 

#Ridge line plot max.Tagestemperaturen  
# Plot_maxtagestemp <-ggplot(temp_HH_Oktober_taeglich,  aes(x=max_temp, y=MESS_JAHR, group=MESS_JAHR, fill=factor(stat(quantile))))+
#   scale_y_continuous(breaks = seq(min_jahr_1, max_jahr_1, by=1) )+
#   stat_density_ridges(
#     geom = "density_ridges_gradient", calc_ecdf = TRUE,
#     quantiles = ridgenoperz, quantile_lines = TRUE
#   ) +
#   scale_fill_viridis_d(name = "Quartiles")

#Ridge line plot min.Tagestemperaturen 
# Plot_maxtagestemp <- ggplot(temp_HH_Oktober_taeglich,  aes(x=min_temp, y=MESS_JAHR, group=MESS_JAHR, fill=factor(stat(quantile))))+
#   scale_y_continuous(breaks = seq(min_jahr_1, max_jahr_1, by=1) )+
#   stat_density_ridges(
#     geom = "density_ridges_gradient", calc_ecdf = TRUE,
#     quantiles = ridgenoperz, quantile_lines = TRUE
#   ) +
#   scale_fill_viridis_d(name = "Quartiles")






#ab hier Versuch, min, max, durchschnitt pro bucket auszurechnen, eine Beobachtung kann anteilig zu mehreren Buckets gehören
#ggf. Funktion f_durchschnitt schreiben


#stündliche Werte zu täglichen Werten zusammenfassen
temp_HH_taeglich <- temp_HH %>% 
  group_by(MESS_JAHR,MESS_MONAT,MESS_TAG) %>%
  summarise(min_temp = min(TT_TU,na.rm = TRUE),
            max_temp = max(TT_TU,na.rm = TRUE),
            mean_temp = mean(TT_TU,na.rm = TRUE) )

#pro Beobachtungsmonat nach mittlerer Tagestemperatur ordnen
analysetabelle <- temp_HH_taeglich %>%
  select(MESS_JAHR, MESS_MONAT, MESS_TAG,mean_temp) %>%
  group_by(MESS_JAHR,MESS_MONAT) %>%
  arrange(MESS_JAHR,MESS_MONAT,mean_temp)
#in Tabelle schreiben, wievielter Wert das ist, niedrigster Wert erhält 1, pro Monat, da oben group_by
analysetabelle <- analysetabelle %>%
  mutate(ntertag =row_number())



#an hier in eine Funktion f_gewichte
anzahl_toepfe <- 3

#jede Anzahl von Tagen in einem Monat heraussuchen
#das können 28, 29, 30, 31 sein (verschiedene Längen der Kalendermonate) oder eine niedrigere Anzahl, wenn einzelne Tage fehlen
#Anzahl der Töpfe höchstens so groß wie Anzahl Beobachtungen in einem Monat, 06.02.2024 MM


anzahl_datenpkt_alle <- analysetabelle  %>% 
  summarise(anzahl_pro_monat = n()) %>% 
  mutate(anzahl_toepfe_tab = anzahl_toepfe) 


anzahl_datenpkt <- anzahl_datenpkt_alle %>% 
  ungroup() %>%
    select(anzahl_pro_monat,anzahl_toepfe_tab) %>%
    arrange(anzahl_pro_monat,anzahl_toepfe_tab)  %>%
    group_by(anzahl_pro_monat,anzahl_toepfe_tab) %>%
    summarise(f_anzahl = first(anzahl_pro_monat), f_anzahl_toepfe = first(anzahl_toepfe_tab))


#Tabelle, die verschiedene Anzahlen von Tagen in einem Monat enthält sowie diese Tage aufzählt
#wird für Berechnung der Gewichte benötigt, für jede Zeile der Tabelle wird ein Vektor der Gewichtungen erzeugt
anzahl_datenpkte_exp <- inner_join(analysetabelle, anzahl_datenpkt_alle, by =c("MESS_JAHR"="MESS_JAHR","MESS_MONAT"="MESS_MONAT"))  %>%
   ungroup() %>%
   select(ntertag,anzahl_pro_monat,anzahl_toepfe_tab) %>%
   distinct(ntertag,anzahl_pro_monat,anzahl_toepfe_tab) %>%
  rename(zeitpunkt = ntertag, anzahl_zeitpkte = anzahl_pro_monat, anzahl_toepfe= anzahl_toepfe_tab )

gewichte_anzahl <- gewichte_pro_anzahl(anzahl_datenpkte_exp)

#analysetabelle  mit anzahl_datenpkte_alle  joinen (in jeder Zeile Anzahl der Daten pro Monat enthalten)
#Variablen MESS_JAHR, MESS_MONAT
analysetab_1 <- inner_join(analysetabelle, anzahl_datenpkt_alle, by =c("MESS_JAHR"="MESS_JAHR","MESS_MONAT"="MESS_MONAT")) 

#daran gewichte_anzahl joinen ( ) 
analysetab_2 <- inner_join(analysetab_1, gewichte_anzahl, by =c("anzahl_pro_monat"="anzahl_zeitpkte","ntertag"="zeitpunkt"))
 
analysetab_std <- analysetab_2 %>%
  rename(temperatur = mean_temp, no_zeitpunkt = ntertag ) %>%
  select(-anzahl_toepfe_tab)

##ergebnis ist Matrix, Zeilen: Töpfe, Spalten: entsprechen Zeilen von analysetab_std
ergebnis <- apply(analysetab_std, 1, function(x) {
 # print(x)
  temp_gewichtet <- rep(0,x['anzahl_toepfe'])
  for(k in 1:x['anzahl_toepfe'] ) { 
    temp_gewichtet[k] <- x['temperatur'] * x[k+7]}
  return(temp_gewichtet)
} )


ergebnis_t <- as.data.frame(t(ergebnis))
for (i in 1:anzahl_toepfe) {
  colnames(ergebnis_t)[i] <- paste0("Gewichtete_Temperatur_",as.character(i))
} 

analysetab_ergebnis <- cbind(analysetab_std,ergebnis_t)

# summarize_if https://dplyr.tidyverse.org/reference/summarise_all.html
#einfachste Lösung: über alle Spalten bis auf Gruppierung summieren, nur benötigte Spalten weiter verwenden
#später genauer untersuchen
ergebnis_pro_monat_minmax <- analysetab_ergebnis %>%
  group_by(MESS_JAHR,MESS_MONAT) %>%
  summarise(max_temperatur = max(temperatur), 
            min_temperatur = min(temperatur))  #%>%
      #      summarize_all(sum) ) %>%
           # across(vars('Gewichtete Temperatur 1': str_c('Gewichtete Temperatur ',as_character(anzahl_toepfe)))), sum) %>%
# summarize_all(sum) %>%
#  select(-(MESS_TAG:anzahl_toepfe))
ergebnis_pro_monat <- analysetab_ergebnis %>%
  group_by(MESS_JAHR,MESS_MONAT) %>%
  summarize_all(mean)  %>%
  select(-(MESS_TAG:anzahl_toepfe))

# ergebnis_pro_monat <- inner_join(ergebnis_pro_monat,ergebnis_pro_monat_minmax, 
#                                  by = c("MESS_JAHR", "MESS_MONAT"))


# ... und jetzt endlich Durchschnittstemperatur pro Topf und Monat ausrechnen ...
durchschnittstemp <- apply(ergebnis_pro_monat, 1, function(x){
  temp_durchschn_gew <- rep(0,anzahl_toepfe ) #closure, anzahl_toepfe in aufrufender Umgebung vorhanden
  for(k in 1:anzahl_toepfe) {
    temp_durchschn_gew[k] <- x[2+anzahl_toepfe+k] / x[2+k]
  }
  return(temp_durchschn_gew)
} )

durchschnittstemp_t <- as.data.frame(t(durchschnittstemp))
for (i in 1:anzahl_toepfe) {
  #paste fügt Leerzeichen ein, paste0 nicht
  colnames(durchschnittstemp_t)[i] <- paste0("Durchschnittstemperatur_",as.character(i))
} 


durchschnittstemperatur_b <- cbind(ergebnis_pro_monat,durchschnittstemp_t )

#nicht benötigte Spalten rauswerfen
durchschnittstemperatur_b <- select(durchschnittstemperatur_b, -(contains("Topf"))) 
durchschnittstemperatur_b <- select(durchschnittstemperatur_b, -(contains("Gewichtete_Temperatur"))) 
#durchschnittstemperatur_b <- select(durchschnittstemperatur, -(contains("Gewichtete_Temperatur"))) 

#jetzt noch min max

durchschnittstemperatur_b <- inner_join(durchschnittstemperatur_b ,ergebnis_pro_monat_minmax, 
                                      by = c("MESS_JAHR", "MESS_MONAT"))
#max. Temperatur in Tabelle
#Tabelle so transponieren, dass alle Spaltenüberschriften außer mess_jahr und mess_monat Bezeichnungen werden (Einträge in extra Spalte)
durchschnittstemperatur <- pivot_longer(durchschnittstemperatur_b, cols= -c("MESS_JAHR", "MESS_MONAT"), 
                                         names_to = "Verlauf", values_to = "Temperatur")

#Diagramm zeichnen
#Liniendiagramm


#Tabelle filtern auf Oktober

daten_diagramm_okt <- filter(durchschnittstemperatur, MESS_MONAT == 10)

scala_x <- unique(daten_diagramm_okt$MESS_JAHR )
# https://r-graph-gallery.com/line-chart-several-groups-ggplot2.html
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#hexadecimal-color-code-chart
# https://forum.posit.co/t/how-to-explicitly-map-labels-and-colors-to-variable-values-in-ggplot/112167
Plot_diagramm_okt <- ggplot(daten_diagramm_okt, aes(x=MESS_JAHR, y=Temperatur, group=Verlauf, color=Verlauf)) +
  geom_line() +
  scale_color_manual(values = c('min_temperatur' =  "#0066FF",
                                'Durchschnittstemperatur_1' = "#0000FF",
                                'Durchschnittstemperatur_2' = "#006633",
                                'Durchschnittstemperatur_3' = "#FF9900",
                                'max_temperatur' = "#FF6633")
                     )+
  scale_x_continuous(breaks= scala_x) +
  ggtitle("Temperaturverlauf im Oktober") +
  #Beschriftung x Achse drehen
  theme(axis.text.x=element_text(angle = 90))+ 
  ylab("Temperatur")





#https://ggplot2.tidyverse.org/reference/ggsave.html
#hier Seitenverhältnis festlegen, dieses Diagramm muss breit sein
ggsave(paste0(results_path,"Plot_diagramm_okt.jpeg"),Plot_diagramm_okt, bg= "white", dpi=300, width = 35, height = 10, units = "cm")

#dieses Diagramm muss hoch sein
ggsave(paste0(results_path,"Plot_mittleretagestempf.jpeg"),Plot_mittleretagestempf, bg= "white", dpi=300, width = 30, height = 40, units = "cm")
#exportieren

#ab hier experiment

#wozu wird das gebraucht?
# anzahl_tage <- anzahl_datenpkt %>%
#   group_by() %>%
#   summarise(anzahl_gesamt = sum(anzahl_pro_monat)) %>%
#   unlist()
# 
# 
# 
# gewichte <- data.frame(matrix(0, 
#                               nrow=anzahl_tage,
#                               ncol=anzahl_toepfe+2))
# 
# #in anzahl_datenpunkte ist jede Anzahl von Tagen aufgelistet, die in den Daten vorkommt. Dazu wird für jede Kombination und jeden Tag Gewichtung pro Tag ausgerechnet.
# #zu  jedem Tag und Topf Gewicht ermitteln, Tage sind Zeilen, Töpfe sind Spalten
# #Ergebnis: Tabelle gewichte
# #funktioniert auch, wenn es weniger Tage als Töpfe gibt
# zeile_gewichte <- 0
# for(j in 1:nrow(anzahl_datenpkt)) {
#   adp <- unlist(anzahl_datenpkt[j,1])
#   anzahl_toepfe <- unlist(anzahl_datenpkt[j,2])
#   for(no_tag in 1:adp) {
#     zeile_gewichte <- zeile_gewichte + 1
#     gewichte[zeile_gewichte,1] <- adp
#     gewichte[zeile_gewichte,2] <- no_tag
#   
#     #über Anzahl Töpfe
#     for(k in 1:anzahl_toepfe) {
#    #   if(no_tag > anzahl_toepfe) {gewichte[zeile_gewichte,k+2] <- 0
#     #  } else {
#         abstand_oben <- (adp*k)/anzahl_toepfe - no_tag
#         abstand_unten <- no_tag - adp*(k-1) / anzahl_toepfe 
#       
#         if (abstand_unten >= 1 && abstand_oben >= 1) {gewichte[zeile_gewichte,k+2] <- 1
#         } else if(abs(abstand_oben) < 1 ) {gewichte[zeile_gewichte,k+2] <- 1- abs(abstand_oben)
#         } else if (abs(abstand_unten) < 1 ) {gewichte[zeile_gewichte,k+2] <- abs(abstand_unten)
#         } else {gewichte[zeile_gewichte,k+2] <- 0}
#    #   }
#     }
#   }
# }
# #Spaltenüberschriften in gewichte Tabelle setzen
# colnames(gewichte)[1] <- "AnzahlTage"
# colnames(gewichte)[2] <-"Tag"
# for (i in 1:anzahl_toepfe) {
#   colnames(gewichte)[i+2] <- paste("Topf_",as.character(i))
# }
# 

#Gewichte neben Datentabelle schreiben
#daten_toepfe <- inner_join(anzahl_datenpkt_alle, gewichte, by=c("anzahl_pro_monat"="AnzahlTage", )) 

#bis hier in eine Funktion f_gewichte



#ab hier Experiment: gruppierte Summe ausrechnen
#Experiment mit densities
# tempmax_densities <- temp_HH_Oktober_taeglich %>%
#   group_by(MESS_JAHR) %>%
#   group_modify(~ ggplot2:::compute_density(.x$max_temp, NULL)) %>%
#   rename(temp_HH_Oktober_taeglich.max_temp = x)
# 
# 
# 
# 
# tempmax_densities_1949 <- filter(tempmax_densities, MESS_JAHR == 1949)
# 
# 
# tempmax_densities_1949 <- tempmax_densities_1949 %>% mutate(cumdensity = cumsum(density))
# 
# 
# summe <- tempmax_densities_1949%>%
#   group_by(.) %>%
#   summarise(sum_count = sum(count),
#             sum_ndensity = sum(ndensity),
#             sum_density = sum(density),
#             min_temp = min(temp_HH_Oktober_taeglich.max_temp),
#             max_temp = max(temp_HH_Oktober_taeglich.max_temp)) %>%
#   mutate(temp_diff = max_temp - min_temp)
# 
#             
# 
# 
# iris_densities <- iris %>%
#   group_by(Species) %>%
#   group_modify(~ ggplot2:::compute_density(.x$Sepal.Length, NULL)) %>%
#   rename(Sepal.Length = x)

# im Beispiel („Anleitung“) wird density aus der Tabelle iris_densities geplottet
# für bedingte Durchschnitte einfach Durchschnitt ausrechnen, bis Dichte summiert gleich Dichte_gesamt / n ist, sollte gehen, da Dichten sehr klein sind bzw. viele Werte vorhanden sind
# Integral ausrechnen

#vergleichen mit Durchschnitt, wobei ggf. bei 31 Tagen Temperaturen gewichtet in zwei Kathegorien fallen

# iris_densities
