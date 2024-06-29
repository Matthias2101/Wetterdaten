library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)





#Ziel: Temperaturen pro Zeitraum in Töpfe einteilen, Ridge Diagramme zeichnen, pro Topf und Zeitraum Durchschnitt berechnen


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

Plot_mittleretagestempf <- ridgelinediagramm(daten = temp_HH_Oktober_taeglich,datenspalte = 'mean_temp',gruppierungsspalte = 'MESS_JAHR', 
                                             zeitspalte = 'MESS_TAG', titel = "Temperaturen Oktober in Hamburg Flughafen",beschriftung_y = "Jahre", beschriftung_x ="Grad Celsius", ridgenoperz = 5) 

#ab hier Vorbereitung für gruppierte Durchschnittstemperaturen
#stündliche Werte zu täglichen Werten zusammenfassen
temp_HH_taeglich <- temp_HH %>% 
  group_by(MESS_JAHR,MESS_MONAT,MESS_TAG) %>%
  summarise(min_temp = min(TT_TU,na.rm = TRUE),
            max_temp = max(TT_TU,na.rm = TRUE),
            mean_temp = mean(TT_TU,na.rm = TRUE) )
anzahl_toepfe_param <- 3


#pro Beobachtungsmonat nach mittlerer Tagestemperatur ordnen und danach im gewählten Zeitraum (Monat) Nummerierung
analysetabelle_mean_temp <- temp_HH_taeglich %>%
  select(MESS_JAHR, MESS_MONAT, MESS_TAG,mean_temp) %>%
  rename(temperatur = mean_temp) %>%
   group_by(MESS_JAHR,MESS_MONAT) %>%
  arrange(MESS_JAHR,MESS_MONAT,temperatur)


analysetabelle_mean_temp <- analysetabelle_mean_temp %>%
  mutate(zeitpunkt =row_number()) #%>%
#   anzahl_datenpkt_mean_temp_2 <- analysetabelle_mean_temp  %>% 
#   summarise(anzahl_pro_monat = n()) 


anzahl_datenpkt <- anzahl_datenpkt_mean_temp_2 %>% 
  ungroup() %>%
  group_by(anzahl_pro_monat,anzahl_toepfe) %>%
  select(anzahl_pro_monat,anzahl_toepfe) %>%
  arrange(anzahl_pro_monat,anzahl_toepfe)  %>%
  distinct(anzahl_pro_monat,anzahl_toepfe)







