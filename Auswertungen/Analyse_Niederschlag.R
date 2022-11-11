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

setwd("/home/matthias/Programme/Analyse_Wetter/R_Projekt_Wetter/")
source("Wetter_Functions.R",local=TRUE)


stationenliste <- Import_Stationenliste_Temperatur("../RR_Stundenwerte_Beschreibung_Stationen.txt")

stationenliste_hamburg <- filter(stationenliste, str_detect(Bundesland, "Hamburg"))
station_quickborn <- filter(stationenliste, str_detect(Stationsname, "Quickborn"))


temp_stat1975 <- Import_Temperaturen_Stuendlich("../stundenwerte_TU_01975_19490101_20211231_hist/produkt_tu_stunde_19490101_20211231_01975.txt")
temp_low_na <- filter(temp_stat1975, TT_TU < -100 | is.na(TT_TU))


niederschlag_stat1975 <- Import_Niederschlag_Taeglich("../tageswerte_RR_01975_19360101_20211231_hist/produkt_nieder_tag_19360101_20211231_01975.txt")
niederschlag_stat1975akt <- Import_Niederschlag_Taeglich("../tageswerte_RR_01975_akt/produkt_nieder_tag_20210316_20220916_01975.txt")

#hist tabelle geht bis 31.12.2005, in unterer Tabelle alles rauswerfen, was früher ist, später automatisieren

niederschlag_stat1975akt  <- niederschlag_stat1975akt %>%
  filter(MESS_JAHR > 2021)


niederschlag_stat1975_inklakt <- rbind(niederschlag_stat1975, niederschlag_stat1975akt)

nied_na <- filter(niederschlag_stat1975_inklakt, is.na(RS) )
check_minmax_niederschlag<- summarise(niederschlag_stat1975_inklakt , 
                                                niederschlag_min=min(RS, na.rm=TRUE),
                                                niederschlag_max = max(RS, na.rm = TRUE))
#sort_niederschlag_stat1975 <- arrange(niederschlag_stat1975, desc(RS))


#durchschnittlichen Niederschlag pro Monat berechnen
niederschlag_monat_stat1975 <- niederschlag_stat1975_inklakt  %>% 
  group_by(MESS_JAHR, MESS_MONAT) %>%
  summarise(NIEDERSCHLAG_MM_DURCH = mean(NIEDERSCHLAG_MM),
            NIEDERSCHLAG_MM_SUM   = sum(NIEDERSCHLAG_MM),
            Tage_mit_Niederschlag = sum(NIEDERSCHLAG_MM > 0)) %>% 
  arrange( NIEDERSCHLAG_MM_DURCH )

niederschlag_jahr_stat1975 <- niederschlag_stat1975_inklakt  %>% 
  group_by(MESS_JAHR) %>%
  summarise(NIEDERSCHLAG_MM_DURCH = mean(NIEDERSCHLAG_MM),
            NIEDERSCHLAG_MM_SUM   = sum(NIEDERSCHLAG_MM),
            Tage_mit_Niederschlag = sum(NIEDERSCHLAG_MM > 0)) %>% 
  arrange( NIEDERSCHLAG_MM_DURCH )



niederschlag_tst_stat1975 <- niederschlag_monat_stat1975  %>% 
  filter(MESS_JAHR == 2022)