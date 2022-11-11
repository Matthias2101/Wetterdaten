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


nied_na <- filter(niederschlag_stat1975, is.na(RS) )

check_minmax_niederschlag_stat1975 <- summarise(niederschlag_stat1975, 
                                                niederschlag_min=min(RS, na.rm=TRUE),
                                                niederschlag_max = max(RS, na.rm = TRUE))
#sort_niederschlag_stat1975 <- arrange(niederschlag_stat1975, desc(RS))
niederschlag_stat1975 <- niederschlag_stat1975 %>% 
  mutate( MESS_JAHR = strtoi(str_sub(MESS_DATUM,1,4), base = 10),
          MESS_MONAT = strtoi(str_sub(MESS_DATUM,5,6), base = 10),
          MESS_TAG = strtoi(str_sub(MESS_DATUM,7,8), base = 10),
          NIEDERSCHLAG_MM = RS)

#durchschnittlichen Niederschlag pro Monat berechnen
niederschlag_monat_stat1975 <- niederschlag_stat1975 %>% 
  group_by(MESS_JAHR, MESS_MONAT) %>%
  summarise(NIEDERSCHLAG_MM_DURCH = mean(NIEDERSCHLAG_MM)) %>% 
  arrange( NIEDERSCHLAG_MM_DURCH )

niederschlag_jahr_stat1975 <- niederschlag_stat1975 %>% 
  group_by(MESS_JAHR) %>%
  summarise(NIEDERSCHLAG_MM_DURCH = mean(NIEDERSCHLAG_MM)) %>% 
  arrange( NIEDERSCHLAG_MM_DURCH )
