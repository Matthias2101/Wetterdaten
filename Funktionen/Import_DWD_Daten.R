#enthält Funktionen, wie z.B. Import und Vorbereitung von Tabellen des DWD
#17.09.2022 Matthias Müller


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


#Import der Liste der Meßstationen mit stündlicher Temperaturmessung
Import_Stationenliste_Temperatur <- function(filename) {
  
  print("startet Import_Stationenliste_Niederschlag")
  print(getwd())
  print(filename)
  
  statlist_file <- filename
  statlist_sep <- " "
  col_names <- c("Stations_id", "von_datum", "bis_datum", "Stationshoehe", "geoBreite", "geoLaenge", "Stationsname", "Bundesland")
  
  
  stationenliste_lines <- stri_read_lines(filename, encoding = "ISO-8859-15")
  stationenliste_lines <- stationenliste_lines[-(1:2)]
  stationenliste <- tibble(Stations_id = substr(stationenliste_lines,  1,  6),
                               von_datum   = substr(stationenliste_lines,  7, 15), 
                               bis_datum   = substr(stationenliste_lines, 16, 24),
                               Stationshoehe = substr(stationenliste_lines, 35, 39),
                               geoBreite   = substr(stationenliste_lines, 43, 51),
                               geoLaenge   = substr(stationenliste_lines, 53, 61), 
                               Stationsname = substr(stationenliste_lines, 62, 102),
                               Bundesland   = substr(stationenliste_lines, 103, 200))
}

#Import der stündlichen Temperaturtabelle
#Umwandlung der Einträge in Jahr/Monat/Tag/Stunde (zusätzliche Spalten)
Import_Temperaturen_Stuendlich <- function(filename) {
  temperaturen <- read_delim(filename, 
                              delim =";",
                              trim_ws = TRUE,
                              na = c("","NA","-999"),
                              col_types = list(
                                STATIONS_ID = col_character(),
                                MESS_DATUM = col_character(),
                                QN_9       = col_character(),
                                TT_TU      = col_double(),
                                RF_TU      = col_number(),
                                eor        = col_character()))
  
  temperaturen <- temperaturen %>%
    mutate( MESS_JAHR = strtoi(str_sub(MESS_DATUM,1,4), base = 10),
            MESS_MONAT = strtoi(str_sub(MESS_DATUM,5,6), base = 10),
            MESS_TAG = strtoi(str_sub(MESS_DATUM,7,8), base = 10),
            MESS_STUNDE = strtoi(str_sub(MESS_DATUM,9,10), base = 10) )
}
  
  

#Liest tägliche Niederschlagswerte ein, Jahr/Monat/Tag des Messdatums in einzelne Spalten
Import_Niederschlag_Taeglich <- function(filename) {
  niederschlag_stat1975 <- read_delim(filename, 
                                      delim =";",
                                      trim_ws = TRUE,
                                      na = c("","NA","-999"),
                                      col_types = list(
                                        STATIONS_ID = col_character(),
                                        MESS_DATUM = col_character(),
                                        QN_6       = col_character(),
                                        RS         = col_double(),
                                        RSF        = col_character(),
                                        SH_TAG     = col_double(),
                                        NSH_TAG    = col_double(),
                                        eor        = col_character()))
  niederschlag_stat1975 <- niederschlag_stat1975 %>% 
    mutate( MESS_JAHR = strtoi(str_sub(MESS_DATUM,1,4), base = 10),
            MESS_MONAT = strtoi(str_sub(MESS_DATUM,5,6), base = 10),
            MESS_TAG = strtoi(str_sub(MESS_DATUM,7,8), base = 10),
            NIEDERSCHLAG_MM = RS) 
}





