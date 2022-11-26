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
#library(ggplot2)
library(viridis)
library(hrbrthemes)




data_path <- '../Daten/Download20221105/gerclimatehourlyairtemp'
data_stationenliste <- 'TU_Stundenwerte_Beschreibung_Stationen.txt'

dir_temp_hist <- 'stundenwerte_TU_01975_19490101_20211231_hist'
file_temp_hist <- 'produkt_tu_stunde_19490101_20211231_01975.txt'

dir_temp_akt <- 'stundenwerte_TU_01975_akt'
file_temp_akt <- 'produkt_tu_stunde_20210503_20221103_01975.txt'

results_path <- '../Ergebnisse/3_Hamburg_Temperatur_Oktober_2022/'

dir_function <- 'Funktionen'
file_functions <- 'Import_DWD_Daten.R'


df_stationenliste <- str_c(data_path,data_stationenliste,sep = '/')
df_temp_hist      <- str_c(data_path,dir_temp_hist,file_temp_hist,sep = '/')
df_temp_akt      <- str_c(data_path,dir_temp_akt,file_temp_akt,sep = '/')

df_functions <- str_c(dir_function,file_functions,sep='/')


source(df_functions,local=TRUE)

stationenliste <- Import_Stationenliste_Temperatur(df_stationenliste)
stationenliste_temp_hamburg <- filter(stationenliste, str_detect(Bundesland, "Hamburg"))

temp_akt_HH <- Import_Temperaturen_Stuendlich(df_temp_akt)
temp_hist_HH <- Import_Temperaturen_Stuendlich(df_temp_hist)


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

temp_HH_10j <- filter(temp_HH_Oktober_taeglich, MESS_JAHR >= 2000)

# Plot
# ggplot(temp_HH_10j, aes(x = mean_temp, y = MESS_JAHR, fill = ..x..)) +
#   geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
#   scale_fill_viridis(name = mean_temp, option = "C") +
#   labs(title = 'Temparaturen Hamburg') +
#   theme_ipsum() +
#   theme(
#     legend.position="none",
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 8)
#   )

#Anleitung unter https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
#Gallery unter https://r-graph-gallery.com/294-basic-ridgeline-plot.html#color
ggplot(temp_HH_10j,  aes(x=mean_temp, y=MESS_JAHR, group=MESS_JAHR))+geom_density_ridges()

#Skalen vorbereiten

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

ggplot(temp_HH_Oktober_taeglich,  aes(x=mean_temp, y=MESS_JAHR, group=MESS_JAHR))+geom_density_ridges()

ggplot(temp_HH_Oktober_taeglich,  aes(x=mean_temp, y=MESS_JAHR, group=MESS_JAHR, fill=factor(stat(quantile))))+
  scale_y_continuous(breaks = seq(min_jahr_1, max_jahr_1, by=4) )+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis_d(name = "Quartiles")
  
ggplot(temp_HH_Oktober_taeglich,  aes(x=max_temp, y=MESS_JAHR, group=MESS_JAHR, fill=factor(stat(quantile))))+
  scale_y_continuous(breaks = seq(min_jahr_1, max_jahr_1, by=4) )+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis_d(name = "Quartiles")

ggplot(temp_HH_Oktober_taeglich,  aes(x=min_temp, y=MESS_JAHR, group=MESS_JAHR, fill=factor(stat(quantile))))+
  scale_y_continuous(breaks = seq(min_jahr_1, max_jahr_1, by=4) )+
  scale_y_continuous(breaks = seq(1948, 2022, by=4) )+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis_d(name = "Quartiles")
