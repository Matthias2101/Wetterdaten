#17.09.2023 Matthias Müller

#zeichnet Ridge Diagramme von Daten
# daten               Inputtabelle
# datenspalte         Spalte, die die darzustellenden Daten enthält (z.B. Temparaturen)
# gruppierungsspalte  Spalte, die die Gruppierung enthält (z.B. Jahr, wenn man tägliche Tempartaturen eines Monats für mehrere Jahre übereinander schreibt)
# zeitspalte          Spalte, die die Zeiteinheiten enthält, die ausgewertet werden, also, wenn Monate ausgewertet werden, die Tage
#ridgenoperz          Anzahl der Perzentile in den Ridge Diagrammen, default auf 3 gesetzt

#wenn man Spaltennamen als Parameter übergibt, dann dataframe[[spaltenname]] hinschreiben, ergibt vector 
ridgelinediagramm <- function(daten,datenspalte,gruppierungsspalte,zeitspalte,ridgenoperz = 3) {
  #Daten sortieren
  daten_s <- daten %>% 
    arrange(gruppierungsspalte, zeitspalte)
  #unique aus base package läßt doppelte Werte aus Vektor weg
  ydiagramme <-  unique(daten[[gruppierungsspalte]])
  ridgelinediagramm <-ggplot(daten,  aes(x=daten[[datenspalte]], y=daten[[gruppierungsspalte]], group=daten[[gruppierungsspalte]], fill=factor(stat(quantile))))+
    scale_y_continuous(breaks = ydiagramme ) +
    stat_density_ridges(
      geom = "density_ridges_gradient", calc_ecdf = TRUE,
      quantiles = ridgenoperz, quantile_lines = TRUE
    ) +
    scale_fill_viridis_d(name = "Quartiles")
  
}
