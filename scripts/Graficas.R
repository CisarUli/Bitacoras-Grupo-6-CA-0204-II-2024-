source("Limpieza.R")
suppressMessages(library(ggiraph)) 
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(patchwork))
suppressMessages(library(tidyr))
suppressMessages(library(sf))
suppressMessages(library(cowplot))

migrantes_desaparecidos |>    #Historico de muertes por año
  filter(!is.na(incident_date)) |>
  ggplot(aes(x = format(incident_date, "%Y"), fill = region)) +
  geom_bar() +
  labs(
    tittle = "Incidentes por año",
    x = "Año",
    y = "Incidentes"
  ) + 
  theme_minimal()



  