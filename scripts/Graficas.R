source("scripts/Limpieza.R")
suppressMessages(library(ggiraph)) 
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(patchwork))
suppressMessages(library(tidyr))
suppressMessages(library(sf))
suppressMessages(library(cowplot))
library(forcats)

migrantes_desaparecidos |>    #Historico de muertes por año
  filter(!is.na(incident_date)) |>
  ggplot(aes(x = format(incident_date, "%Y"))) +
  geom_bar(fill = "darkred") +
  labs(
    tittle = "Incidentes por año",
    x = "Año",
    y = "Incidentes"
  ) + 
  theme_minimal()
migrantes_desaparecidos |>    #Historico de muertes por año
  filter(!is.na(incident_date)) |>
  ggplot(aes(y = fct_infreq(region))) +
  geom_bar(fill = "darkblue") +
  labs(
    tittle = "Incidentes por region",
    x = "Region",
    y = "Incidentes"
  ) + 
  theme_minimal()

migrantes_desaparecidos |>     #grafica por causa de muerte
  filter(!is.na(incident_date)) |>
  ggplot(aes(x = number_dead, y = fct_infreq(cause_of_death), fill = region)) +
  geom_col() +
  labs(
    tittle = "Causa de muerte",
    x = "Muertes",
    y = "Causa"
  ) + 
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

migrantes_desaparecidos |>
  group_by(year) |>
  summarise(
    muertes = sum(number_dead, na.rm = TRUE),
    rescatados = sum(number_of_survivors, na.rm = TRUE)
  ) |>
  ggplot(aes(year)) +
  geom_line(aes(y = muertes), color = "darkred", size = 1) +
  geom_line(aes(y = rescatados), color = "darkblue", size = 1) +
  geom_point(aes(y = muertes), color = "orange", size = 2) +
  geom_point(aes(y = rescatados), color = "cyan", size = 2) +
  geom_smooth(aes(y = muertes), se = FALSE, size = 1, linetype = "dotted") +
  geom_smooth(aes(y = rescatados), se = FALSE, size = 1, linetype = "twodash") +
  labs(title = "Sobrevivientes vs Muertes",
       x = "Año", 
       y = "Migrantes") +
  theme_minimal()

migrantes_desaparecidos |>     #Mujeres muertes
  group_by(region) |>
  summarise(
    muertes = sum(number_of_females, na.rm = TRUE)
  ) |>
  ggplot(aes(x = muertes, y = fct_reorder(region, -muertes))) +
  geom_col(fill = "darkred") +
  labs(
    tittle = "Muertes de hombres migrantes",
    x = "Muertes",
    y = "Region"
  ) + 
  theme_minimal() 

migrantes_desaparecidos |>     #Hombres muertes
  group_by(region) |>
  summarise(
    muertes = sum(number_of_males, na.rm = TRUE)
  ) |>
  ggplot(aes(x = muertes, y = fct_reorder(region, -muertes))) +
  geom_col(fill = "darkred") +
  labs(
    tittle = "Muertes de hombres migrantes",
    x = "Muertes",
    y = "Region"
  ) + 
  theme_minimal() 
  
migrantes_desaparecidos |>     #grafica por causa de muerte con facet
  filter(!is.na(incident_date)) |>
  ggplot(aes(x = number_dead, y = fct_infreq(cause_of_death))) +
  geom_col(fill = "darkgreen") +
  facet_grid(~region) +
  labs(
    tittle = "Causa de muerte",
    x = "Muertes",
    y = "Causa"
  ) + 
  theme_minimal() 


