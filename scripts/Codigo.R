library(tidyverse)
library(ggplot2)
library(ProjectTemplate)
library(ggmap)
library(dplyr)
library(tidyr)
library(knitr)
load.project()

MigrantesDesaparecidos <- rbind(
  MissingMigrants.Global.2014,
  MissingMigrants.Global.2015,
  MissingMigrants.Global.2016,
  MissingMigrants.Global.2017,
  MissingMigrants.Global.2018,
  MissingMigrants.Global.2019,
  MissingMigrants.Global.2020,
  MissingMigrants.Global.2021,
  MissingMigrants.Global.2022)

MigrantesDesaparecidos$Total.Number.of.Dead.and.Missing = as.integer(
  MigrantesDesaparecidos$Total.Number.of.Dead.and.Missing
)

MigrantesDesaparecidos <- MigrantesDesaparecidos |>
  separate(Coordinates,
           into = c("Latitud", "Longitud"),
           sep = ",", fill = "left")
MigrantesDesaparecidos$Latitud <- as.numeric(MigrantesDesaparecidos$Latitud)
MigrantesDesaparecidos$Longitud <- as.numeric(MigrantesDesaparecidos$Longitud)

TodoMigrantesDesaparecidos <- MigrantesDesaparecidos |>
  mutate(
    Number.Dead = replace_na(Number.Dead, 0),
    Number.of.Survivors = replace_na(Number.of.Survivors, 0)
  )

total_incidentes <- MigrantesDesaparecidos |>
  group_by(Region) |>
  summarise(Total = n())  
max_value <- max(total_incidentes$Total)
regiones_relevantes <- total_incidentes %>%
  filter(Total >= 0.2 * max_value)
RelevanteMigrantesDesaparecidos <- MigrantesDesaparecidos %>%
  filter(Region %in% regiones_relevantes$Region)

Resumen_Anual <- MigrantesDesaparecidos |>
  filter(!is.na(Incident.Date)) |>
  group_by(Year) %>%
  summarise(mean_muertes = mean(Number.Dead, na.rm = TRUE),
            median_muertes = median(Number.Dead, na.rm = TRUE),
            range_muertes = paste(range(Number.Dead, na.rm = TRUE), collapse = " a "),
            var_muertes = var(Number.Dead, na.rm = TRUE),
            sd_muertes = sd(Number.Dead, na.rm = TRUE),
            mean_sobrevivientes = mean(Number.of.Survivors, na.rm = TRUE),
            median_sobrevivientes = median(Number.of.Survivors, na.rm = TRUE),
            range_sobrevivientes = paste(range(Number.of.Survivors, na.rm = TRUE), collapse = " a "),
            var_sobrevivientes = var(Number.of.Survivors, na.rm = TRUE),
            sd_sobrevivientes = sd(Number.of.Survivors, na.rm = TRUE)
            )

Resumen_Regional <- MigrantesDesaparecidos |>
  group_by(Region) %>%
  summarise(mean_muertes = mean(Number.Dead, na.rm = TRUE),
            median_muertes = median(Number.Dead, na.rm = TRUE),
            range_muertes = paste(range(Number.Dead, na.rm = TRUE), collapse = " a "),
            var_muertes = var(Number.Dead, na.rm = TRUE),
            sd_muertes = sd(Number.Dead, na.rm = TRUE),
            mean_sobrevivientes = mean(Number.of.Survivors, na.rm = TRUE),
            median_sobrevivientes = median(Number.of.Survivors, na.rm = TRUE),
            range_sobrevivientes = paste(range(Number.of.Survivors, na.rm = TRUE), collapse = " a "),
            var_sobrevivientes = var(Number.of.Survivors, na.rm = TRUE),
            sd_sobrevivientes = sd(Number.of.Survivors, na.rm = TRUE)
  )


RelevanteMigrantesDesaparecidos |> #Regiones con más de 500 incidentes
  ggplot(aes(x = Region)) + 
  geom_bar(fill = "darkblue") +
  theme_minimal() +
  labs(title = "Incidentes por Región",
       x = "Región",
       y = "Total de Incidentes")

MigrantesDesaparecidos |> #Todas las regiones
  ggplot(aes(x = Region)) + 
  geom_bar(fill = "darkblue") +
  theme_minimal() +
  labs(title = "Incidentes por Región",
       x = "Región",
       y = "Total de Incidentes")

RelevanteMigrantesDesaparecidos |> #Regiones con más de 500 incidentes
  ggplot(aes(x = Region, y = Number.Dead)) +
  geom_col(fill = "cyan") +
  theme_minimal() +
  labs(
    title = "Total de Muertes por Región",
    x = "Región",
    y = "Total de Muertes")

MigrantesDesaparecidos |> #Todas las regiones
ggplot(aes(x = Region, y = Number.Dead)) +
  geom_col(fill = "orange") +
  theme_minimal() +
  labs(
    title = "Total de Muertes por Región",
       x = "Región",
       y = "Total de Muertes")

MigrantesDesaparecidos |> #Falta que aparezca el mapa
ggplot(aes(x = Longitud, y = Latitud)) +
  geom_point(color = "red", size = 0.5) +
  theme_minimal() +
  labs(
    title = "Mapa de Incidentes según Coordenadas",
    x = "Longitud",
    y = "Latitud")

MigrantesDesaparecidos |>
  ggplot(aes(x = Cause.of.Death)) +
  geom_bar(fill = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Distribución de las Causas de Muerte")

TodoMigrantesDesaparecidos |>
  ggplot(aes(x = Number.Dead, y = Number.of.Survivors)) +
  geom_point(color = "purple", size = 3) +
  theme_minimal() +
  labs(
    title = "Supervivientes vs Fallecidos",
    x = "Número de Fallecidos",
    y = "Número de Supervivientes")
