library(tidyverse)
library(ggmap)
MissingMigrants.Global.2015 |>
  ggplot(aes(x = Region)) + 
  geom_bar(fill = "darkblue") +
  theme_minimal() +
  labs(title = "Incidentes por Región",
       x = "Región",
       y = "Total de Incidentes")
MissingMigrants.Global.2015 |>
ggplot(aes(x = Region, y = Number.Dead)) +
  geom_col(fill = "orange") +
  theme_minimal() +
  labs(
    title = "Total de Muertes por Región",
       x = "Región",
       y = "Total de Muertes")
MissingMigrants.Global.2015 |>
ggplot(aes(x = Longitude, y = Latitude)) +
  geom_point(color = "red", size = 2) +
  theme_minimal() +
  labs(
    title = "Mapa de Incidentes según Coordenadas",
    x = "Longitud",
    y = "Latitud")
MissingMigrants.Global.2015 |>
  ggplot(aes(x = Cause.of.Death)) +
  geom_bar(fill = "lightgreen") +
  theme_minimal() +
  labs(
    title = "Distribución de las Causas de Muerte")
MissingMigrants.Global.2015 |> 
  ggplot(aes(x = as.Date(Incident.Date))) +
  geom_line(stat = "count", color = "darkred") +
  theme_minimal() +
  labs(
    title = "Número de Incidentes por Fecha",
    x = "Fecha",
    y = "Número de Incidentes")
MissingMigrants.Global.2015 |>
  ggplot(aes(x = Gender, y = NumberDead, fill = Gender)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Número de Fallecidos por Género", x = "Género", y = "Número de Fallecidos")
