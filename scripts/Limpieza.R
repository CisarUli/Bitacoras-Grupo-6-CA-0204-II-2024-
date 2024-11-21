suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(knitr))
suppressMessages(library(countrycode))

lista_archivos <- lapply(
  X = dir(path = "data/"),
  FUN = function(f) {
    read_csv(paste0("data/", f))
  }
)
migrantes_desaparecidos <- bind_rows(lista_archivos) |>
  janitor::clean_names()

migrantes_desaparecidos <- migrantes_desaparecidos |> # separate_wider_delim(col, delim)
  separate(coordinates,
    into = c("latitud", "longitud"),
    sep = ",", fill = "left"
  )
migrantes_desaparecidos$latitud <- as.numeric(migrantes_desaparecidos$latitud)
migrantes_desaparecidos$longitud <- as.numeric(migrantes_desaparecidos$longitud) # separa las coordenadas

migrantes_desaparecidos <- migrantes_desaparecidos |> # extrae la fecha exacta
  mutate(incident_date = as.Date(
    str_extract(
      incident_date,
      "\\b\\d{2}/\\d{2}/\\d{4}\\b"
    ),
    format = "%m/%d/%Y"
  ))
migrantes_desaparecidos <- migrantes_desaparecidos[, -c(1, 6, 9, 16, 20:21)] # elimina la columna 1
migrantes_desaparecidos <- migrantes_desaparecidos |>
  mutate(
    location_of_death = str_remove_all(location_of_death, " \\(see coordinates for exact location\\)")
  )
migrantes_desaparecidos <- migrantes_desaparecidos |> # Extrae países y causas de muerte
  mutate(
    country_of_death = countrycode(location_of_death,
      origin = "country.name",
      destination = "country.name"
    ),
    cause_of_death = str_extract(cause_of_death, "^[^/]+"),
    region = str_extract(
      region,
      "Asia|America|Africa|Europe|Caribbean|Mediterranean"
    ),
    region = str_replace(region, "Caribbean", "America")
  )
# migrantes_desaparecidos <- migrantes_desaparecidos |>
# mutate(across(where(is.numeric), ~ replace_na(., 0))
# )
migrantes_desaparecidos$cause_of_death <- as.factor(migrantes_desaparecidos$cause_of_death)
migrantes_desaparecidos$region <- as.factor(migrantes_desaparecidos$region)
migrantes_desaparecidos$country_of_death <- as.factor(migrantes_desaparecidos$country_of_death)
