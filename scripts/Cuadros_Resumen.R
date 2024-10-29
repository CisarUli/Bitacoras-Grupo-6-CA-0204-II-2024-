source("Limpieza.R")
migrantes_desaparecidos |>  
  filter(!is.na(incident_date)) |>   # Cambiar por la variable a resumir
  group_by(Año = format(incident_date, "%Y")) |>    # Cambiar por v a resumir
  summarise(
    prom_m = mean(number_dead),
    prom_S = mean(number_of_survivors),
    prom_d = mean(minimum_estimated_number_of_missing),
    max_m = max(number_dead),
    max_S = max(number_of_survivors),
    max_d = max(minimum_estimated_number_of_missing),
    var_m = var(number_dead),
    var_S = var(number_of_survivors),
    var_d = var(minimum_estimated_number_of_missing),
    desv_m = sd(number_dead),
    desv_S = sd(number_of_survivors),
    desv_d = sd(minimum_estimated_number_of_missing),
    per_m = sum(number_dead) / 
      (sum(number_dead) +
         sum(number_of_survivors) +
         sum(minimum_estimated_number_of_missing)) * 100,
    per_S = sum(number_of_survivors) / 
      (sum(number_dead) +
         sum(number_of_survivors) +
         sum(minimum_estimated_number_of_missing)) * 100,
    per_d = sum(minimum_estimated_number_of_missing) / 
      (sum(number_dead) +
         sum(number_of_survivors) +
         sum(minimum_estimated_number_of_missing)) * 100
    ) |>
  kable( 
        caption = "Resumen de migrantes desaparecidos por Año")

migrantes_desaparecidos |>
  group_by(cause_of_death) |>
  summarise(
    muertes = sum(number_dead, na.rm = TRUE),
    porcentaje = muertes/sum(migrantes_desaparecidos$number_dead, na.rm = TRUE) * 100)

migrantes_desaparecidos |> #Muerte por sexo por año
  filter(
    is.na(number_of_survivors),
    is.na(minimum_estimated_number_of_missing)
  ) |>
  group_by(year) |>
  summarise(
    mujeres = sum(number_of_females, na.rm = TRUE),
    hombres = sum(number_of_males, na.rm = TRUE),
    per_hombres = hombres/(hombres + mujeres) * 100,
    per_mujeres = mujeres/(mujeres + hombres) * 100
  )  

migrantes_desaparecidos |> #Muerte por sexo por año
  group_by(year) |>
  summarise(
    mujeres = sum(number_of_females, na.rm = TRUE),
    hombres = sum(number_of_males, na.rm = TRUE),
    per_hombres = hombres/(hombres + mujeres) * 100,
    per_mujeres = mujeres/(mujeres + hombres) * 100
  )

library(knitr)
library(kableExtra)

elementos <- data.frame(
  Primarios = c(
    "Las medidas de tendencia central", 
    "Cómo utilizar el modelo ARIMA",
    "Análisis de regresión lineal simple",
    "Geografía de la migración",
    "Predicción de muertes de migrantes",
    "Predicción de sobrevivientes migrantes",
    "Predicción de desapariciones de migrantes"
  ),
  Secundarios = c(
    "Implicaciones de la desaparición de inmigrantes", 
    "Aspectos psicológicos de la inmigración",
    "Importancia de los datos migratorios para modelar y formar correctas políticas",
    "Protección de migrantes de desapariciones forzadas",
    "Comparación de muertes y rescates de migrantes",
    " ",
    " "
  )
)
kable(elementos, "html", col.names = c("Primarios", "Secundarios")) |>
  kable_styling("striped", full_width = F) |>
  add_header_above(c("Elementos de reporte" = 2)) |>
  column_spec(1:2, width = "5cm", extra_css = "text-align: left;") |>
  row_spec(0, bold = T)


