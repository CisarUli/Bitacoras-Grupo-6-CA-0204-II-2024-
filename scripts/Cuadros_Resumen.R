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


