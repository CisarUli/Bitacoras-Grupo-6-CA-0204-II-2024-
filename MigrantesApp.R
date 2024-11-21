library(shiny)
library(maps)
suppressMessages(library(tidyverse))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(knitr))
suppressMessages(library(countrycode))
suppressMessages(library(forcats))
suppressMessages(library(cowplot))
library(knitr)
library(kableExtra)

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
migrantes_desaparecidos$cause_of_death <- as.factor(migrantes_desaparecidos$cause_of_death)
migrantes_desaparecidos$region <- as.factor(migrantes_desaparecidos$region)
migrantes_desaparecidos$country_of_death <- as.factor(migrantes_desaparecidos$country_of_death)

world_map <- map_data("world") |>
  mutate(region = case_when(
    region %in% c("USA", "Canada", "Mexico") ~ "Norteamérica",
    region %in% c("Guatemala", "El Salvador", "Honduras", "Costa Rica", "Panama", "Belize", 
                  "Nicaragua") ~ "Centroamérica",
    region %in% c("Brazil", "Argentina", "Chile", "Colombia", "Peru", "Venezuela", "Paraguay", 
                  "Uruguay", "Bolivia", "Ecuador", "Guyana", "Suriname") ~ "Suramérica",
    region %in% c("France", "Germany", "Spain", "Italy", "UK", "Poland", "Russia", "Ukraine") ~ "Europa",
    region %in% c("China", "Japan", "South Korea", "India", "Pakistan", "Bangladesh") ~ "Asia",
    region %in% c("Egypt", "Morocco", "Algeria", "Tunisia", "Libya", "Sudan", "Nigeria", "Ghana", 
                  "Ivory Coast", "Senegal", "Mali", "Togo", "Burkina Faso", "Democratic Republic of the Congo", 
                  "Cameroon", "Gabon", "Republic of Congo", "Chad", "Kenya", "Tanzania", "Uganda", 
                  "Ethiopia", "Somalia", "Rwanda", "Burundi", "South Africa", "Namibia", "Botswana", 
                  "Zimbabwe", "Mozambique", "Zambia", "Angola") ~ "África",
    region %in% c("Australia", "New Zealand") ~ "Oceanía",
    TRUE ~ "Otros"
  ))

migrantes_desaparecidos <- migrantes_desaparecidos |>
  mutate(mapamundi = case_when(
    country_of_death %in% c("United States", "Canada", "Mexico") ~ "Norteamérica",
    country_of_death %in% c("Guatemala", "El Salvador", "Honduras", "Costa Rica", "Panama", "Belize", 
                              "Nicaragua") ~ "Centroamérica",
    country_of_death %in% c("Brazil", "Argentina", "Chile", "Colombia", "Peru", "Venezuela", "Paraguay", 
                  "Uruguay", "Bolivia", "Ecuador", "Guyana", "Suriname") ~ "Suramérica",
    region == "Asia" ~ "Asia",            
    region == "Europe" ~ "Europa",        
    region == "Africa" ~ "África",        
    region == "Mediterranean" ~ "Europa", 
    TRUE ~ "Otros"
  ))

ui <- fluidPage(
  titlePanel("Mapa de desapariciones de migrantes"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "regiones",
        label = "Región seleccionada: ",
        choices = unique(world_map$region),
        selected = unique(world_map$region)
      )
    ),
    
    mainPanel(
      plotOutput("mapa") 
    )
  )
)

server <- function(input, output) {
  output$mapa <- renderPlot({
    mapa_filtrado <- world_map |>
      filter(region %in% input$regiones)
    casos_filtrados <- migrantes_desaparecidos |>
      filter(mapamundi %in% input$regiones)
    ggplot() +
      geom_polygon(data = mapa_filtrado, 
                   aes(x = long, y = lat, group = group),
                   fill = "lightblue", color = "white") +
      geom_point(data = casos_filtrados,
                 aes(x = longitud,
                     y = latitud),
                 color = "red", size = 0.5, alpha = 0.7) +
      theme_minimal() +
      labs(
        title = "Mapa Mundial",
        subtitle = paste("Región seleccionada:", paste(input$regiones, collapse = ", "))
      )
  })
}

shinyApp(ui = ui, server = server)