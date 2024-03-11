# http://www.nomasviolenciacontramujeres.cl/registro-de-femicidios/
# registro realizado por la Red Chilena contra la Violencia hacia las Mujeres desde 2010 en adelante

# este script obtiene los datos sobre femicidios en Chile y los descarga.
# obtiene los enlaces con el paquete {rvest} haciendo web scraping del sitio de la Red,
# luego descarga los datos desde Google Docs usando el paquete {googlesheets4}

# install.packages("googlesheets4")

library(googlesheets4)
library(rvest)
library(purrr)
library(glue)
library(janitor)
library(lubridate)
library(dplyr)
library(tidyr)

#obtener enlaces ----

#web scraping del sitio
registro <- session("http://www.nomasviolenciacontramujeres.cl/registro-de-femicidios/")

#obtener enlaces a los datos, que están en google sheets
registro_enlaces <- registro |> 
  read_html() |> 
  html_elements(".vce-col-inner") |> 
  html_elements("a") |> 
  html_attr("href")

gs4_deauth() #usar google sheets sin cuenta

# read_sheet(registro_enlaces[1])

# obtener todos los datos ----
planillas <- map(registro_enlaces, ~{
  message(glue("descargando archivo {.x}..."))
  
  #descargar planilla
  datos <- read_sheet(.x) |> try()
  
  if (class(datos)[1] == "try-error") {
    return(NULL) #salir si no se obtiene archivo
  } else {
    return(datos)
  }
})

# planillas <- map(registro_enlaces, ~{
#   message(glue("descargando archivo{.x}..."))
#   metadatos <- gs4_get(.x) |> try()
#   
#   if (class(metadatos)[1] == "try-error") return(NULL) #salir si no se obtiene archivo
#   
#   # nombre <- metadatos$sheets$name[1] |> tolower()  #obtener nombre de la hoja de la planilla
#   
#   datos <- read_sheet(metadatos)
# 
#   return(datos)
# })


# descargar datos 2024 ----
# porque esta tabla viene en otro formato, debe descargarse distinto
library(googledrive)
drive_download(registro_enlaces[15], path = "datos/femicidios_2024.xlsx")


femicidios_2024 <- readxl::read_excel("datos/femicidios_2024.xlsx")

#agregar 2024 a lista de planillas
planillas_2 <- list(planillas, femicidios_2024) |> 
  list_flatten()

# limpiar planillas y guardar en excel ----

# fechas <- map(datos[[2]], ~{
#   if (class(.x)[1] == "POSIXct") {
#     return(.x)
#   } else {
#     return(NULL)
#   }
# })
# fechas_2 <- fechas |> unlist() |> as_datetime()
# planillas[[10]]

femicidios_2024 |> 
  janitor::clean_names() |> 
  select(femicidios_2024) |> 
  slice(4:8) |> 
  mutate(fecha = janitor::excel_numeric_to_date(as.numeric(femicidios_2024)))


planillas_limpias <- map(planillas_2, ~{
  # .x = planillas[[12]] #tiene texto escrito despues de unas fechas
  # .x = planillas[[10]]
  # .x = planillas_2[[16]]
  
  # message("limpiando ", .x)
  
  if (is.null(.x)) return(tibble())
  
  datos_2 <- .x |>
    clean_names() |> 
    filter(!is.na(x5)) |> #columna que posiblemente es el nombre de la víctima
    row_to_names(1) |> 
    clean_names() |> 
    rename(id = 1, fecha = 2) |> #supuestamente el numero de la victima del año
    filter(!is.na(id)) |> 
    # mutate(fecha = map(fecha, as.character)) |> #columna fechas viene en multiples formatos
    # unnest(fecha) 
    mutate(across(where(is.list), ~map(.x, as.character))) |> #convertir todas las columnas de multiples formatos a caracter
    unnest(where(is.list), keep_empty = T) |> 
    select(-starts_with("na_"))
  
  #si las fechas vienen en formato excel (numeros que suman mas de 45 mil), convertir
  fechas <- datos_2 |> 
    filter(!is.na(fecha)) |> 
    mutate(fecha = as.integer(fecha)) |> 
    pull(fecha)
  
  if (any(!is.na(fechas)) & any(fechas > 45000)) {
    message("convirtiendo fechas excel...")
    datos_2 <- datos_2 |> 
      mutate(fecha = as.numeric(fecha),
             fecha = janitor::excel_numeric_to_date(fecha)) |> 
      mutate(fecha = as.character(fecha)) #consistencia
  }
  return(datos_2)
})


# casos por año
map_int(planillas_limpias, nrow)

planillas_limpias

# #guardar planillas individuales en excel ---- 
walk(planillas_limpias, ~{
  if (nrow(.x) > 1) {
    año <- .x |> select(fecha) |> pull() |> ymd() |> min(na.rm = T) |> year()
    writexl::write_xlsx(.x, glue("datos/femicidios_{año}.xlsx"))
  }
})
