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
drive_download(registro_enlaces[15], path = "datos/femicidios 2024.xlsx")



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

planillas_limpias <- map(planillas, ~{
  # .x = planillas[[12]] #tiene texto escrito despues de unas fechas
  # .x = planillas[[10]]
  
  if (is.null(.x)) return(tibble())
  
  .x |>
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
})


# casos por año
map_int(planillas_limpias, nrow)


# #guardar planillas individuales en excel ---- 
walk(planillas_limpias, ~{
  if (nrow(.x) > 1) {
    año <- .x |> select(fecha) |> pull() |> ymd() |> min(na.rm = T) |> year()
    writexl::write_xlsx(.x, glue("datos/femicidios_{año}.xlsx"))
  }
})
