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

# # obtener todos los datos ----
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

