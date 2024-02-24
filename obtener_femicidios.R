# http://www.nomasviolenciacontramujeres.cl/registro-de-femicidios/
# registro realizado por la Red Chilena contra la Violencia hacia las Mujeres desde 2010 en adelante

install.packages("googlesheets4")
library(googlesheets4)
library(rvest)
library(purrr)
library(glue)
library(janitor)
library(lubridate)
library(dplyr)

# obtener enlaces ----

#web scraping del sitio
registro <- session("http://www.nomasviolenciacontramujeres.cl/registro-de-femicidios/")

#obtener enlaces de google sheets
registro_enlaces <- registro |> 
  read_html() |> 
  html_elements(".vce-col-inner") |> 
  html_elements("a") |> 
  html_attr("href")

# descargar datos ----
gs4_deauth() #usar google sheets sin cuenta

# tabla <- read_sheet(registro_enlaces[14])
#
# metadatos <- gs4_get(registro_enlaces[13])
# la tabla de 2024 viene en formato excel, así que no se puede obtener de la misma forma

# obtener todos los datos ----
planillas <- map(registro_enlaces, ~{
  # .x <- registro_enlaces[5]
  message(.x)
  metadatos <- gs4_get(.x) |> try()
  
  if (class(metadatos)[1] == "try-error") return(NULL) #salir si no se obtiene
  
  nombre <- metadatos$sheets$name[1] |> tolower()
  
  datos <- read_sheet(metadatos)

  return(datos)
})


# descargar datos 2024 ----
# porque esta tabla viene en otro formato, debe descargarse distinto
library(googledrive)
drive_download(registro_enlaces[15], path = "datos/femicidios 2024.xlsx")

