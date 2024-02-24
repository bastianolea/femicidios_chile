library(fs)
library(dplyr)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
library(tidyr)


# cargar planillas individuales ----
archivos <- dir_ls("datos")

datos <- map(archivos, ~{
  readxl::read_xlsx(.x)
  }) |> 
  list_rbind()


# revisar datos ----
datos |> names()

datos |> filter(is.na(id)) #hay casos repetidos


## revisar edad ----
datos |> select(id, fecha, 
                starts_with("edad")) |> 
  filter(is.na(edad_victima),
         is.na(edad) | is.na(edad_2)) |> 
  mutate(edad_victima = if_else(is.na(edad_victima) & !is.na(edad), edad, edad_victima),
         edad_victima = if_else(is.na(edad_victima) & !is.na(edad_2), edad_2, edad_victima)) |> 
  filter(is.na(edad_victima))

datos |> filter(is.na(lugar))
datos |> filter(is.na(nombre))
datos |> count(lugar, region, comuna)

## revisar nombre ----
datos |> select(id, fecha, 
                starts_with("nombre")) |> 
  filter(is.na(nombre_victima)) |> 
  mutate(nombre_victima = if_else(is.na(nombre_victima) & !is.na(nombre), nombre, nombre_victima)) |> 
  filter(is.na(nombre_victima))

## revisar fecha ----
datos |> select(id, fecha) |> 
  #convertir textos a fecha, a ver cuántos no son transformados automáticamente
  mutate(fecha_2 = ymd(fecha)) |> 
  filter(is.na(fecha_2)) |> #quedan 6 filas sin fecha, 5 de ellas son fechas que además tienen texto, por lo que no se convierten a formato fecha automáticamente
  #usar regex para extraer la fecha del texto, luego convertir esas fechas en formato fecha
  mutate(fecha_3 = str_extract(fecha, "\\d+-\\d+-\\d{4}"),
         fecha_4 = dmy(fecha_3)) |> 
  #aplicar nuevas fechas obtenidas a la columna principal de fechas
  mutate(fecha_2 = if_else(is.na(fecha_2) & !is.na(fecha_4), fecha_4, fecha_2))

datos |> count(categorias_red_chilena)
datos |> filter(is.na(categorias_red_chilena)) |> 
  glimpse()

## revisar categoría ----
datos |> count(categorias_red_chilena, 
               categoria_red_chilena) |> 
  mutate(categoria_femicidio = if_else(is.na(categorias_red_chilena) & !is.na(categoria_red_chilena),
                                       categoria_red_chilena,
                                       categorias_red_chilena)) |> 
  mutate(categoria_femicidio = str_to_sentence(categoria_femicidio),
         categoria_femicidio = str_remove(categoria_femicidio, " /.*")) |> 
  mutate(categoria_femicidio = replace_na(categoria_femicidio, "Desconocido")) |> 
  count(categoria_femicidio)

datos |> filter(is.na(relacion_victima_femicida)) |> 
  glimpse()


# limpiar datos ----
datos_2 <- datos |> 
  #nombre
  mutate(nombre_victima = if_else(is.na(nombre_victima) & !is.na(nombre), nombre, nombre_victima)) |> 
  #edad
  mutate(edad_victima = if_else(is.na(edad_victima) & !is.na(edad), edad, edad_victima),
         edad_victima = if_else(is.na(edad_victima) & !is.na(edad_2), edad_2, edad_victima)) |> 
  #fecha
  mutate(fecha_2 = ymd(fecha)) |> 
  mutate(fecha_3 = str_extract(fecha, "\\d+-\\d+-\\d{4}"),
         fecha_4 = dmy(fecha_3)) |> 
  mutate(fecha_femicidio = if_else(is.na(fecha_2) & !is.na(fecha_4), fecha_4, fecha_2)) |> 
  mutate(año = year(fecha)) |> 
  #categoria
  mutate(categoria_femicidio = if_else(is.na(categorias_red_chilena) & !is.na(categoria_red_chilena), categoria_red_chilena, categorias_red_chilena)) |> 
  mutate(categoria_femicidio = str_to_sentence(categoria_femicidio),
         categoria_femicidio = str_remove(categoria_femicidio, " /.*")) |> 
  mutate(categoria_femicidio = replace_na(categoria_femicidio, "Desconocido")) |> 
  #ordenar
  arrange(fecha_femicidio, id) |> 
  select(id, fecha_femicidio,
         nombre_victima, edad_victima, categoria_femicidio,
         everything())

# guardar ----
writexl::write_xlsx(datos_2, "datos/femicidios_chile_consolidado.xlsx")
arrow::write_parquet(datos_2, "datos/femicidios_chile_consolidado.parquet")
