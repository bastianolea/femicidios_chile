library(fs)
library(dplyr)
library(stringr)
library(purrr)
library(glue)
library(lubridate)
library(tidyr)


# cargar planillas individuales ----
archivos <- dir_ls("datos", regexp = "xlsx") |> str_subset("consolidado", negate = T)

datos <- map(archivos, ~{
  readxl::read_xlsx(.x)
  }) |> 
  list_rbind()

# 
# # revisar datos ----
# datos |> names()
# 
# datos |> filter(is.na(id)) #hay casos repetidos
# 
# 
# ## revisar edad ----
# datos |> select(id, fecha, 
#                 starts_with("edad")) |> 
#   filter(is.na(edad_victima),
#          is.na(edad) | is.na(edad_2)) |> 
#   mutate(edad_femicida = case_when(
#   mutate(edad_victima = if_else(is.na(edad_victima) & !is.na(edad), edad, edad_victima),
#          edad_victima = if_else(is.na(edad_victima) & !is.na(edad_2), edad_2, edad_victima)) |> 
#   filter(is.na(edad_victima))
# 
# datos |> filter(is.na(lugar))
# datos |> filter(is.na(nombre))
# datos |> count(lugar, region, comuna)
# 
# ## revisar nombre ----
# datos |> select(id, fecha, 
#                 starts_with("nombre")) |> 
#   filter(is.na(nombre_victima)) |> 
#   mutate(nombre_victima = if_else(is.na(nombre_victima) & !is.na(nombre), nombre, nombre_victima)) |> 
#   filter(is.na(nombre_victima))
# 
# ## revisar fecha ----
# datos |> select(id, fecha) |> 
#   #convertir textos a fecha, a ver cuántos no son transformados automáticamente
#   mutate(fecha_2 = ymd(fecha)) |> 
#   filter(is.na(fecha_2)) |> #quedan 6 filas sin fecha, 5 de ellas son fechas que además tienen texto, por lo que no se convierten a formato fecha automáticamente
#   #usar regex para extraer la fecha del texto, luego convertir esas fechas en formato fecha
#   mutate(fecha_3 = str_extract(fecha, "\\d+-\\d+-\\d{4}"),
#          fecha_4 = dmy(fecha_3)) |> 
#   #aplicar nuevas fechas obtenidas a la columna principal de fechas
#   mutate(fecha_2 = if_else(is.na(fecha_2) & !is.na(fecha_4), fecha_4, fecha_2))
# 
# datos |> count(categorias_red_chilena)
# datos |> filter(is.na(categorias_red_chilena)) |> 
#   glimpse()
# 
# ## revisar categoría ----
# datos |> count(categorias_red_chilena, 
#                categoria_red_chilena) |> 
#   mutate(categoria_femicidio = if_else(is.na(categorias_red_chilena) & !is.na(categoria_red_chilena),
#                                        categoria_red_chilena,
#                                        categorias_red_chilena)) |> 
#   mutate(categoria_femicidio = str_to_sentence(categoria_femicidio),
#          categoria_femicidio = str_remove(categoria_femicidio, " /.*")) |> 
#   mutate(categoria_femicidio = replace_na(categoria_femicidio, "Desconocido")) |> 
#   count(categoria_femicidio)
# 
# datos |> filter(is.na(relacion_victima_femicida)) |> 
#   glimpse()
# 

# limpiar datos ----
datos_2 <- datos |> 
  #nombre
  mutate(nombre_victima = if_else(is.na(nombre_victima) & !is.na(nombre), nombre, nombre_victima)) |> 
  select(-nombre) |> 
  #fecha
  mutate(fecha_2 = ymd(fecha)) |> 
  mutate(fecha_3 = str_extract(fecha, "\\d+-\\d+-\\d{4}"),
         fecha_4 = dmy(fecha_3)) |> 
  mutate(fecha_femicidio = if_else(is.na(fecha_2) & !is.na(fecha_4), fecha_4, fecha_2)) |> 
  mutate(año = year(fecha_femicidio)) |>
  select(-fecha, -fecha_2, -fecha_3, -fecha_4)


# datos_2 |> 
#   select(id, año, edad, edad_2, edad_victima, edad_femicida) |> 
#   # filter(año == 2021)
#   filter(año %in% c(2022, 2023))
# 
# #femicida
# 2010:2021 edad_2
# 2022, 2023 edad
# 
# 
# #victima
# 2010:2021 edad
# 2022, 2023 edad_victima

# edades
datos_3 <- datos_2 |>
  mutate(edad_victima = case_when(año %in% c(2022, 2023) ~ edad_victima,
                                  año %in% c(2011:2013) ~ edad_victima,
                                  año %in% c(2010, 2014:2021) ~ edad, 
                                  año == 2024 ~ edad,
                                  .default = edad_victima)) |> 
  # mutate(edad_victima = if_else(is.na(edad_victima) & !is.na(edad), edad, edad_victima)) |> 
  mutate(edad_victima = case_when(str_detect(edad_victima, "mes") ~ 0,
                                  .default = as.integer(edad_victima)),
         edad_victima = as.integer(edad_victima)) |>
  #edad femicida
  mutate(edad_femicida = case_when(año %in% c(2022, 2023) ~ edad,
                                   año %in% c(2011:2013) ~ edad_femicida,
                                   año %in% c(2010, 2014:2021) ~ edad_2, 
                                   año == 2024 ~ edad_femicida,
                                   .default = edad_femicida),
         edad_femicida = as.numeric(edad_femicida)) |> 
  # mutate(edad_femicida = if_else(is.na(edad_femicida) & !is.na(edad_2), edad_2, edad_femicida),
  #        edad_femicida = if_else(is.na(edad_femicida) & !is.na(edad), edad, edad_femicida), #en los años mas recientes (2022, 2023), "edad" es la del femicida, en las anteriores habian2 columnas edad
  #        edad_femicida = as.numeric(edad_femicida)) |> 
  select(-edad, -edad_2) |> 
  mutate(diferencia_edad = edad_femicida - edad_victima)

datos_4 <- datos_3 |> 
  #ocupaciones
  mutate(ocupacion_victima = if_else(is.na(ocupacion_victima) & !is.na(ocupacion), ocupacion, ocupacion_victima),
         ocupacion_femicida = if_else(is.na(ocupacion_femicida) & !is.na(ocupacion_2), ocupacion_2, ocupacion_femicida),
         ocupacion_victima = str_to_sentence(ocupacion_victima),
         ocupacion_femicida = str_to_sentence(ocupacion_femicida)) |> 
  select(-ocupacion, -ocupacion_2) |> 
  #categoria
  mutate(categoria_femicidio = if_else(is.na(categorias_red_chilena) & !is.na(categoria_red_chilena), categoria_red_chilena, categorias_red_chilena)) |> 
  mutate(categoria_femicidio = str_to_sentence(categoria_femicidio),
         categoria_femicidio = str_remove(categoria_femicidio, " /.*")) |> 
  mutate(categoria_femicidio = replace_na(categoria_femicidio, "Desconocido")) |> 
  select(-categorias_red_chilena, -categoria_red_chilena) |> 
  #catgegoria reducida
  mutate(categoria_femicidio_2 = str_replace(categoria_femicidio, "Desconocido", "Femicidio"),
         categoria_femicidio_2 = str_replace(categoria_femicidio_2, "Femicidio íntimo familiar", "Femicidio íntimo")) |> 
  mutate(categoria_femicidio_2 = fct_lump(categoria_femicidio_2, prop = 0.005, other_level = "Femicidio")) |>
  #subcategoria
  # mutate(subcategoria_femicidio = case_when(str_detect(categoria_femicidio, "Femicicio
  #ordenar
  arrange(fecha_femicidio, id) |> 
  select(id, fecha_femicidio, año,
         nombre_victima, edad_victima, ocupacion_victima,
         categoria_femicidio, categoria_femicidio_2,
         forma_de_agresion, violencia_sexual, relacion_victima_femicida, 
         lugar, comuna, region,
         nombre_femicida, edad_femicida, ocupacion_femicida, confiesa_delito,
         tipificacion_penal, sentencia, antecedentes_sobre_el_hecho, antecedentes_ley_vif,
         situacion_judicial_estado_causa, situacion_judicial_estado_femicida, 
         everything()) |> 
  #repetidos
  distinct(id, nombre_victima, .keep_all = TRUE)

# datos_2 |> count(categoria_femicidio_2)
# datos_2 |> count(año)
# datos_2 |> filter(is.na(año))
# datos_2 |> 
#   filter(año < 2010) |> 
#   select(fecha, año)


datos_4 |> filter(año == max(año, na.rm=T)) |> dplyr::count(edad_victima)
datos_4 |> filter(año == max(año, na.rm=T)) |> dplyr::count(edad_femicida)

datos_4 |> filter(año == 2024)
datos_4 |> filter(is.na(año))


datos_4 |> group_by(año) |> 
  summarize(mean(edad_victima, na.rm = T),
            mean(edad_femicida, na.rm = T)
  )

datos_4 |> glimpse()

# correcciones ----
datos_5 <- datos_4 |> 
  mutate(fecha_femicidio = if_else(nombre_victima == "Rosa Elena Letelier López",
                                   dmy("01-03-2012"), fecha_femicidio),
         año = if_else(nombre_victima == "Rosa Elena Letelier López",
                                   2012, año)
         )
                                     

# guardar ----
writexl::write_xlsx(datos_5, "datos/femicidios_chile_consolidado.xlsx")
arrow::write_parquet(datos_5, "datos/femicidios_chile_consolidado.parquet")
