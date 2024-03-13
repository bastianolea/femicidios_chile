library(fs)
library(dplyr)
library(stringr)
library(forcats)
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
  select(-fecha, -fecha_2, -fecha_3, -fecha_4) |> 
  mutate(informacion_medios_1 = str_remove(informacion_medios_1, " .*"),
         informacion_medios_2 = str_remove(informacion_medios_2, " .*"))

## edades ----
datos_3 <- datos_2 |>
  mutate(edad_victima = case_when(año %in% c(2022, 2023) ~ edad_victima,
                                  año %in% c(2011:2013) ~ edad_victima,
                                  año %in% c(2010, 2014:2021) ~ edad, 
                                  año == 2024 ~ edad,
                                  .default = edad_victima)) |> 
  # mutate(edad_victima = if_else(is.na(edad_victima) & !is.na(edad), edad, edad_victima)) |> 
  mutate(edad_victima = case_when(str_detect(edad_victima, "mes") ~ "0",
                                  .default = edad_victima)) |>
  mutate(edad_victima = as.character(edad_victima),
         edad_victima = str_extract(edad_victima, "\\d+"),
         edad_victima = as.integer(edad_victima)) |> 
  #edad femicida
  mutate(edad_femicida = case_when(año %in% c(2022, 2023) ~ edad,
                                   año %in% c(2011:2013) ~ edad_femicida,
                                   año %in% c(2010, 2014:2021) ~ edad_2, 
                                   año == 2024 ~ edad_femicida,
                                   .default = edad_femicida)) |> 
  mutate(edad_femicida = as.character(edad_femicida),
         edad_femicida = str_extract(edad_femicida, "\\d+"),
         edad_femicida = as.integer(edad_femicida)) |> 
  # mutate(edad_femicida = if_else(is.na(edad_femicida) & !is.na(edad_2), edad_2, edad_femicida),
  #        edad_femicida = if_else(is.na(edad_femicida) & !is.na(edad), edad, edad_femicida), #en los años mas recientes (2022, 2023), "edad" es la del femicida, en las anteriores habian2 columnas edad
  #        edad_femicida = as.numeric(edad_femicida)) |> 
  select(-edad, -edad_2) |> 
  mutate(diferencia_edad = edad_femicida - edad_victima)

# datos_3 |> filter(is.na(edad_femicida)) |> tally()
# datos_3 |> filter(is.na(edad_victima)) |> tally()
# 
# datos_3 |> 
#   mutate(conteo = nchar(edad_femicida)) |> 
#   dplyr::count(conteo)
# 
# datos_3 |> 
#   filter(nchar(edad_femicida) >= 3) |> 
#   select(edad_femicida) |> 
  

# categóricas ----
datos_4 <- datos_3 |> 
  #ocupaciones
  mutate(ocupacion_victima = if_else(is.na(ocupacion_victima) & !is.na(ocupacion), ocupacion, ocupacion_victima),
         ocupacion_femicida = if_else(is.na(ocupacion_femicida) & !is.na(ocupacion_2), ocupacion_2, ocupacion_femicida),
         ocupacion_victima = str_to_sentence(ocupacion_victima),
         ocupacion_femicida = str_to_sentence(ocupacion_femicida),
         ocupacion_victima = if_else(is.na(ocupacion_victima), "Desconocida", ocupacion_victima),
         ocupacion_femicida = if_else(is.na(ocupacion_femicida), "Desconocida", ocupacion_femicida)
         ) |> 
  select(-ocupacion, -ocupacion_2) |> 
  #agresiones
  mutate(forma_de_agresion = tolower(forma_de_agresion),
         forma_de_agresion = case_when(str_detect(forma_de_agresion, "puñalad") ~ "apuñalada",
                                       str_detect(forma_de_agresion, "disparo|bala|balead") ~ "arma de fuego",
                                       str_detect(forma_de_agresion, "hacha") ~ "hacha",
                                       str_detect(forma_de_agresion, "degolla") ~ "degollada",
                                       str_detect(forma_de_agresion, "asfixia") ~ "asfixia",
                                       str_detect(forma_de_agresion, "estrangula") ~ "estrangulada",
                                       str_detect(forma_de_agresion, "quema|calcina") ~ "quemada",
                                       str_detect(forma_de_agresion, "golpe|contundente") ~ "golpeada",
                                       str_detect(forma_de_agresion, "lanzada") ~ "lanzada desde altura",
                                       is.na(forma_de_agresion) ~ "desconocido/otras agresiones",
                                       .default = forma_de_agresion)) |> 
  mutate(forma_de_agresion = fct_lump_min(forma_de_agresion, 2, other_level = "desconocido/otras agresiones"),
         forma_de_agresion = str_to_sentence(forma_de_agresion)) |> 
  #violencia intrafamiliar
  mutate(antecedentes_ley_vif = tolower(antecedentes_ley_vif),
         antecedentes_ley_vif = case_when(str_detect(antecedentes_ley_vif, "vif") ~ "Violencia intrafamiliar",
                                          str_detect(antecedentes_ley_vif, "d\\b") ~ "Violencia intrafamiliar",
                                          str_detect(antecedentes_ley_vif, "denuncia") ~ "Violencia intrafamiliar",
                                          str_detect(antecedentes_ley_vif, "cautelar|mc") ~ "Medida cautelar",
                                          str_detect(antecedentes_ley_vif, "aleja|acercar") ~ "Orden de alejamiento",
                                          str_detect(antecedentes_ley_vif, "no\\b") ~ "Sin antecedentes",
                                          str_detect(antecedentes_ley_vif, "si|sí") ~ "Violencia intrafamiliar",
                                          str_detect(antecedentes_ley_vif, "desconocido") ~ "Desconocido",
                                          is.na(antecedentes_ley_vif) ~ "Desconocido",
                                          .default = antecedentes_ley_vif)) |> 
  mutate(antecedentes_ley_vif = fct_lump_min(antecedentes_ley_vif, 5, other_level = "Desconocido/Otros"),
         antecedentes_ley_vif = str_to_sentence(antecedentes_ley_vif)) |>
  #violencia sexual
  mutate(violencia_sexual = case_when(str_detect(violencia_sexual, "Si|Sí|si|sí") ~ "Violencia sexual",
                                      str_detect(violencia_sexual, "No|no") ~ "Sin violencia sexual",
                                      str_detect(violencia_sexual, "presume|presunta|investiga") ~ "Presunta violencia sexual",
                                      is.na(violencia_sexual) ~ "Se desconoce",
                                      .default = "Se desconoce")) |> 
  #categoria
  mutate(
    # categoria_femicidio = if_else(is.na(categorias_red_chilena) & !is.na(categoria_red_chilena), categoria_red_chilena, categorias_red_chilena)
    categoria_femicidio = case_when(!is.na(categoria_red_chilena) ~ categoria_red_chilena,
                                    !is.na(categorias_red_chilena) ~ categorias_red_chilena,
                                    !is.na(tipificacion_red_chilena) ~ tipificacion_red_chilena)
    ) |> 
  # count(categoria_femicidio) |> arrange(desc(n)) |> print()
  mutate(categoria_femicidio = str_to_sentence(categoria_femicidio),
         categoria_femicidio = str_remove(categoria_femicidio, " /.*")) |> 
  mutate(categoria_femicidio = replace_na(categoria_femicidio, "Desconocido")) |> 
  select(-categorias_red_chilena, -categoria_red_chilena) |> 
  #catgegoria reducida
  mutate(categoria_femicidio_2 = str_replace(categoria_femicidio, "Desconocido", "Femicidio"),
         categoria_femicidio_2 = str_replace(categoria_femicidio_2, "Femicidio íntimo familiar", "Femicidio íntimo")) |> 
  mutate(categoria_femicidio_2 = fct_lump(categoria_femicidio_2, prop = 0.005, other_level = "Femicidio")) |> 
  #relacion
  mutate(relacion_victima_femicida = case_when(str_detect(relacion_victima_femicida, "Desconocido") ~ "Desconocida",
                                               is.na(relacion_victima_femicida) ~ "Desconocida",
                                               .default = relacion_victima_femicida))
  #subcategoria
  # mutate(subcategoria_femicidio = case_when(str_detect(categoria_femicidio, "Femicicio




# datos_4 |> count(categoria_femicidio) |> arrange(desc(n))  
# datos_4 |> count(categoria_femicidio_2) |> arrange(desc(n))

# datos_4 |> group_by(año) |> count(categoria_femicidio_2, .drop = F) |> arrange(desc(n)) |> print(n=Inf)
  
# datos_4 |> filter(año == max(año, na.rm=T)) |> dplyr::count(edad_victima)
# datos_4 |> filter(año == max(año, na.rm=T)) |> dplyr::count(edad_femicida)
# 
# datos_4 |> filter(año == 2024)
# datos_4 |> filter(is.na(año))
# 
# 
# datos_4 |> group_by(año) |> 
#   summarize(mean(edad_victima, na.rm = T),
#             mean(edad_femicida, na.rm = T)
#   )


# correcciones y orden ----
datos_5 <- datos_4 |> 
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
  distinct(id, nombre_victima, .keep_all = TRUE) |> 
  #datos faltantes
  mutate(fecha_femicidio = if_else(nombre_victima == "Rosa Elena Letelier López",
                                   dmy("01-03-2012"), fecha_femicidio),
         año = if_else(nombre_victima == "Rosa Elena Letelier López",
                                   2012, año)
         )
                                     

# guardar ----
writexl::write_xlsx(datos_5, "datos/femicidios_chile_consolidado.xlsx")
arrow::write_parquet(datos_5, "datos/femicidios_chile_consolidado.parquet")
arrow::write_parquet(datos_5, "app/femicidios_chile_consolidado.parquet")
