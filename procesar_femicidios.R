# en este script se cargan todos los datos obtenidos mediante el web scrapping, y se procesan para dejar en un formato limpio. 
# se corrigen los nombres, las fechas, las edades, Y la mayoría de las variables categóricas sobre los crímenes son limpiadas y también simplificadas en versiones más breves (con menos niveles o categorías).
# finalmente, la mayor parte de este script busca hacer coincidir la información de múltiples variables que indican la ubicación de los hechos delictuales con una lista estandarizada de las comunas del país, de modo que cada femicidio tenga un código de comuna único que permita visualizar los datos geográficamente. Esto resulta particularmente complejo, porque la información geográfica puede encontrarse en distintas columnas dependiendo del año, y la información geográfica no está indicada de manera exacta, sino que contiene ubicaciones poco precisas, mal escritas, con ortografía incorrecta o alternativa, o simplemente no existe información de ubicación pero si existe referencia a lugares en la información de cada caso.

library(fs)
library(dplyr)
library(stringr)
library(forcats)
library(purrr)
library(glue)
library(lubridate)
library(tidyr)
library(textclean)

# cargar planillas individuales ----

archivos <- dir_ls("datos", regexp = "xlsx") |> str_subset("consolidado", negate = T)

# carga todos los datos que están en archivos Excel separados por año
datos <- map(archivos, readxl::read_xlsx) |> 
  list_rbind()


# limpiar datos ----
datos_2 <- datos |> 
  # nombre: unir los nombres que están en dos columnas en una sola columna
  mutate(nombre_victima = if_else(is.na(nombre_victima) & !is.na(nombre), nombre, nombre_victima)) |> 
  select(-nombre) |> 
  # fecha: unificar fechas de femicidio
  mutate(fecha_2 = ymd(fecha)) |> 
  mutate(fecha_3 = str_extract(fecha, "\\d+-\\d+-\\d{4}"),
         fecha_4 = dmy(fecha_3)) |> 
  mutate(fecha_femicidio = if_else(is.na(fecha_2) & !is.na(fecha_4), fecha_4, fecha_2)) |> 
  mutate(año = year(fecha_femicidio)) |>
  select(-fecha, -fecha_2, -fecha_3, -fecha_4) |> 
  mutate(informacion_medios_1 = str_remove(informacion_medios_1, " .*"),
         informacion_medios_2 = str_remove(informacion_medios_2, " .*"))

## edades ----
# las edades están en distintas columnas dependiendo del año de los datos
datos_3 <- datos_2 |>
  mutate(edad_victima = case_when(año %in% c(2022, 2023) ~ edad_victima,
                                  año %in% c(2011:2013) ~ edad_victima,
                                  año %in% c(2010, 2014:2021) ~ edad, 
                                  año == 2024 ~ edad,
                                  .default = edad_victima)) |> 
  # víctimas menores de 1 año
  mutate(edad_victima = case_when(str_detect(edad_victima, "mes") ~ "0",
                                  .default = edad_victima)) |>
  mutate(edad_victima = as.character(edad_victima),
         edad_victima = str_extract(edad_victima, "\\d+"),
         edad_victima = as.integer(edad_victima)) |> 
  # edad femicida
  mutate(edad_femicida = case_when(año %in% c(2022, 2023) ~ edad,
                                   año %in% c(2011:2013) ~ edad_femicida,
                                   año %in% c(2010, 2014:2021) ~ edad_2, 
                                   año == 2024 ~ edad_femicida,
                                   .default = edad_femicida)) |> 
  mutate(edad_femicida = as.character(edad_femicida),
         edad_femicida = str_extract(edad_femicida, "\\d+"),
         edad_femicida = as.integer(edad_femicida)) |> 
  select(-edad, -edad_2) |> 
  mutate(diferencia_edad = edad_femicida - edad_victima)



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
  # agresiones: reducir categorías
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
  # violencia intrafamiliar: reducir categorías
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
  # violencia sexual: reducir categorías
  mutate(violencia_sexual = case_when(str_detect(violencia_sexual, "Si|Sí|si|sí") ~ "Violencia sexual",
                                      str_detect(violencia_sexual, "No|no") ~ "Sin violencia sexual",
                                      str_detect(violencia_sexual, "presume|presunta|investiga") ~ "Presunta violencia sexual",
                                      is.na(violencia_sexual) ~ "Se desconoce",
                                      .default = "Se desconoce")) |> 
  # categoria
  mutate(categoria_femicidio = case_when(!is.na(categoria_red_chilena) ~ categoria_red_chilena,
                                         !is.na(categorias_red_chilena) ~ categorias_red_chilena,
                                         !is.na(tipificacion_red_chilena) ~ tipificacion_red_chilena)
  ) |> 
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


# correcciones y orden ----
datos_5 <- datos_4 |> 
  # ordenar variables
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






# lugares y comunas ----
# existen columnas de comuna, lugar, región, y también una columna de información extra. En los siguientes pasos se intenta usar todas estas columnas para coincidirlas con los códigos únicos territoriales de las comunas, para así georeferenciar los casos

# cargar lista de comunas
cut_comunas <- readr::read_csv2("datos/comunas_chile_cut.csv") |> 
  mutate(comuna_match = comuna |> strip() |> replace_non_ascii())

# crear id único para cada caso
datos_5b <- datos_5 |> 
  mutate(id2 = 1:n())

## casos con comuna ----
# coincidir la comuna de los datos con las comunas de la lista de comunas
match_comuna <- datos_5b |>
  select(-region) |> 
  mutate(comuna_match = comuna |> strip() |> replace_non_ascii()) |> 
  select(-comuna) |> 
  left_join(cut_comunas,
            by = join_by(comuna_match == comuna_match))

glimpse(match_comuna)

# separar los datos entre los que obtuvieron una coincidencia entre la comuna con la lista de comunas y los que no
con_comuna <- match_comuna |> 
  filter(!is.na(cut_comuna))

# casos sin comuna
sin_comuna <- match_comuna |> 
  filter(is.na(cut_comuna))  



## casos con comuna en variable lugar ----
# si en el paso anterior no se coincidió la comuna con la lista de comunas, se intenta buscar en la comuna en la columna de lugares

match_lugar <- sin_comuna |> 
  select(-region, -cut_region, -cut_comuna, -comuna) |> 
  mutate(lugar_match = lugar |> strip() |> replace_non_ascii()) |> 
  left_join(cut_comunas,
            by = join_by(lugar_match == comuna_match))

match_lugar |> glimpse()

# casos donde se obtuvo una comuna a partir del lugar
con_match_lugar <- match_lugar |> 
  filter(!is.na(cut_comuna))

# casos sin comuna en variable lugar
sin_match_lugar <- match_lugar |> 
  filter(is.na(cut_comuna))

# comunas no reconocidas
sin_match_lugar |> pull(lugar) |> unique()

sin_lugar <- sin_match_lugar |> 
  filter(is.na(lugar))

## comuna mencionada en información ----
# si no se obtuvo una coincidencia desde la columna de comuna, ni la columna del lugar, buscar mención de una comuna en la columna de información extra
match_informacion <- sin_lugar |>
  select(-cut_comuna, -cut_region, -comuna, -comuna_match) |> 
  # detectar mención de comunas dentro del párrafo de información
  mutate(comuna = case_when(str_detect(informacion_sobre_el_hecho, "Playa Negra") ~ "Penco",
                            str_detect(informacion_sobre_el_hecho, "Concepción") ~ "Concepción",
                            str_detect(informacion_sobre_el_hecho, "La Huayca") ~ "Iquique",
                            str_detect(informacion_sobre_el_hecho, "Cerro Chuño") ~ "Arica",
                            str_detect(informacion_sobre_el_hecho, "Antogafasta") ~ "Antofagasta",
                            str_detect(informacion_sobre_el_hecho, "toma Porvenir") ~ "Alto Hospicio",
                            str_detect(informacion_sobre_el_hecho, "Victoria") ~ "Victoria",
                            str_detect(informacion_sobre_el_hecho, "Pichilemu") ~ "Pichilemu",
                            str_detect(informacion_sobre_el_hecho, "río Aconcagua") ~ "Valparaíso",
                            str_detect(informacion_sobre_el_hecho, "Constitución") ~ "Constitución",
                            str_detect(informacion_sobre_el_hecho, "Zuñiga") ~ "San Vicente",
                            str_detect(informacion_sobre_el_hecho, "Puerto Montt") ~ "Puerto Montt",
                            str_detect(informacion_sobre_el_hecho, "San Pedro") ~ "San Pedro de la Paz",
                            str_detect(informacion_sobre_el_hecho, "Calama") ~ "Calama",
                            str_detect(informacion_sobre_el_hecho, "El Tume") ~ "Villarrica",
                            str_detect(informacion_sobre_el_hecho, "Cesfam Pedro del Río") ~ "Concepción",
                            str_detect(informacion_sobre_el_hecho, "Renaico") ~ "Renaico",
                            str_detect(informacion_sobre_el_hecho, "Metrenco") ~ "Padre Las Casas",
                            str_detect(informacion_sobre_el_hecho, "Posta Central") ~ "Santiago",
                            str_detect(informacion_sobre_el_hecho, "Antonio Cabrera Santiago") ~ "Santiago",
                            str_detect(informacion_sobre_el_hecho, "Osorno") ~ "Osorno")) |> 
  select(-region) |> 
  left_join(cut_comunas,
            by = join_by(comuna == comuna)) |> 
  filter(!is.na(cut_comuna))

match_informacion |> glimpse()



## comuna manualmente corregida ----
# coincidir manualmente comunas que están escritas distinto que la lista de comunas
match_manual <- sin_match_lugar |> 
  select(-cut_comuna, -cut_region, -region, -comuna, -comuna_match) |> 
  mutate(comuna = case_match(lugar, 
                             "Aysén" ~ "Aisén", 
                             "Los Ángeles" ~ "Los Angeles", 
                             "Tal Tal" ~ "Taltal", 
                             "Padre las Casas" ~ "Padre Las Casas",
                             "Coyhaique" ~ "Coihaique",
                             "Alhue" ~ "Alhué",
                             "Concon" ~ "Concón",
                             "El Olivar" ~ "Olivar",
                             "Macúl" ~ "Macul",
                             "Valparaiso" ~ "Valparaíso",
                             "Llay Llay" ~ "Llaillay",
                             "Trehuaco" ~ "Treguaco",
                             "Neltume" ~ "Panguipulli",
                             "Entre Lagos" ~ "Puyehue",
                             "Rapel" ~ "Navidad",
                             "Huentelolen" ~ "Tirúa",
                             "Puerto Natales" ~ "Natales",
                             "San Francisco de Mostazal" ~ "Mostazal",
                             "San José de la Mariquina" ~ "Mariquina",
                             "Santiago Centro" ~ "Santiago",
                             "Lontué" ~ "Molina",
                             "Reinaco" ~ "Renaico",
                             "Paipote" ~ "Copiapó",
                             "Labranza" ~ "Temuco",
                             "Puerto Cisnes" ~ "Cisnes",
                             "Batuco" ~ "Lampa",
                             "Rallenco" ~ "Panguipulli",
                             "San Vicente de Tagua Tagua" ~ "San Vicente",
                             "Hornopirén" ~ "Hualaihué",
                             "Colín" ~ "Maule",
                             "Araucanía" ~ "Temuco",
                             "Coñaripe" ~ "Panguipulli",
                             "Rosario" ~ "Rengo",
                             "La Calera" ~ "Calera",
                             "Rio Negro" ~ "Río Negro",
                             "Puerto Saavedra" ~ "Curacautín",
                             "La Granja / Copiapó" ~ "La Granja",
                             "Puerto Aysen" ~ "Aisén",
                             "Liquiñe" ~ "Panguipulli",
                             "Curanipe" ~ "Pelluhue",
                             .default = lugar)) |> 
  left_join(cut_comunas,
            by = join_by(comuna == comuna)) |> 
  filter(!is.na(cut_comuna))

match_manual |> glimpse()
# cut_comunas |> 
#   filter(str_detect(comuna, "Pichilemu"))



## unir ----
# unir todos los resultados de las distintas coincidencias con la lista de comunas
datos_6 <- bind_rows(con_comuna,
                     con_match_lugar,
                     match_informacion,
                     match_manual) |> 
  relocate(lugar, comuna, region, .after = nombre_victima)


datos_6

## casos sin datos ----
sin_ningun_match <- datos_5b |> 
  filter(!id2 %in% unique(datos_6$id2))

# por región ----
# si no se obtuvo ninguna coincidencia, dar un tratamiento especial
match_region <- sin_ningun_match |> 
  # select(lugar, comuna, region) |> 
  mutate(comuna = case_when(region == "Metropolitana" ~ "Santiago")) |> 
  select(-region) |> 
  left_join(cut_comunas,
            by = join_by(comuna == comuna))
# y se dejan los sin match 

match_region |> glimpse()

# sin_ningun_match_2 <- sin_ningun_match |> 
#   filter(!id2 %in% unique(match_region$id2))



# unir resultados ----
datos_7 <- bind_rows(con_comuna,
                     con_match_lugar,
                     match_informacion,
                     match_manual,
                     match_region)

datos_7 |> 
  select(starts_with("comuna"))

# ordenar resultados
datos_8 <- datos_7 |> 
  distinct(id2, .keep_all = TRUE) |> 
  select(-region) |> 
  left_join(cut_comunas |> select(cut_comuna, region),
            by = join_by(cut_comuna)) |> 
  relocate(lugar, comuna, region, .after = nombre_victima) |> 
  select(-id2) |> 
  arrange(desc(fecha_femicidio))



# datos_7 |> 
#   filter(region == "Metropolitana de Santiago") |> 
#   select(fecha_femicidio, comuna, lugar, region) |> 
#   filter(is.na(comuna))



# revisiones ----

# misma cantidad de casos antes y después de procesamiento
nrow(datos_2) == nrow(datos_7)

# todos los datos con año
!any(is.na(datos_7$año))

# datos del año presentes
max(datos_7$año) == year(today())

datos_7 |> glimpse()



# guardar ----
writexl::write_xlsx(datos_7, "datos/femicidios_chile_consolidado.xlsx")
arrow::write_parquet(datos_7, "datos/femicidios_chile_consolidado.parquet")
