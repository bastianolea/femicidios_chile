library(dplyr)
library(forcats)
library(gt)
library(grid)
# library(thematic)
# library(fresh)
library(colorspace)
# library(colorjam)
# conflicted::conflicts_prefer(dplyr::count())
# conflicted::conflicts_prefer(dplyr::filter())

color_fondo = "#262626"
color_na = "#262626" |> lighten(.2)
color_detalle = "#262626" |> lighten(.4)
color_texto = "#262626" |> lighten(.6)
color_principal = "#f7d03a"
color_intermedio = "#A36F01"
color_secundario = "#f7d03a" |> darken(.8)
color_negativo = "#EB3737"
color_negativo_intermedio = "#EB802A"

# cargar datos
femicidios <- arrow::read_parquet("datos/femicidios_chile_consolidado.parquet")


datos <- femicidios |> 
  # select(-categoria_femicidio_2) |> 
  select(id, nombre_victima, fecha_femicidio,
         edad_victima,
         comuna, region,
         categoria_femicidio, forma_de_agresion, violencia_sexual, relacion_victima_femicida, antecedentes_ley_vif,
         nombre_femicida, edad_femicida, ocupacion_femicida, confiesa_delito,	tipificacion_penal,
         informacion_medios_1, informacion_medios_2) |> 
  mutate(fuente = case_when(!is.na(informacion_medios_2) ~ glue::glue("[Fuente 1]({informacion_medios_1}), [fuente 2]({informacion_medios_2})"),
                            !is.na(informacion_medios_1) ~ glue::glue("[Fuente]({informacion_medios_1})"), 
                            .default = "Sin fuentes"),
         fuente = purrr::map(fuente, gt::md)) |> 
  select(-informacion_medios_1, -informacion_medios_2) |> 
  arrange(desc(fecha_femicidio)) |> 
  slice(1:100) |> 
  mutate(across(where(is.character), ~tidyr::replace_na(.x, "Sin información")))

datos |> 
  gt() |> 
  cols_align(columns = where(is.numeric), 
             align = "center") |> 
  tab_style(locations = cells_column_labels(),
            style = cell_text(weight = "bold")) |> 
  tab_style(locations = cells_body(columns = nombre_victima),
            style = list(
              cell_text(color = color_principal, weight = "bold"))) |> 
  tab_style(locations = cells_body(columns = nombre_femicida),
            style = list(
              cell_text(color = color_negativo))) |> 
  #color edad
  data_color(
    columns = edad_victima,
    method = "numeric", domain = c(0, 100), apply_to = "text",
    palette = c(color_negativo, color_texto, color_negativo)
  ) |> 
  #color de casos sin información
  tab_style(style = cell_text(color = color_na),
            locations = cells_body(columns = comuna, rows = comuna == "Sin información")) |> 
  tab_style(style = cell_text(color = color_na),
            locations = cells_body(columns = region, rows = region == "Sin información")) |> 
  tab_style(style = cell_text(color = color_na),
            locations = cells_body(columns = categoria_femicidio, rows = categoria_femicidio == "Desconocido")) |> 
  tab_style(style = cell_text(color = color_na),
            locations = cells_body(columns = forma_de_agresion, rows = forma_de_agresion == "Desconocido/otras agresiones")) |> 
  tab_style(style = cell_text(color = color_na),
            locations = cells_body(columns = violencia_sexual, rows = violencia_sexual == "Se desconoce")) |> 
  tab_style(style = cell_text(color = color_na),
            locations = cells_body(columns = antecedentes_ley_vif, rows = antecedentes_ley_vif == "Desconocido")) |> 
  tab_style(style = cell_text(color = color_na),
            locations = cells_body(columns = relacion_victima_femicida, rows = relacion_victima_femicida == "Desconocida")) |> 
  tab_style(style = cell_text(color = color_na),
            locations = cells_body(columns = ocupacion_femicida, rows = ocupacion_femicida == "Sin información")) |> 
  tab_style(style = cell_text(color = color_na),
            locations = cells_body(columns = confiesa_delito, rows = confiesa_delito == "Sin información")) |> 
  tab_style(style = cell_text(color = color_na),
            locations = cells_body(columns = tipificacion_penal, rows = tipificacion_penal == "Sin información")) |> 
  #missing numéricos 
  tab_style(style = cell_text(color = color_fondo),
            locations = cells_body(columns = edad_femicida, rows = is.na(edad_femicida))) |> 
  tab_style(style = cell_text(color = color_fondo),
            locations = cells_body(columns = edad_victima, rows = is.na(edad_victima))) |> 
#fecha
fmt_date(
  columns = fecha_femicidio,
  date_style = "day_m_year",
  locale = "es-CL"
) |> 
  cols_label(
    id = "N°",
    nombre_victima = "Nombre de la víctima",
    fecha_femicidio = "Fecha",
    edad_victima = "Edad",
    comuna = "Comuna", region = "Región",
    categoria_femicidio = "Categoría", 
    forma_de_agresion = "Agresión", 
    violencia_sexual = "Violencia sexual", 
    relacion_victima_femicida = "Relación entre víctima y femicida", 
    antecedentes_ley_vif = "Antecedentes de violencia intrafamiliar",
    nombre_femicida = "Nombre del femicida", edad_femicida = "Edad del femicida", ocupacion_femicida = "Ocupación del femicida", confiesa_delito = "Confesión del delito",	tipificacion_penal = "Tipificación penal",
    fuente = "Información en medios de comunicación"
  ) |> 
  tab_options(table.font.color = color_texto, table.font.color.light = color_texto,
              table_body.hlines.color = color_detalle,
              table_body.vlines.color = color_detalle, 
              column_labels.border.top.color = color_fondo, column_labels.border.bottom.color = color_detalle, 
              table_body.border.bottom.color = color_detalle,
              table.background.color = color_fondo, 
              table.font.names = "Archivo Narrow")
