library(chilemapas)
library(ggplot2)

color_principal = "#f7d03a"
color_intermedio = "#A36F01"
color_secundario = "#332901" 
color_negativo = "#EB3737"

datos_7 <- arrow::read_parquet("datos/femicidios_chile_consolidado.parquet")
cut_comunas <- readr::read_csv2("datos/comunas_chile_cut.csv")

# mapas ----
# cargar mapa
mapa_comunas <- chilemapas::mapa_comunas %>%
  left_join(
    chilemapas::codigos_territoriales %>%
      select(matches("comuna"))) |> 
  mutate(codigo_comuna = as.numeric(codigo_comuna)) |> 
  # borrar islas
  filter(!codigo_comuna %in% c(5104, 5201)) #|>
  # # mutate(geometry = smoothr::drop_crumbs(geometry, threshold = units::set_units(30, km^2)))
  # mutate(geometry = ifelse(codigo_region %in% c("05"),
  #                          smoothr::drop_crumbs(geometry, threshold = units::set_units(30, km^2)) |> unlist(),
  #                          geometry))

# mapa de regiones
mapa_regiones <- mapa_comunas |> 
  group_by(codigo_region) |> 
  summarize(geometry = st_union(geometry))


# datos ----
casos_comuna <- datos_7 |> 
  # filter(año == 2024) |> 
  filter(año >= 2020) |> 
  group_by(comuna, region, cut_comuna) |> 
  summarize(n = n(),
            edad = mean(edad_victima, na.rm = T))

# agregar datos a mapa de comunas
mapa_datos <- mapa_comunas |> 
  left_join(casos_comuna, by = c("codigo_comuna" = "cut_comuna")) |> 
  mutate(punto = geometry |> st_simplify() |> st_centroid(of_largest_polygon = TRUE))

# visualizar ----
mapa_datos |> 
  ggplot() +
  # mapa de chile
  geom_sf(data = mapa_regiones,
          aes(geometry = geometry), 
          fill = color_principal, color = "white", alpha = 0.8) +
  # puntos
  geom_sf(aes(geometry = punto, 
              size = n),
          alpha = 0.6, color = color_negativo) +
  coord_sf(xlim = c(-76, -66.5), expand = FALSE) +
  # guides(size = guide_legend(position = "bottom")) +
  guides(size = guide_none()) +
  theme_void()


readr::write_rds(mapa_comunas, "mapa_comunas.rds")
readr::write_rds(mapa_regiones, "mapa_regiones.rds")


# tabla ----

casos_region <- casos_comuna |> 
  left_join(cut_comunas |> 
              select(cut_comuna, cut_region), by = "cut_comuna") |> 
  group_by(region, cut_region) |> 
  summarize(n = sum(n), .groups = "drop") |> 
  filter(!is.na(region))

# ordenar
casos_region_2 <- casos_region |> 
  mutate(cut_region_orden = forcats::fct_relevel(as.character(cut_region),
                              "15", "1", "2", "3", "4", "5", "13", "6", "7", "16",
                              "8", "9", "14", "10", "11", "12")) |> 
  mutate(region_orden = as.numeric(cut_region_orden)) |> 
  mutate(region = stringr::str_wrap(region, 24),
         region = forcats::fct_reorder(region, region_orden)) |> 
  arrange(region) |> 
  mutate(region = forcats::fct_rev(region)) |> 
  slice(1:5)

etiqueta_pos = ifelse(casos_region_2$n < 20, 0, 1)
etiqueta_x = ifelse(casos_region_2$n < 20, 1, -1)
etiqueta_color = ifelse(casos_region_2$n < 20, color_principal, "white")
  

ancho_col = 0.6/16*length(casos_region_2$region)

casos_region_2 |> 
  ggplot(aes(x = n, y = region)) +
  geom_col(width = ancho_col,
           fill = color_principal) +
  geom_text(aes(label = n), 
            hjust = etiqueta_pos,
            nudge_x = etiqueta_x, 
            color = etiqueta_color) +
  scale_x_continuous(expand = expansion(c(0, 0))) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank()) +
  labs(y = NULL, x = "víctimas por región")


casos_region_2$region |> as.character() |>  dput()



mapa_comunas |> 
  filter(codigo_region == "05") |> 
  filter(!codigo_comuna %in% c(5104, 5201)) |>  #excluir islas
mutate(geometry = smoothr::drop_crumbs(geometry, threshold = units::set_units(30, km^2)))



# mapa region ----
mapa_datos |> 
  filter(codigo_region == "05") |> 
  ggplot() +
  # mapa de chile
  geom_sf(data = mapa_regiones,
          aes(geometry = geometry), 
          fill = color_principal, color = "white", alpha = 0.8) +
  # puntos
  geom_sf(aes(geometry = punto, 
              size = n),
          alpha = 0.6, color = color_negativo) +
  coord_sf(xlim = c(-76, -66.5), expand = FALSE) +
  # guides(size = guide_legend(position = "bottom")) +
  guides(size = guide_none()) +
  theme_void()
