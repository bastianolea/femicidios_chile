library(chilemapas)
library(ggplot2)

color_principal = "#f7d03a"
color_intermedio = "#A36F01"
color_secundario = "#332901" 
color_negativo = "#EB3737"

# mapas ----
# cargar mapa
mapa_comunas <- chilemapas::mapa_comunas %>%
  left_join(
    chilemapas::codigos_territoriales %>%
      select(matches("comuna"))) |> 
  mutate(codigo_comuna = as.numeric(codigo_comuna))

# mapa de regiones
mapa_regiones <- mapa_comunas |> 
  group_by(codigo_region) |> 
  summarize(geometry = st_union(geometry))


# datos ----
casos_comuna <- datos_6 |> 
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
  geom_sf(data = mapa_regiones,
          aes(geometry = geometry), 
          fill = color_principal, color = "white") +
  # puntos
  geom_sf(aes(geometry = punto, 
              size = n),
          alpha = 0.6, color = color_negativo) +
  coord_sf(xlim = c(-76, -66.5)) +
  guides(size = guide_legend(position = "bottom")) +
  theme_void()