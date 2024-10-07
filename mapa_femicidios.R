library(tidyverse)
library(chilemapas)
library(ggplot2)

color_fondo = "#262626"
color_detalle = "#757575" #"#262626" |> lighten(.4)
color_detalle_oscuro = "#303030" #color_detalle |> darken(.6)
color_na = "#4C4C4C" #color_fondo |> lighten(.2)
color_texto = "#A1A1A1" #color_fondo |> lighten(.6)
color_texto_claro = "#C6C6C6" #color_texto |> lighten(.4)
color_principal = "#f7d03a"
color_intermedio = "#A36F01"
color_secundario = "#332901" #color_principal |> darken(.8)
color_negativo = "#EB3737"
color_negativo_intermedio = "#EB802A"

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


# tipografías para ragg/sysfonts
library(showtext)
sysfonts::font_add_google("Archivo Narrow", "Archivo Narrow", db_cache = TRUE)
showtext_auto()

mapa_femicidios <- mapa_datos |> 
  ggplot() +
  # mapa de chile
  geom_sf(data = mapa_regiones,
          aes(geometry = geometry), 
          fill = color_principal, color = color_fondo, alpha = 1) +
  # puntos
  geom_sf(aes(geometry = punto, 
              size = n),
          alpha = 0.6, color = color_negativo) +
  coord_sf(xlim = c(-76*1.03, -66.5*0.97), expand = TRUE) +
  # guides(size = guide_legend(position = "bottom")) +
  guides(size = guide_none()) +
  theme_void() +
  list(theme(plot.background = element_rect(fill = color_fondo, linewidth = 0), 
             panel.background = element_rect(fill = color_fondo, linewidth = 0),
             panel.grid = element_blank(), 
             legend.background = element_rect(fill = color_fondo,linewidth = 0)
  )) +
  annotate("text", x = -78.5, y = -17.5, label = "Femicidios en Chile",
           color = color_principal, size = 10,
           family = "Archivo Narrow", fontface = "bold", hjust = 0) +
  annotate("text", x = -78.5, y = -18.5, 
           label = str_wrap("Visualización de datos del registro de femicidios realizado por la Red Chilena contra la Violencia hacia las Mujeres, desde 2010 en adelante.",
                            24),
           size = 8,
           lineheight = .3,
           color = color_detalle,
           family = "Archivo Narrow", face = "bold", hjust = 0, vjust = 1) +
  labs(caption = "Diseñado y programado por Bastián Olea Herrera.\nFuente de los datos: Red Chilena contra la Violencia hacia las Mujeres") +
  theme(plot.caption = element_text(family = "Archivo Narrow", 
                                  color = color_detalle, size = 14,
                                  lineheight = .3,
                                  margin = margin(t = -20, r = -6, b = 6)))
  # ggview::canvas(810, 2000, units = "px")

# install.packages("ggview")
# library(ggview)
mapa_femicidios

ggsave(mapa_femicidios, filename = "graficos/mapa_femicidios_chile.jpg",
       width = 810, height = 2000, units = "px")
       # width = 5, height = 8, scale = 0.3)

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
