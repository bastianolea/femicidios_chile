# library(shiny)
library(dplyr)
library(forcats)
library(ggplot2)
library(grid)
library(thematic)
library(fresh)
library(colorspace)
library(colorjam)

color_fondo = "#262626"
color_detalle = "#262626" |> lighten(.4)
color_texto = "white"
color_principal = "#f7d03a"
color_intermedio = "#A36F01"
color_secundario = "#f7d03a" |> darken(.8)
color_negativo = "#EB3737"

femicidios <- arrow::read_parquet("datos/femicidios_chile_consolidado.parquet")


tema_fondo <- list(theme(plot.background = element_rect(fill = color_fondo, linewidth = 0), 
                         panel.background = element_rect(fill = color_fondo, linewidth = 0),
                         panel.grid = element_blank(), 
                         legend.background = element_rect(fill = color_fondo)
))

tema_texto <- list(theme(axis.text = element_text(color = color_detalle),
                         text = element_text(color = color_detalle)))

# gradiente_amarillo <- linearGradient(c(color_principal, color_secundario), group = FALSE)

gradiente_amarillo <- linearGradient(
  c(color_fondo, 
    blend_colors(c(rep(color_fondo, 3), color_principal)),
    blend_colors(c(rep(color_fondo, 2), color_principal)),
    blend_colors(c(color_fondo, color_principal)),
    blend_colors(c(color_fondo, rep(color_principal, 2))),
    # darken(color_principal, .8), darken(color_principal, .7),
    # darken(color_principal, .5), darken(color_principal, .3), darken(color_principal, .2),
    color_principal), 
  x1 = unit(0, "npc"), y1 = unit(0, "npc"),
  x2 = unit(0, "npc"), y2 = unit(.7, "npc")
)

gradiente_amarillo_rojo <- colorRampPalette(c(color_principal, color_negativo))

# casos con gradiente ----
femicidios |> 
  group_by(año) |> 
  summarize(victimas = n()) |>
  mutate(p = mean(victimas),
         v = p * 0.03) |> 
  ggplot(aes(año, victimas)) +
  geom_area(fill = gradiente_amarillo) +
  geom_segment(aes(yend = 0, y = victimas + 10, xend = año), color = color_fondo, alpha = .2) +
  # geom_text(aes(label = victimas, y = victimas + v), vjust = 0) +
  scale_y_continuous(expand = expansion(c(0, 0.1)))+
  scale_x_continuous(breaks = 2010:2024, expand = expansion(0)) +
  theme_minimal() +
  theme(axis.line = element_blank()) +
  tema_fondo +
  tema_texto


# casos por categoría ----
femicidios |> 
  group_by(año) |> 
  mutate(categoria_femicidio_2 = fct_infreq(categoria_femicidio_2)) |> 
  dplyr::count(categoria_femicidio_2, .drop = F) |> 
  mutate(p = mean(n),
         v = p * 0.03) |> 
  ggplot(aes(año, n, fill = categoria_femicidio_2)) +
  geom_col() +
  # geom_segment(aes(yend = 0, xend = año), color = color_fondo, alpha = .2) +
  # geom_line(color = color_fondo, linewidth = 2) +
  # geom_line(color = color_secundario, linewidth = 0.5) +
  # geom_text(aes(label = victimas, y = victimas + v), vjust = 0) +
  scale_y_continuous(expand = expansion(c(0, 0.1)))+
  scale_x_continuous(breaks = 2010:2024) +
  theme_minimal() +
  tema_fondo +
  tema_texto +
  theme(axis.line = element_line(linewidth = 0.5, color = color_secundario),
        axis.ticks = element_blank())


#puntos diferencia edad ----
femicidios |> 
  select(id, fecha_femicidio, año, starts_with("edad")) |> 
  # mutate(victima_menor = if_else(edad_victima < 18, "Menor de edad", "Mayor de edad")) |> 
  mutate(diferencia = edad_femicida - edad_victima) |> 
  ggplot(aes(edad_femicida, edad_victima)) +
  geom_point(aes(color = diferencia)) +
  geom_vline(xintercept = 18) +
  geom_hline(yintercept = 18) +
  # geom_smooth()
  # scale_color_brewer(type = "div", palette = "Spectral")
  scale_color_continuous_diverging(palette = "Tropic") +
  tema_fondo +
  tema_texto

# 
# femicidios |> 
#   mutate(victima_menor = if_else(edad_victima < 18, "Menor de edad", "Mayor de edad")) |>
#   ggplot(aes(fecha_femicidio, edad_victima)) +
#   geom_point(aes(color = victima_menor)) +
#   tema_fondo +
#   tema_texto

# puntos: diferencia de edad ----
femicidios |> 
  # mutate(victima_menor = if_else(edad_victima < 18, "Menor de edad", "Mayor de edad")) |>
  # mutate(diferencia = edad_femicida - edad_victima) |>
  mutate(diferencia = case_when(edad_femicida - edad_victima >= 20 ~ "Más de 20",
                                edad_femicida - edad_victima >= 15 ~ "Más de 15",
                                edad_femicida - edad_victima >= 10 ~ "Más de 10",
                                edad_femicida - edad_victima >= 5  ~ "Más de 5",
                                .default = "Menos de 5")) |>
  mutate(diferencia_hay = if_else(diferencia == "Menos de 5", "No", "Sí")) |> 
  arrange(fecha_femicidio, edad_victima) |> 
  mutate(diferencia = fct_relevel(diferencia, "Más de 20",
                                  "Más de 15",
                                  "Más de 10",
                                  "Más de 5")) |> 
  ggplot(aes(fecha_femicidio, edad_victima)) +
  geom_hline(yintercept = 18, color = color_negativo, linetype = "dashed", linewidth = 1) +
  geom_point(aes(color = diferencia)) +
  scale_color_manual(values = c(rev(gradiente_amarillo_rojo(4)), color_detalle)) + 
  scale_y_continuous(expand = expansion(0.01)) +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  tema_fondo + tema_texto +
  guides(color = guide_legend(override.aes = list(size = 3)))

# puntos: edad del femicida ----
femicidios |> 
  ggplot(aes(fecha_femicidio, edad_femicida)) +
  geom_point(color = color_principal) +
  geom_hline(yintercept = 18, color = color_principal) +
  geom_hline(aes(yintercept = median(edad_femicida, na.rm = T)), 
             color = "red", linewidth = 2) +
  tema_fondo +
  tema_texto


# pasos: diferencias de edad promedio ----
femicidios |> 
  group_by(año) |> 
  summarize(edad_femicida = mean(edad_femicida, na.rm = T),
            edad_victima = mean(edad_victima, na.rm = T)) |> 
  ggplot(aes(x = año)) +
  geom_segment(aes(y = edad_victima, yend = edad_femicida, xend = año), color = color_detalle) +
  geom_step(aes(y = edad_victima), color = color_principal, linewidth = 2, direction = "mid") +
  geom_step(aes(y = edad_femicida), color = color_negativo, linewidth = 2, direction = "mid") +
  geom_text(aes(label = round(edad_victima, 1), y = edad_victima), size = 3, color = color_principal, nudge_y = -0.5) +
  geom_text(aes(label = round(edad_femicida, 1), y = edad_femicida), size = 3, color = color_negativo, nudge_y = +0.5) +
  tema_fondo +
  tema_texto
