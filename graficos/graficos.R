# library(shiny)
library(dplyr)
library(forcats)
library(ggplot2)
library(grid)
# library(thematic)
# library(fresh)
library(colorspace)
# library(colorjam)
# conflicted::conflicts_prefer(dplyr::count())
# conflicted::conflicts_prefer(dplyr::filter())

color_fondo = "#262626"
color_detalle = "#262626" |> lighten(.4)
color_texto = "#262626" |> lighten(.6)
color_principal = "#f7d03a"
color_intermedio = "#A36F01"
color_secundario = "#f7d03a" |> darken(.8)
color_negativo = "#EB3737"
color_negativo_intermedio = "#EB802A"

# cargar datos
femicidios <- arrow::read_parquet("datos/femicidios_chile_consolidado.parquet")

# temas
tema_fondo <- list(theme(plot.background = element_rect(fill = color_fondo, linewidth = 0), 
                         panel.background = element_rect(fill = color_fondo, linewidth = 0),
                         panel.grid = element_blank(), 
                         legend.background = element_rect(fill = color_fondo,linewidth = 0)
))

tema_texto <- list(theme(axis.text = element_text(color = color_texto),
                         text = element_text(color = color_texto),
                         axis.title = element_text(face = "bold"),
                         legend.title = element_text(face = "bold"),
                         )
                   )


gradiente_amarillo_rojo <- colorRampPalette(c(color_principal, color_negativo))
# hues::swatch(degradar_amarillo(6))

gradiente_sombra <- linearGradient(
  c(color_fondo, 
    NA, NA, NA, NA
    # fill_alpha(color_fondo, .5),
    # fill_alpha(color_fondo, 0)
    # blend_colors(c(rep(color_fondo, 4), NA)),
    # blend_colors(c(rep(color_fondo, 3), NA)),
    # blend_colors(c(rep(color_fondo, 2), NA)),
    # blend_colors(c(color_fondo, color_invisible)),
    # degradar_sombra(10),
    # color_invisible
    ), 
  x1 = unit(0, "npc"), y1 = unit(0, "npc"),
  x2 = unit(0, "npc"), y2 = unit(3, "npc")
)

degradar_amarillo <- colorRampPalette(c(color_fondo, color_fondo, color_principal), bias = 4)
# hues::swatch(degradar_amarillo(6))

gradiente_amarillo <- linearGradient(
  c(degradar_amarillo(10),
    color_principal), 
  x1 = unit(0, "npc"), y1 = unit(0, "npc"),
  x2 = unit(0, "npc"), y2 = unit(.8, "npc")
)

gradiente_amarillo_2 <- linearGradient(
  c(degradar_amarillo(10),
    color_principal), 
  x1 = unit(0, "npc"), y1 = unit(0, "npc"),
  x2 = unit(0, "npc"), y2 = unit(.4, "npc")
)

#—----

# area: casos con gradiente ----
femicidios |> 
  filter(año < 2024) |> 
  group_by(año) |> 
  summarize(victimas = n()) |>
  ggplot(aes(año, victimas)) +
  geom_area(fill = gradiente_amarillo) +
  geom_segment(aes(yend = 0, y = victimas + 5, xend = año), color = color_fondo, alpha = .2) +
  geom_text(aes(label = victimas, y = -3),
            color = color_texto |> lighten(.4), size = 4, fontface = "bold") +
  geom_line(stat = "smooth", method = "lm", color = color_fondo, linewidth = 2, alpha = .8, lineend = "round") +
  geom_line(stat = "smooth", method = "lm", color = color_negativo, linewidth = 1, alpha = .8) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(expand = expansion(c(0.01, 0)), breaks = seq(10, 70, by = 10)) +
  scale_x_continuous(breaks = 2010:2024, expand = expansion(0.01)) +
  theme_minimal() +
  theme(axis.line = element_blank(),
        axis.text.y = element_text(margin = margin(l = 4, r = 0)),
        axis.text.x = element_text(margin = margin(t = 1, b = 6))) +
  tema_fondo + tema_texto +
  labs(y = "Víctimas de femicidio", x = "Femicidios por año")


# area: casos por violencia sexual ----
femicidios |> 
  filter(año < 2024) |> 
  select(año, violencia_sexual) |> 
  mutate(violencia_sexual = case_when(violencia_sexual %in% c("Violencia sexual", "Presunta violencia sexual") ~ violencia_sexual,
                                      .default = "Otros casos"),
         violencia_sexual = fct_relevel(violencia_sexual, "Violencia sexual", "Presunta violencia sexual", "Otros casos")) |> 
  group_by(año, violencia_sexual) |> 
  summarize(victimas = n()) |>
  ggplot(aes(año, victimas)) +
  geom_col(aes(fill = violencia_sexual), width = 0.6) +
  geom_point(aes(color = violencia_sexual), size = NA) +
  geom_area(data = tibble(año = 2009:2024, victimas = 60), outline.type = "lower", color = color_fondo,
            fill = gradiente_sombra, alpha = 1) +
  geom_segment(data = tibble(victimas = seq(10, 60, by = 10)),
                 aes(x = 2009, xend = 2024, y = victimas), 
               color = color_fondo, alpha = .2) +
  scale_fill_manual(values = list(color_negativo,
                               color_negativo_intermedio,
                               gradiente_amarillo_2)) +
  scale_color_manual(values = c("Violencia sexual" = color_negativo,
                               "Presunta violencia sexual" = color_negativo_intermedio,
                               "Otros casos" = color_principal)) +
  scale_y_continuous(expand = expansion(c(0.01, 0.01)))+
  scale_x_continuous(breaks = 2010:2023,
                     expand = expansion(c(0, 0))) +
  theme_minimal() +
  theme(axis.line = element_blank()) +
  tema_fondo + tema_texto +
  labs(color = "Presencia de\nviolencia sexual", y = "Víctimas de femicidio por año", x = NULL) +
  guides(fill = guide_none(),
         color = guide_legend(override.aes = list(size = 4))) +
  theme(legend.position = "top") +
  theme(legend.title = element_text(hjust = 1),
        legend.text = element_text(margin = margin(l = 0, r = 6)),
        legend.margin = margin(t = 10, b = -10),
        plot.title = element_text(color = "grey80"),
        plot.subtitle = element_text(color = "grey80"),
        axis.text.y = element_text(margin = margin(l = 4, r = -12)),
        axis.title.x = element_text(margin = margin(t = 12, b = 0)))


# casos por categoría ----
femicidios |> 
  filter(año < 2024) |> 
  group_by(año) |> 
  mutate(categoria_femicidio_2 = fct_infreq(categoria_femicidio_2)) |> 
  dplyr::count(categoria_femicidio_2, .drop = F) |> 
  mutate(p = mean(n),
         v = p * 0.03) |> 
  ggplot(aes(año, n, fill = categoria_femicidio_2, col = categoria_femicidio_2)) +
  geom_col(width = 0.6, color = alpha(color_fondo, .2)) +
  geom_point(size = NA) +
  scale_y_continuous(expand = expansion(c(0, 0.1)))+
  scale_x_continuous(breaks = 2010:2023) +
  theme_minimal() +
  tema_fondo +
  tema_texto +
  theme(axis.line = element_line(linewidth = 0.5, color = color_secundario),
        axis.ticks = element_blank(),
        axis.text.y = element_text(margin = margin(l = 4, r = -4))) +
  labs(y = "Femicidios según categoría", n = NULL, color = "Categorías de femicidio") +
  guides(fill = guide_none(),
         color = guide_legend(override.aes = list(size = 4, fill = NA, linewidth = NA)))


#puntos diferencia edad ----
femicidios |> 
  select(id, fecha_femicidio, año, starts_with("edad")) |> 
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
  ggplot(aes(edad_femicida, edad_victima, color = diferencia)) +
  geom_point(aes(color = diferencia)) +
  # geom_vline(xintercept = 18, color = color_negativo, linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = 18, color = color_negativo, linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = c(rev(gradiente_amarillo_rojo(4)), color_detalle)) + 
  scale_y_continuous(expand = expansion(0.01), 
                     breaks = c(18, 25, 35, 45, 55, 65, 75, 85)) +
  scale_x_continuous(breaks = c(18, 25, 35, 45, 55, 65, 75, 85)) +
  tema_fondo +
  tema_texto +
  labs(title = "Femicidios en Chile",
       subtitle = "cada punto corresponde a un femicidio, donde su altura indica la edad de la víctima, \ny el color de los puntos indica la diferencia de edad entre ella y el femicida",
       y = "Edad de la víctima al momento del femicidio",
       x = "Edad del femicida al ejecutar el crimen", 
       color = "Diferencia de edad\nentre femicida y víctima") +
  theme(panel.grid.major = element_line(color = color_detalle |> darken(.6))) +
  theme(legend.title = element_text(hjust = 1),
        legend.text = element_text(margin = margin(l = 4, r = 0)),
        legend.margin = margin(l = -20),
        plot.title = element_text(color = "grey80"),
        plot.subtitle = element_text(color = "grey80"),
        axis.title.x = element_text(margin = margin(t = 12, b = 0))) +
  guides(color = guide_legend(override.aes = list(size = 4)))


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
  scale_y_continuous(expand = expansion(0.01), 
                     breaks = c(18, 25, 35, 45, 55, 65, 75, 85)) +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
  tema_fondo + tema_texto +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  labs(title = "Femicidios en Chile",
       subtitle = "cada punto corresponde a un femicidio, donde su altura indica la edad de la víctima, \ny el color de los puntos indica la diferencia de edad entre ella y el femicida",
       y = "Edad de la víctima al momento del femicidio",
       x = "Año del femicidio", 
       color = "Diferencia de edad\nentre femicida y víctima") +
  theme(panel.grid.major.y = element_line(color = color_detalle |> darken(.6))) +
  theme(legend.position = "top") +
  theme(legend.title = element_text(hjust = 1),
        legend.text = element_text(margin = margin(r = 6)),
        legend.margin = margin(t = 10, b = -10),
        plot.title = element_text(color = "grey80"),
        plot.subtitle = element_text(color = "grey80"),
        axis.title.x = element_text(margin = margin(t = 12, b = 0)))

# puntos: edad del femicida ----
femicidios |> 
  ggplot(aes(fecha_femicidio, edad_femicida)) +
  geom_point(color = color_principal) +
  geom_hline(yintercept = 18, color = color_principal) +
  geom_hline(aes(yintercept = median(edad_femicida, na.rm = T)), 
             color = "red", linewidth = 2) +
  scale_x_date(date_breaks = "years", date_labels = "%Y") +
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
  geom_text(aes(label = round(edad_victima, 1), y = edad_victima), 
            size = 3, alpha = .7, color = color_principal, nudge_y = -0.5) +
  geom_text(aes(label = round(edad_femicida, 1), y = edad_femicida), 
            size = 3, alpha = .7, color = color_negativo, nudge_y = +0.5) +
  scale_x_continuous(breaks = 2010:2024) +
  tema_fondo +
  tema_texto +
  labs(y = "Edad del femicida en relación con edad de la víctima", x = NULL)



femicidios |> 
  count(forma_de_agresion)

femicidios |> 
  count(lugar, comuna, region) |> 
  arrange(desc(n))

femicidios |> 
  count(region) |> 
  arrange(desc(n))

femicidios |> 
  count(lugar) |> 
  arrange(desc(n))

femicidios |> 
  count(violencia_sexual)

femicidios |> 
  glimpse()


femicidios |> 
  count(categoria_femicidio)

femicidios |> 
  count(categoria_femicidio_2)


femicidios |> 
  count(antecedentes_ley_vif) |> 
  print(n=Inf)
