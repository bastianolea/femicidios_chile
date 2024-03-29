suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(forcats))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gt))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(bslib))
suppressPackageStartupMessages(library(htmltools))
suppressPackageStartupMessages(library(shinycssloaders))
suppressPackageStartupMessages(library(glue))

# datos ----
femicidios <- arrow::read_parquet("datos/femicidios_chile_consolidado.parquet")

# colores ----
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


gradiente_amarillo_rojo <- colorRampPalette(c(color_principal, color_negativo))
# hues::swatch(degradar_amarillo(6))

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

# gradiente_sombra <- linearGradient(
#   c(color_fondo, 
#     NA, NA, NA, NA), 
#   x1 = unit(0, "npc"), y1 = unit(0, "npc"),
#   x2 = unit(0, "npc"), y2 = unit(3, "npc")
# )


# temas ----
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


options(spinner.type = 8, spinner.color = color_principal)

# thematic_shiny(bg = color_fondo, fg = color_texto, accent = color_secundario)


ui <- fluidPage(
  title = "Femicidios en Chile", lang = "es",
  
  theme = bs_theme(
    bg = color_fondo, fg = color_texto, primary = color_principal, 
    base_font = font_link(
      "Archivo Narrow",
      href = "https://fonts.googleapis.com/css2?family=Archivo+Narrow:ital,wght@0,400..700;1,400..700&display=swap"
    )
  ),
  # 
  # use_theme(create_theme(
  #   theme = "default",
  #   bs_vars_input(bg = color_fondo),
  #   bs_vars_global(body_bg = color_fondo, 
  #                  text_color = color_texto, 
  #                  link_color = color_texto,
  #                  border_radius_base = "6px"),
  #   bs_vars_font(size_base = "14px", #aumentar globalmente tamaño de letra  
  #                family_sans_serif = "Open Sans" #cargar fuente o tipo de letra
  #   ),
  #   bs_vars_button(
  #     default_color = color_fondo,
  #     default_bg = color_secundario,
  #     default_border = color_fondo,
  #     border_radius_base = "6px"
  #   )
  # )),
  
  ## css ----
  
  tags$style(
    glue("h3 {
      color: {{color_principal}};
    }",
    .open = "{{", .close = "}}")
  ),
  
  
  
  ## header ----
  fluidRow(
    column(12,
           style = css(margin_bottom = "16px"),
           
           h1("Femicidios en Chile", 
              style = css(color = color_principal,
                          margin_top = "20px")
           ),
           
           markdown("Visualización de datos del [registro de femicidios](http://www.nomasviolenciacontramujeres.cl/registro-de-femicidios/) realizado por la [Red Chilena contra la Violencia hacia las Mujeres](www.nomasviolenciacontramujeres.cl) desde 2010 en adelante.")
           
    )
  ),
  
  
  # cuerpo ----
  tabsetPanel(type = "pills",
              header = div(style = css(margin_bottom = "28px")),
              
              tabPanel("Gráficos",
                       
                       ## gráficos ----
                       
                       fluidRow(
                         column(12,
                                h3("Casos anuales de femicidios"),
                                plotOutput("grafico_historico") |> withSpinner(),
                                hr()
                         )
                       ),
                       
                       
                       fluidRow(
                         column(12,
                                h3("Femicidios anuales especificando violencia sexual"),
                                plotOutput("barras_violencia_sexual") |> withSpinner(),
                                hr()
                         )
                       ),
                       
                       fluidRow(
                         column(12,
                                h3("Femicidios por categoría del crimen"),
                                plotOutput("barras_categoria") |> withSpinner(),
                                hr()
                         )
                       ),
                       
                       
                       fluidRow(
                         column(12,
                                h3("Femicidios por edad de la víctima y edad del femicida"),
                                markdown("En este gráfico, cada punto corresponde a un femicidio, donde su altura indica la edad de la víctima, \ny el color de los puntos indica la diferencia de edad entre ella y el femicida"),
                                plotOutput("puntos_edad_1") |> withSpinner(),
                                hr()
                         )
                       ),
                       
                       fluidRow(
                         column(12,
                                h3("Femicidios por edad de la víctima y año del femicidio"),
                                markdown("Cada punto corresponde a un femicidio, donde su altura indica la edad de la víctima, y el color de los puntos indica la diferencia de edad entre ella y el femicida"),
                                plotOutput("puntos_edad_2") |> withSpinner(),
                                hr()
                         )
                       ),
                       
                       fluidRow(
                         column(12,
                                h3("Edades de femicidas, por año"),
                                plotOutput("puntos_edad_femicida") |> withSpinner(),
                                hr()
                         )
                       ),
                       
                       fluidRow(
                         column(12,
                                h3("Edades promedio de femicidas y víctimas, por año"),
                                plotOutput("lineas_edad_1") |> withSpinner()
                         )
                       )
                       
              ),
              
              
              
              
              ## tabla ----
              tabPanel("Tablas",
                       fluidRow(
                         column(12, style = css(max_height = "2000px",
                                                overflow_y = "scroll"),
                                
                                h3("Detalle de casos de femicidio por año"),
                                
                                selectInput("tabla_año", label = "Seleccionar año", choices = 2024:2010, selected = 2023),
                                
                                gt_output("tabla_general") |> withSpinner()
                                
                         )
                       )
              ),
              
              
              ## descargar ----
              tabPanel("Datos",
                       fluidRow(
                         column(12,
                                # br(),
                                # hr(),
                                h2("Descargar datos"),
                                br(),
                                
                                markdown("Descargar datos de femicidios procesados y limpiados, a partir de los datos originales compilados y mantenidos por la [Red Chilena contra la Violencia hacia las Mujeres](http://www.nomasviolenciacontramujeres.cl/registro-de-femicidios/)."),
                                markdown("La diferencia de este dataset con los datos originales es la mayoría de las variables categóricas han sido limpiadas y en varios casos simplificadas, y los nombres de las variables han sido estandarizados, permitiendo unir en un solo archivo los registros desde 2010 hasta 2024."),
                                # downloadButton("descargar_2024", label = "Datos femicidios 2024", 
                                #                style = css(background_color = color_principal)
                                # ),
                                
                                div(style = css(margin_top = "20px"),
                                    downloadButton("descargar_todo", label = "Descargar datos consolidados", 
                                                   style = css(background_color = color_principal)
                                    )
                                )
                         )
                       )
              )
  ),
  
  ## firma ----
  fluidRow(
    column(12, style = "opacity: 1; font-size: 80%;",
           hr(),
           
           markdown("Diseñado y programado por [Bastián Olea Herrera.](https://bastian.olea.biz) en [R](https://www.tidyverse.org) y [Shiny](https://shiny.posit.co)"),
           
           markdown("Puedes explorar mis otras [aplicaciones interactivas sobre datos sociales en mi portafolio.](https://bastianolea.github.io/shiny_apps/)"),
           
           markdown("Fuente de los datos: [registro de femicidios](http://www.nomasviolenciacontramujeres.cl/registro-de-femicidios/) realizado por la [Red Chilena contra la Violencia hacia las Mujeres](www.nomasviolenciacontramujeres.cl)"),
           
           markdown("Código de fuente de esta app y de la obtención de los datos [disponibles en GitHub.](https://github.com/bastianolea/femicidios_chile)"),
           
           div(style = "height: 20px")
           
    )
  )
  
)


server <- function(input, output) {
  
  ## datos ----
  femicidios_anual <- reactive({
    femicidios |> 
      filter(año < 2024) |> 
      group_by(año) |> 
      summarize(victimas = n())
  })
  
  ## gráfico area principal ----
  output$grafico_historico <- renderPlot({
    femicidios_anual() |>
      ggplot(aes(año, victimas)) +
      geom_area(fill = gradiente_amarillo) +
      geom_segment(aes(yend = 0, y = victimas + 5, xend = año), color = color_fondo, alpha = .2) +
      geom_text(aes(label = victimas, y = -3),
                color = color_texto_claro, size = 4, fontface = "bold") +
      # geom_line(stat = "smooth", method = "lm", color = color_fondo, linewidth = 2, alpha = .8, lineend = "round") +
      # geom_line(stat = "smooth", method = "lm", color = color_negativo, linewidth = 1, alpha = .8) +
      coord_cartesian(clip = "off") +
      scale_y_continuous(expand = expansion(c(0.01, 0)), breaks = seq(10, 70, by = 10)) +
      scale_x_continuous(breaks = 2010:2024, expand = expansion(0.01)) +
      theme_minimal() +
      theme(axis.line = element_blank(),
            axis.text.y = element_text(margin = margin(l = 4, r = 0)),
            axis.text.x = element_text(margin = margin(t = 1, b = 6))) +
      tema_fondo + tema_texto +
      labs(y = "Víctimas de femicidio", x = "Femicidios por año")
  }) |> 
    bindCache()
  
  ## barras violencia sexual ----
  output$barras_violencia_sexual <- renderPlot({
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
      # geom_area(data = tibble(año = 2009:2024, victimas = 60), outline.type = "lower", color = color_fondo,
      #           fill = gradiente_sombra, alpha = 1) +
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
  }) |> 
    bindCache()
  
  ## barras categoría ----
  output$barras_categoria <- renderPlot({
    femicidios |>
      filter(año < 2024) |> 
      group_by(año) |> 
      mutate(categoria_femicidio_2 = fct_infreq(categoria_femicidio_2)) |> 
      count(categoria_femicidio_2, .drop = F) |> 
      mutate(categoria_femicidio_2 = fct_relevel(categoria_femicidio_2, "Femicidio íntimo", 
                                                 after = 99)) |> 
      ggplot(aes(año, n, fill = categoria_femicidio_2, col = categoria_femicidio_2)) +
      geom_col(width = 0.6, color = alpha(color_fondo, .2)) +
      geom_point(size = NA) +
      # geom_area(data = tibble(año = 2009:2024, n = 60), outline.type = "lower", color = color_fondo,
      #           fill = gradiente_sombra, alpha = 1) +
      scale_fill_brewer(palette = "Dark2") +
      scale_color_brewer(palette = "Dark2") +
      scale_y_continuous(expand = expansion(c(0, 0.1)))+
      scale_x_continuous(breaks = 2010:2023,
                         expand = expansion(c(0, 0))) +
      theme_minimal() +
      tema_fondo +
      tema_texto +
      theme(axis.text.y = element_text(margin = margin(l = 4, r = -10))) +
      labs(y = "Femicidios según categoría", n = NULL, color = "Categorías de femicidio") +
      guides(fill = guide_none(),
             color = guide_legend(override.aes = list(size = 4, fill = NA, linewidth = NA)))
    
  }) |> 
    bindCache()
  
  ## edad ----
  femicidios_edad <- reactive({
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
                                      "Más de 5"))
  })
  
  ### puntos 1 ----
  output$puntos_edad_1 <- renderPlot({
    femicidios_edad() |> 
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
      labs(y = "Edad de la víctima al momento del femicidio",
           x = "Edad del femicida al ejecutar el crimen", 
           color = "Diferencia de edad\nentre femicida y víctima") +
      theme(panel.grid.major = element_line(color = color_detalle_oscuro)) +
      theme(legend.title = element_text(hjust = 1),
            legend.text = element_text(margin = margin(l = 4, r = 0)),
            legend.margin = margin(l = -20),
            plot.title = element_text(color = "grey80"),
            plot.subtitle = element_text(color = "grey80"),
            axis.title.x = element_text(margin = margin(t = 12, b = 0))) +
      guides(color = guide_legend(override.aes = list(size = 4)))
  }) |> 
    bindCache()
  
  
  ### puntos 2 ----
  output$puntos_edad_2 <- renderPlot({
    femicidios_edad() |>
      ggplot(aes(fecha_femicidio, edad_victima)) +
      geom_hline(yintercept = 18, color = color_negativo, linetype = "dashed", linewidth = 1) +
      geom_point(aes(color = diferencia)) +
      scale_color_manual(values = c(rev(gradiente_amarillo_rojo(4)), color_detalle)) + 
      scale_y_continuous(expand = expansion(0.01), 
                         breaks = c(18, 25, 35, 45, 55, 65, 75, 85)) +
      scale_x_date(date_breaks = "years", date_labels = "%Y") +
      tema_fondo + tema_texto +
      guides(color = guide_legend(override.aes = list(size = 4))) +
      labs(y = "Edad de la víctima al momento del femicidio",
           x = "Año del femicidio", 
           color = "Diferencia de edad\nentre femicida y víctima") +
      theme(panel.grid.major.y = element_line(color = color_detalle_oscuro)) +
      theme(legend.position = "top") +
      theme(legend.title = element_text(hjust = 1),
            legend.text = element_text(margin = margin(r = 6)),
            legend.margin = margin(t = 10, b = -10),
            plot.title = element_text(color = "grey80"),
            plot.subtitle = element_text(color = "grey80"),
            axis.title.x = element_text(margin = margin(t = 12, b = 0)))
  }) |> 
    bindCache()
  
  
  ### puntos femicida ----
  output$puntos_edad_femicida <- renderPlot({
    femicidios |> 
      ggplot(aes(fecha_femicidio, edad_femicida)) +
      geom_hline(yintercept = 18, color = color_negativo, linetype = "dashed", linewidth = 1) +
      geom_hline(aes(yintercept = median(edad_femicida, na.rm = T)), 
                 color = color_negativo, linewidth = 2) +
      geom_point(color = color_principal, size = 3, alpha = .6) +
      annotate("label", x = as.Date("2023-12-01"), y = 37, label = "promedio", 
               fill = color_fondo, color = color_negativo, label.size = 0) +
      scale_x_date(date_breaks = "years", date_labels = "%Y") +
      scale_y_continuous(expand = expansion(0.01), 
                         breaks = c(18, 25, 35, 45, 55, 65, 75, 85)) +
      tema_fondo +
      tema_texto
  }) |> 
    bindCache()
  
  ### lineas diferencia edad ----
  output$lineas_edad_1 <- renderPlot({
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
  }) |> 
    bindCache()
  
  
  
  
  ## tablas ----
  output$tabla_general <- render_gt({
    
    datos <- femicidios |> 
      filter(año == input$tabla_año) |> 
      arrange(desc(id)) |> 
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
      mutate(across(where(is.character), ~tidyr::replace_na(.x, "Sin información")))
    
    datos |> 
      gt() |> 
      cols_align(columns = where(is.numeric), 
                 align = "center") |> 
      tab_style(locations = cells_column_labels(),
                style = cell_text(weight = "bold")) |> 
      tab_style(locations = cells_body(columns = nombre_victima),
                style = list(cell_text(color = color_principal, weight = "bold"))) |> 
      tab_style(locations = cells_body(columns = nombre_femicida),
                style = list(cell_text(color = color_negativo))) |> 
      #color edad
      data_color(
        columns = edad_victima,
        method = "numeric", domain = c(0, 100), apply_to = "text",
        palette = c(color_negativo, color_texto, color_negativo)) |> 
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
                locations = cells_body(columns = tipificacion_penal, rows = tipificacion_penal == "Sin información")
      ) |> 
      #missing numéricos 
      tab_style(style = cell_text(color = color_fondo),
                locations = cells_body(columns = edad_femicida, rows = is.na(edad_femicida))) |> 
      tab_style(style = cell_text(color = color_fondo),
                locations = cells_body(columns = edad_victima, rows = is.na(edad_victima))) |> 
      #formato fecha
      fmt_date(columns = fecha_femicidio,
               date_style = "day_m_year",
               locale = "es-CL") |> 
      #nombres de columnas
      cols_label(
        id = "N°",
        nombre_victima = "Nombre de la víctima",
        fecha_femicidio = "Fecha",
        edad_victima = "Edad",
        comuna = "Comuna", region = "Región",
        categoria_femicidio = "Categoría", 
        forma_de_agresion = "Agresión", 
        violencia_sexual = "Violencia sexual", 
        relacion_victima_femicida = "Relación víctima/femicida", 
        antecedentes_ley_vif = "Antecedentes VIF",
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
  })
  
  
  ## descargas ----
  
  output$descargar_todo <- downloadHandler(
    filename = function() {
      "femicidios_chile_consolidado.xlsx"
    },
    content = function(file) {
      file.copy("datos/femicidios_chile_consolidado.xlsx", file)
    }
  )
}


shinyApp(ui = ui, server = server)
