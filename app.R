library(shiny)
library(dplyr)
library(ggplot2)
library(gt)
library(thematic)
library(bslib)

color_fondo = "black"
color_texto = "white"
color_secundario = "#f7d03a"
color_destacado = color_secundario
color_detalle = "grey50"
color_rojo = "red"

femicidios <- arrow::read_parquet("datos/femicidios_chile_consolidado.parquet")


options(spinner.type = 4, spinner.color = color_secundario)

thematic_shiny(bg = color_fondo, fg = color_texto, accent = color_secundario)


ui <- fluidPage(title = "Femicidios en Chile", lang = "es",
                
                theme = bs_theme(
                  bg = color_fondo, fg = color_texto, primary = color_destacado,
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
                
                h1("Femicidios"),
                
                plotOutput("grafico_historico"),
                
                gt_output("tabla_general")
                
)


server <- function(input, output) {
  
  femicidios_anual <- reactive({
    femicidios |> 
      group_by(año) |> 
      summarize(victimas = n())
  })
  
  output$grafico_historico <- renderPlot({
    # femicidios_anual() |> 
    femicidios |> 
      group_by(año) |> 
      summarize(victimas = n()) |>
      mutate(p = mean(victimas),
             v = p * 0.03) |> 
      ggplot(aes(año, victimas)) +
      geom_area(fill = color_secundario) +
      geom_segment(aes(yend = 0, xend = año), color = color_fondo, alpha = .2) +
      geom_line(color = color_fondo, linewidth = 2) +
      geom_line(color = color_secundario, linewidth = 0.5) +
      geom_text(aes(label = victimas, y = victimas + v), vjust = 0) +
      scale_y_continuous(expand = expansion(c(0, 0.1)))+
      scale_x_continuous(breaks = 2010:2024) +
      theme(axis.line = element_line(linewidth = 0.5, color = color_secundario))
  })
  
  output$tabla_general <- render_gt({
    femicidios |> 
      arrange(desc(fecha_femicidio)) |> 
      slice(1:100) |> 
      mutate(across(where(is.character), ~tidyr::replace_na(.x, " "))) |> 
      select(-año) |> 
      gt() |> 
      cols_align(columns = where(is.numeric), 
                 align = "center") |> 
      tab_style(locations = cells_column_labels(),
                style = cell_text(weight = "bold")) |> 
      tab_style(locations = cells_body(columns = nombre_victima),
                style = list(
                  cell_text(color = color_secundario, weight = "bold"))) |> 
      tab_style(locations = cells_body(columns = nombre_femicida),
                style = list(
                  cell_text(color = color_rojo))) |> 
      #color edad
      data_color(
        columns = edad_victima,
        method = "numeric", domain = c(0, 100), apply_to = "text",
        palette = c(color_rojo, color_texto, color_rojo)
      ) |> 
      # #color sector político
      # data_color(columns = c(sector), 
      #            method = "factor", apply_to = "fill",
      #            levels = c("Izquierda", "Derecha", "Ninguno"), 
      #            palette = c(color_izquierda, color_derecha, color_fondo)) |> 
      # data_color(columns = c(sector), 
      #            method = "factor", apply_to = "text",
      #            levels = c("Izquierda", "Derecha", "Ninguno"), 
      #            palette = c("white", "white", color_texto)) |> 
      # #color partido
      # data_color(columns = c(partido, sector), 
      #            method = "factor", apply_to = "text",
      #            levels = c("Ninguno"),
      #            palette = color_detalle2, na_color = color_texto) |> 
      # #color otros
      # data_color(columns = c(perjudicado), 
      #            method = "factor", apply_to = "text",
      #            levels = c("Otros"),
      #            palette = color_detalle2, na_color = color_texto) |> 
      # fmt_number(columns = monto, sep_mark = ".", decimals = 0) |> 
    #fecha
    fmt_date(
      columns = fecha_femicidio,
      date_style = "day_m_year",
      locale = "es-CL"
    ) |> 
      cols_label(
        id = "N°",
        nombre_victima = "Nombre de la víctima",
        fecha_femicidio = "Fecha"
      ) |> 
      tab_options(table.font.color = color_texto, table.font.color.light = color_texto,
                  table_body.hlines.color = color_detalle,
                  table_body.vlines.color = color_detalle, 
                  column_labels.border.top.color = color_fondo, column_labels.border.bottom.color = color_detalle, 
                  table_body.border.bottom.color = color_detalle,
                  table.background.color = color_fondo, 
                  table.font.names = "Archivo Narrow")
  })
}


shinyApp(ui = ui, server = server)
