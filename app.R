library(shiny)

femicidios <- arrow::read_parquet("datos/femicidios_chile_consolidado.parquet")

ui <- fluidPage(title = "Femicidios en Chile", lang = "es",

)


server <- function(input, output) {

}


shinyApp(ui = ui, server = server)
