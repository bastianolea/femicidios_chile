library(tidyr)

# fechas <- map(datos[[2]], ~{
#   if (class(.x)[1] == "POSIXct") {
#     return(.x)
#   } else {
#     return(NULL)
#   }
# })
# fechas_2 <- fechas |> unlist() |> as_datetime()
# planillas[[10]]
          
planillas_limpias <- map(planillas, ~{
  # .x = planillas[[12]] #tiene texto escrito despues de unas fechas
  # .x = planillas[[10]]
  
  if (is.null(.x)) return(tibble())
  
  .x |>
    clean_names() |> 
    filter(!is.na(x5)) |> #columna que posiblemente es el nombre de la víctima
    row_to_names(1) |> 
    clean_names() |> 
    rename(id = 1, fecha = 2) |> #supuestamente el numero de la victima del año
    filter(!is.na(id)) |> 
    # mutate(fecha = map(fecha, as.character)) |> #columna fechas viene en multiples formatos
    # unnest(fecha) 
    mutate(across(where(is.list), ~map(.x, as.character))) |> #convertir todas las columnas de multiples formatos a caracter
    unnest(where(is.list), keep_empty = T) |> 
    select(-starts_with("na_"))
})


# casos por año
map_int(planillas_limpias, nrow)


# guardar planillas individuales en excel ---- 
walk(planillas_limpias, ~{
  if (nrow(.x) > 1) {
    año <- .x |> select(fecha) |> pull() |> ymd() |> min(na.rm = T) |> year()
    writexl::write_xlsx(.x, glue("datos/femicidios_{año}.xlsx"))
  }
})

# guardar archivo consolidado
# planillas_limpias |> list_rbind()


# cargar todos ----
library(fs)

archivos <- dir_ls("datos")

datos <- map(archivos, ~{
  readxl::read_xlsx(.x) #|> 
  # janitor::row_to_names(2)
  })

datos
