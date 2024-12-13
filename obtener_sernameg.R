# web scraping de datos "oficiales" de femicidios desde el sitio web del Sernameg (Servicio Nacional de la Mujer y Equidad de Género), que lleva conteos de femicidios consumados y frustrados
library(rvest)
library(dplyr)
library(janitor)
library(tidyr)
library(stringr)


# scraping tabla ----
# https://www.sernameg.gob.cl/?page_id=27084

sitio <- session("https://www.sernameg.gob.cl/?page_id=27084") |> 
  read_html()

tablas <- sitio |> 
  html_table(convert = FALSE)

consumados_0 <- tablas[[1]] |> 
  row_to_names(2) |> 
  clean_names() |> 
  mutate(total = ifelse(region == "Total", 
                        "Total", NA)) |> 
  fill(total, .direction = "down") |> 
  filter(is.na(total)) |> 
  select(-total)

consumados_1 <- consumados_0 |> 
  # recodificar regiones
  mutate(region = str_remove(region, "^\\w+ ")) |>  # eliminar la primera palabra
  mutate(region = recode(region,
                         "O’Higgins" = "Libertador Gral. Bernardo O'Higgins",
                         "Metropolitana" = "Metropolitana de Santiago",
                         "Aysén" = "Aysén del General Carlos Ibáñez del Campo",
                         "Magallanes" = "Magallanes y de la Antártica Chilena"))

# pivotar a largo
consumados_2 <- consumados_1 |> 
  pivot_longer(cols = starts_with("x"),
               names_to = "año", 
               values_to = "n") |> 
  mutate(año = str_replace(año, "x", "")) |> 
  mutate(across(c(año, n), as.numeric))

consumados <- consumados_2 |> 
  rename(femicidios_consumados = n)

# guardar
readr::write_rds(consumados, "datos_sernameg/sernameg_femicidios_consumados.rds")
writexl::write_xlsx(consumados, "datos_sernameg/sernameg_femicidios_consumados.xlsx")



frustrados_0 <- tablas[[1]] |> 
  row_to_names(2) |> 
  clean_names() |> 
  mutate(total = ifelse(region == "Total", 
                        "Total", NA)) |> 
  fill(total, .direction = "down") |> 
  filter(!is.na(total)) |> 
  select(-total) |> 
  row_to_names(3) |> 
  clean_names() |> 
  filter(region != "Total")

frustrados_1 <- frustrados_0 |> 
  # recodificar regiones
  mutate(region = str_remove(region, "^\\w+ ")) |>  # eliminar la primera palabra
  mutate(region = recode(region,
                         "O’Higgins" = "Libertador Gral. Bernardo O'Higgins",
                         "Metropolitana" = "Metropolitana de Santiago",
                         "Aysén" = "Aysén del General Carlos Ibáñez del Campo",
                         "Magallanes" = "Magallanes y de la Antártica Chilena"))

# pivotar a largo
frustrados_2 <- frustrados_1 |> 
  pivot_longer(cols = starts_with("x"),
               names_to = "año", 
               values_to = "n") |> 
  mutate(año = str_replace(año, "x", "")) |> 
  mutate(across(c(año, n), as.numeric))

frustrados <- frustrados_2 |> 
  rename(femicidios_frustrados = n)

# guardar
readr::write_rds(frustrados, "datos_sernameg/sernameg_femicidios_frustrados.rds")
writexl::write_xlsx(frustrados, "datos_sernameg/sernameg_femicidios_frustrados.xlsx")




# datos abiertos ----
# https://datos.gob.cl/dataset/7161?comment=true

library(curl)

# descargar
curl_download("https://datos.gob.cl/dataset/81bcd6a4-7933-489d-9d85-059bf41ced96/resource/aa1f71c4-82ff-46b0-af81-cfd62eec34bb/download/vcm-femicidiossegunregion2008-2012.xlsx",
              "datos_sernameg/datos_2008-2012.xlsx")

curl_download("https://datos.gob.cl/dataset/81bcd6a4-7933-489d-9d85-059bf41ced96/resource/9c950def-c3f2-49ae-9046-6bda08bc09c9/download/vcm-femicidiossegunregion2013-2014.xlsx",
              "datos_sernameg/datos_2013-2014.xlsx")


# cargar
datos_1 <- readxl::read_excel("datos_sernameg/datos_2008-2012.xlsx")

datos_2 <- readxl::read_excel("datos_sernameg/datos_2013-2014.xlsx")

# son lo mismo que la tabla de sernameg, pero están desactualizados