library(tidyverse)
library(terra)
library(tidyterra)
library(readxl)
library(glue)

estacional <- function(x) {
  
  x = as.Date(x)
  
  estaciones <- c('Verano','Otoño','Invierno','Primavera')
  
  año = year(x)
  mes = month(x)
  
  año <- ifelse(mes == 12,año+1,año)
  
  estacion <- factor(case_when(
    mes %in% c(12, 1, 2) ~ "Verano",
    mes %in% c(3, 4, 5) ~ "Otoño",
    mes %in% c(6, 7, 8) ~ "Invierno",
    mes %in% c(9, 10, 11) ~ "Primavera"), levels=estaciones)
  
  return(tibble(año = año, estacion = estacion))
}

cob <- rast(list.files('data/processed/raster/cobertura/biomas/',full.names=T)[1])
shac <- vect('data/processed/vectorial/shac.shp')

fechas <- read_xlsx('data/raw/tabulada/Aconcagua Alto_swei_1981-01-01-2024-04-01.xlsx') |> 
  mutate(estacional(fecha),
         .before = fecha) |>
  mutate(fecha = paste(año,estacion)) |> 
  pull(fecha)
fechas <- fechas[189:length(fechas)]

dir <- 'data/raw/raster/indices/'
index_names <- list.files(dir)

lapply(index_names,\(index) {
  
  sub_names <- list.files(glue('{dir}{index}'))
  
  lapply(sub_names, \(sub) {
    
    print(sub)
    
    sub_tif <- list.files(glue('{dir}{index}/{sub}'),full.names=T)
    sub_tif <- setdiff(sub_tif,sub_tif[grep('aux',sub_tif)])
    
    shac_proj <- shac |> 
      project(rast(sub_tif[1]))
    
    r <- rast(sub_tif) |> 
      crop(shac_proj) |> 
      project('EPSG:32719') |> 
      mask(shac)
    
    fecha_estacional <- str_extract(sub_tif, "\\d{4}-\\d{2}-\\d{2}") |> 
      estacional() |> 
      mutate(fecha_estacional = paste(año,estacion)) |> 
      pull(fecha_estacional)
    
    names(r) <- fecha_estacional
    
    r_estacional <- lapply(unique(fecha_estacional),\(x) {
      r_fecha <- r[[which(names(r) == x)]] |> 
        app(mean, na.rm=T)
      names(r_fecha) <- x
      r_fecha
    }) |> rast()
    
    fechas_faltantes <- setdiff(unique(fechas), names(r_estacional))
    
    for (fecha in fechas_faltantes) {
      nueva_layer <- r_estacional[[1]]
      values(nueva_layer) <- NA
      names(nueva_layer) <- fecha
      r_estacional <- c(r_estacional, nueva_layer)
    }
    
    r_estacional <- r_estacional[[match(unique(fechas), names(r_estacional))]]
    
    writeRaster(r_estacional,glue('data/processed/raster/indices/{sub}.tif'),
                overwrite=T)
    
  })
})




