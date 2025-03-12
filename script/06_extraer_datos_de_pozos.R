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

pozos <- vect('data/processed/vectorial/pozo_filtrado.shp')

pb_list <- lapply(1:dim(pozos)[1],\(x) {
  buffer(pozos[x],1500)
})

cob <- rast('data/processed/raster/cobertura/tipo_vegetacional.tif')

cob_pol <- cob |> 
  as.polygons()

tif <- list.files('data/processed/raster/indices',full.names=T)

data_index <- lapply(pb_list,\(pb) {
  
  unidad_analisis <- intersect(pb,cob_pol) |> 
    mutate(cobertura = ifelse(cobertura == 1, 'AG','NV'))
  
  values_ua <- lapply(tif,\(x) {
    r <- rast(x)
    name_r <- gsub('.tif','',basename(sources(r)))
    
    terra::extract(r,unidad_analisis,fun=mean) |> 
      mutate(shac = unidad_analisis$shac,
             codigo = unidad_analisis$codigo,
             cobertura = unidad_analisis$cobertura,
             .before = -ID) |> 
      select(-ID) |> 
      pivot_longer(c(everything(),-c(shac,codigo,cobertura)),values_to = name_r, names_to = 'fecha')
  }) |> 
    reduce(full_join) |> 
    suppressMessages()
}) |> 
  bind_rows() |> 
  separate(fecha, into = c('año','estacion'),sep=' ') |> 
  mutate(año = as.integer(año),
         estacion = factor(estacion,levels = c('Verano','Otoño','Invierno','Primavera'))) |> 
  select(año,estacion,shac,codigo,cobertura,everything())

#gwi

gwi <- read_rds('data/processed/rds/gwi.rds') |> 
  mutate(año = as.integer(año),
         codigo = as.integer(codigo)) |> 
  select(año,estacion,shac,codigo,GWI)

data_index_p1 <- data_index |> 
  left_join(gwi) |> 
  select(año,estacion,shac,codigo,cobertura,GWI,everything())

#swei

swei <- read_xlsx('data/raw/tabulada/Aconcagua Alto_swei_1981-01-01-2024-04-01.xlsx') |> 
  mutate(estacional(fecha),
         .before = fecha) |> 
  select(-fecha) |> 
  group_by(año,estacion) |> 
  reframe(SWEI = mean(swei,na.rm=T)) |> 
  mutate(año = as.integer(año),
         SWEI_lag_1 = lag(SWEI,1),
         SWEI_lag_2 = lag(SWEI,2),
         SWEI_lag_3 = lag(SWEI,3),
         SWEI_lag_4 = lag(SWEI,4))

data_index_p2 <- data_index_p1 |> 
  left_join(swei) |>
  select(año,estacion,shac,codigo,cobertura,GWI,names(swei)[-c(1:2)],everything())

write_rds(data_index_p2,'data/processed/rds/dataset.rds')

#### nuevo proceso

pozos <- vect('data/processed/vectorial/pozo_filtrado.shp')

pb_list <- lapply(1:dim(pozos)[1],\(x) {
  buffer(pozos[x],2000)
})

index_names <- gsub('.tif','',list.files('data/processed/raster/indices'))

data_p1 <- lapply(index_names, \(index) {
  
  r <- rast(glue('data/processed/raster/indices/{index}.tif'))
  r <- r[[which(between(as.numeric(str_extract(names(r),'\\d{4}')),2000,2022))]]
  
  data_index <- lapply(pb_list,\(pb) {
    terra::extract(r,pb,fun=mean) |> 
      mutate(shac = pb$shac,
             codigo = pb$codigo,
             .before = -ID) |> 
      select(-ID) |> 
      pivot_longer(c(everything(),-(shac:codigo)),values_to = index, names_to = 'fecha')
  }) |> 
    bind_rows()
}) |> 
  reduce(full_join) |> 
  suppressMessages()

#ndvi y seti por tipo vegetacional


cob_list <- list.files('data/processed/raster/cobertura/biomas/',full.names=T)

index_names <- gsub('.tif','',list.files('data/processed/raster/indices'))
index_names <- grep('SETI|NDVI',index_names, value=T)

data_p2 <- lapply(index_names, \(index) {
  
  r <- rast(glue('data/processed/raster/indices/{index}.tif'))
  r <- r[[which(between(as.numeric(str_extract(names(r),'\\d{4}')),2000,2022))]]
  
  r_ag <- list()
  r_vn <- list()
  
  año <- 2000:2022
  
  for (x in seq_along(año)) {
    gc()
    
    r_año <- r[[grep(año[x],names(r))]]
    
    cob_año <- grep(año[x],cob_list,value=T) |> 
      rast()
    
    cob_año_ag <- ifel(cob_año == 21,1,NA)
    cob_año_vn <- ifel(cob_año %in% c(3,12,66),1,NA)
    
    r_ag[[x]] <- r_año |> 
      project(cob_año_ag) |> 
      mask(cob_año_ag)
    r_vn[[x]] <- r_año |> 
      project(cob_año_vn) |> 
      mask(cob_año_vn)
  }
  
  r_ag <- rast(r_ag)
  r_vn <- rast(r_vn)
  
  data_index <- lapply(pb_list,\(pb) {
    
    data_ag <- terra::extract(r_ag,pb,fun=mean) |> 
      mutate(shac = pb$shac,
             codigo = pb$codigo,
             .before = -ID) |> 
      select(-ID) |> 
      pivot_longer(c(everything(),-(shac:codigo)),values_to = paste0('AG_',index), names_to = 'fecha')
    
    data_vn <- terra::extract(r_vn,pb,fun=mean) |> 
      mutate(shac = pb$shac,
             codigo = pb$codigo,
             .before = -ID) |> 
      select(-ID) |> 
      pivot_longer(c(everything(),-(shac:codigo)),values_to = paste0('VN_',index), names_to = 'fecha')
    
    left_join(data_ag,data_vn) |> 
      suppressMessages()
    
  }) |> 
    bind_rows()
}) |> 
  reduce(full_join) |> 
  suppressMessages()

# coberturas

cob_list <- list.files('data/processed/raster/cobertura/biomas/',full.names=T)

data_p3 <- lapply(cob_list, \(cob_año) {
  
  r <- rast(cob_año) |> 
    
  r <- r[[which(between(as.numeric(str_extract(names(r),'\\d{4}')),2000,2022))]]
  
  r_ag <- ifel(cob_año == 21,1,NA)
  r_vn <- ifel(cob_año %in% c(3,12,66),1,NA)
  r_in <- ifel(cob_año == 24,1,NA)
  r_total <- cob_año
  values(r_total) <- 1
   
  data_index <- lapply(pb_list,\(pb) {
    terra::extract(r,pb,fun=mean) |> 
      mutate(shac = pb$shac,
             codigo = pb$codigo,
             .before = -ID) |> 
      select(-ID) |> 
      pivot_longer(c(everything(),-(shac:codigo)),values_to = index, names_to = 'fecha')
  }) |> 
    bind_rows()
}) |> 
  reduce(full_join) |> 
  suppressMessages()

# unir ambos

data <- left_join(data_p1,data_p2) |> 
  supressMessages()