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

#### nuevo proceso

pozos <- vect('data/processed/vectorial/pozo_filtrado.shp')

pb_list <- lapply(1:dim(pozos)[1],\(x) {
  buffer(pozos[x],2000)
})

index_names <- gsub('.tif','',list.files('data/processed/raster/indices'))

data_hidro <- lapply(index_names, \(index) {
  
  r <- rast(glue('data/processed/raster/indices/{index}.tif'))
  r <- r[[which(between(as.numeric(str_extract(names(r),'\\d{4}')),2000,2022))]]
  
  data_index <- lapply(pb_list,\(pb) {
    terra::extract(r,pb,fun=mean,na.rm=T) |> 
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

r_cob <- cob_list |> 
  rast()

r_ag_base <- ifel(r_cob == 21,1,NA)
r_ag_base <- r_ag_base[[rep(1:nlyr(r_ag_base), each = 4)]]

r_vn_base <- ifel(r_cob %in% c(3,12,66),1,NA)
r_vn_base <- r_vn_base[[rep(1:nlyr(r_vn_base), each = 4)]]

index_names <- gsub('.tif','',list.files('data/processed/raster/indices'))
index_names <- grep('SETI|NDVI',index_names, value=T)

data_vi <- lapply(index_names, \(index) {
  
  print(index)
  
  r <- rast(glue('data/processed/raster/indices/{index}.tif'))
  r <- r[[which(between(as.numeric(str_extract(names(r),'\\d{4}')),2000,2022))]]
  
  r_ag <- r |>
    project(r_ag_base) |> 
    mask(r_ag_base)
  
  r_vn <- r |>
    project(r_vn_base) |> 
    mask(r_vn_base)
    
  data_index <- lapply(pb_list,\(pb) {
    
    data_ag <- terra::extract(r_ag,pb,fun=mean,na.rm=T) |> 
      mutate(shac = pb$shac,
             codigo = pb$codigo,
             .before = -ID) |> 
      select(-ID) |> 
      pivot_longer(c(everything(),-(shac:codigo)),values_to = paste0('AG_',index), names_to = 'fecha')
    
    data_vn <- terra::extract(r_vn,pb,fun=mean,na.rm=T) |> 
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

r_cob <- cob_list |> 
  rast()
r_cob <- r_cob[[which(between(as.numeric(str_extract(names(r_cob),'\\d{4}')),2000,2022))]]

r_ag_base <- ifel(r_cob == 21,1,NA)
r_vn_base <- ifel(r_cob %in% c(3,12,66),1,NA)
r_if_base <- ifel(r_cob == 24,1,NA)
r_total <- r_cob
values(r_total) <- 1

data_cob <- lapply(pb_list,\(pb) {
  
  ag_cob <- terra::extract(r_ag_base,pb,fun=sum,na.rm=T) |> 
    mutate(shac = pb$shac,
           codigo = pb$codigo,
           .before = -ID) |> 
    select(-ID) |> 
    pivot_longer(c(everything(),-(shac:codigo)),values_to = 'ag_cob', names_to = 'fecha')
  
  vn_cob <- terra::extract(r_vn_base,pb,fun=sum,na.rm=T) |> 
    mutate(shac = pb$shac,
           codigo = pb$codigo,
           .before = -ID) |> 
    select(-ID) |> 
    pivot_longer(c(everything(),-(shac:codigo)),values_to = 'vn_cob', names_to = 'fecha')
  
  if_cob <- terra::extract(r_if_base,pb,fun=sum,na.rm=T) |> 
    mutate(shac = pb$shac,
           codigo = pb$codigo,
           .before = -ID) |> 
    select(-ID) |> 
    pivot_longer(c(everything(),-(shac:codigo)),values_to = 'in_cob', names_to = 'fecha')
  
  total_cob <- terra::extract(r_total,pb,fun=sum,na.rm=T) |> 
    mutate(shac = pb$shac,
           codigo = pb$codigo,
           .before = -ID) |> 
    select(-ID) |> 
    pivot_longer(c(everything(),-(shac:codigo)),values_to = 'total_cob', names_to = 'fecha')
  
  reduce(list(ag_cob,vn_cob,if_cob,total_cob),full_join) |> 
    suppressMessages() |> 
    mutate(ag_cob = ag_cob/total_cob,
           vn_cob = vn_cob/total_cob,
           in_cob = in_cob/total_cob) |> 
    select(-total_cob)
}) |> 
  bind_rows()

data_cob <- data_cob |> 
  mutate(año = as.numeric(str_extract(fecha,'\\d{4}'))) |> 
  select(-fecha)

#gwi

data_gwi <- read_rds('data/processed/rds/gwi.rds') |> 
  mutate(año = as.integer(año),
         codigo = as.integer(codigo)) |> 
  select(año,estacion,shac,codigo,GWI)

#swei

data_swei <- read_xlsx('data/raw/tabulada/Aconcagua Alto_swei_1981-01-01-2024-04-01.xlsx') |> 
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

# unir

data_index <- left_join(data_hidro,data_vi) |> 
  suppressMessages()

data <- data_index |>
  mutate(año = as.numeric(str_extract(fecha,'\\d{4}')),
         estacion = factor(str_extract(fecha,'[A-Za-zñÑáéíóúÁÉÍÓÚ]+'),
                           levels = c('Verano','Otoño','Invierno','Primavera'))) |> 
  left_join(data_cob) |> 
  left_join(data_gwi) |> 
  left_join(data_swei) |> 
  select(shac,codigo,año,estacion,GWI,contains('cob'),contains('SWEI'),
         contains('NDVI'),contains('SETI'),everything(),-fecha)

names(data) <- c(names(data)[1:4],gsub('-','_',toupper(names(data)))[5:67])

write_rds(data,'data/processed/rds/dataset.rds')
