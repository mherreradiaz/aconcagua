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

# extraccion general

buffer <- 3000

unidades <- vect(glue('data/processed/vectorial/unidades_{buffer}.shp'))
cob <- rast('data/processed/raster/cobertura/cobertura_70.tif')
index_names <- gsub('.tif','',list.files('data/processed/raster/indices'))

años <- str_extract(names(cob), "\\d{4}")

data_index <- lapply(index_names,\(index) {
  r <- glue('data/processed/raster/indices/{index}.tif') |> 
    rast()
  if (!grepl('NDVI|SETI',index)) {
    terra::extract(r,unidades,fun=mean,na.rm=T) |> 
      mutate(shac = unidades$shac,
             codigo = unidades$codigo,
             .before = ID) |> 
      select(-ID) |> 
      pivot_longer(c(everything(),-shac,-codigo), names_to = 'fecha',
                   values_to = gsub('-','_',index))
  } else {
    año_r <- str_extract(names(r), "\\d{4}")
    index_ag <- list()
    index_vn <- list()
    for (x in seq_along(años)) {
      año <- años[x]
      cob_año <- cob[[grep(año,names(cob))]]
      index_año <- r[[grep(año,names(r))]]
      index_ag[[x]] <- mask(index_año,cob_año,maskvalues=1,inverse=T)
      index_vn[[x]] <- mask(index_año,cob_año,maskvalues=2,inverse=T)
    }
    r_ag <- index_ag |> rast()
    r_vn <- index_vn |> rast()
    
    terra::extract(r_ag,unidades,fun=mean,na.rm=T) |> 
      mutate(shac = unidades$shac,
             codigo = unidades$codigo,
             .before = ID) |> 
      select(-ID) |> 
      pivot_longer(c(everything(),-shac,-codigo), names_to = 'fecha',
                   values_to = paste0(gsub('-','_',index),'_AG')) |> 
      left_join(
        terra::extract(r_vn,unidades,fun=mean,na.rm=T) |> 
          mutate(shac = unidades$shac,
                 codigo = unidades$codigo,
                 .before = ID) |> 
          select(-ID) |> 
          pivot_longer(c(everything(),-shac,-codigo), names_to = 'fecha',
                       values_to = paste0(gsub('-','_',index),'_VN'))
      ) |> suppressMessages()
  }
}) |> 
  reduce(full_join) |> 
  suppressMessages()

# coberturas

cob <- list.files('data/processed/raster/cobertura/biomas/', full.names=T) |> 
  rast()

cob_ag <- ifel(cob == 21,1,NA)
cob_vn <- ifel(cob %in% c(3,12,66),1,NA)
cob_inf <- ifel(cob == 24,1,NA)
cob_total <- cob |> 
  setValues(1)

data_cob <- terra::extract(cob_ag,unidades,fun=sum,na.rm=T) |> 
  mutate(shac = unidades$shac,
         codigo = unidades$codigo,
         .before = -ID) |> 
  select(-ID) |> 
  pivot_longer(c(everything(),-(shac:codigo)),values_to = 'COB_AG', names_to = 'fecha') |> 
  left_join(
    terra::extract(cob_vn,unidades,fun=sum,na.rm=T) |> 
      mutate(shac = unidades$shac,
             codigo = unidades$codigo,
             .before = -ID) |> 
      select(-ID) |> 
      pivot_longer(c(everything(),-(shac:codigo)),values_to = 'COB_VN', names_to = 'fecha')
  ) |> suppressMessages() |> 
  left_join(
    terra::extract(cob_inf,unidades,fun=sum,na.rm=T) |> 
      mutate(shac = unidades$shac,
             codigo = unidades$codigo,
             .before = -ID) |> 
      select(-ID) |> 
      pivot_longer(c(everything(),-(shac:codigo)),values_to = 'COB_INF', names_to = 'fecha')
  ) |> suppressMessages() |> 
  left_join(
    terra::extract(cob_total,unidades,fun=sum,na.rm=T) |> 
      mutate(shac = unidades$shac,
             codigo = unidades$codigo,
             .before = -ID) |> 
      select(-ID) |> 
      pivot_longer(c(everything(),-(shac:codigo)),values_to = 'COB_TOTAL', names_to = 'fecha')
  ) |> suppressMessages() |> 
  mutate(COB_AG = COB_AG/COB_TOTAL,
         COB_VN = COB_VN/COB_TOTAL,
         COB_INF = COB_INF/COB_TOTAL,
         fecha = str_extract(fecha, "\\d{4}")) |> 
  select(-COB_TOTAL)

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

data <- data_index |> 
  separate(col = fecha,into = c('año','estacion'), sep = ' ') |> 
  mutate(año = as.integer(año),
         estacion = factor(estacion,levels = c('Verano','Otoño','Invierno','Primavera'))) |> 
  left_join(
    data_cob |> 
      mutate(año = as.integer(fecha)) |> 
      select(-fecha)
    ) |>
  left_join(data_gwi) |> 
  left_join(data_swei) |> 
  suppressMessages() |> 
  select(shac,codigo,año,estacion,GWI,contains('COB'),contains('SWEI'),
         contains('NDVI'),contains('SETI'),everything())

write_rds(data,glue('data/processed/rds/dataset_{buffer}.rds'))
