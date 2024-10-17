source('script/00_setup.R')
library(tidyterra)
library(tools)

# SWEI

cob <- rast('data/processed/raster/cobertura/cobertura.tif')

data <- read_rds('data/processed/rds/dataset_limpio.rds') |> 
  mutate(codigo = as.numeric(paste0(ifelse(cob == 'AG',1,2),shac)),
         .before=ncGWDI) |> 
  filter(estacion == 'Primavera') |> 
  select(año:codigo,contains('SWEI'))

nombres <- data |> select(contains('SWEI')) |> names()

for (i in seq_along(nombres)) {
  
  data_var <- data |> 
    select(año:codigo,all_of(nombres[i]))
  
  años <- unique(data_var$año)
  
  r_stack <- lapply(años, function(x) {
    value <- data_var |> 
      filter(año == x) |> 
      pull(all_of(nombres[i])) |> 
      unique()
    ifel(!is.na(cob), value, NA)
  }) |> 
    rast()
  
  names(r_stack) <- años
  varnames(r_stack) <- nombres[i]
  
  r_stack <- project(r_stack,cob) 
  
  writeRaster(r_stack,'data/processed/raster/indices/{nombres[i]}.tif', overwrite = TRUE)
  gc()
  
}

# NDVI

ndvi_r <- rast(list.files('data/raw/raster/indices/',full.names=T)[19])

ndvi <- rast(list(ifel(!is.na(ndvi_r[[1]]),NA,NA),
                  ndvi_r,
                  ifel(!is.na(ndvi_r[[1]]),NA,NA)))
names(ndvi) <- 2000:2022
varnames(ndvi) <- 'zcNDVI'

ndvi <- project(ndvi, cob)
ndvi <- mask(ndvi,cob)

writeRaster(ndvi,'data/processed/raster/indices/zcNDVI.tif',
            overwrite=T)

ndvi_lead <- rast(list(ndvi_r,
                       ifel(!is.na(ndvi_r[[1]]),NA,NA),
                       ifel(!is.na(ndvi_r[[1]]),NA,NA)))

names(ndvi_lead) <- 2000:2022
varnames(ndvi_lead) <- 'zcNDVI_lead'

ndvi_lead <- project(ndvi_lead, cob)
ndvi_lead <- mask(ndvi_lead,cob)

writeRaster(ndvi_lead,'data/processed/raster/indices/zcNDVI_lead.tif',
            overwrite=T)

# indices

r_files <- list.files('data/raw/raster/indices/',full.names=T)[-19]

swei <- read_xlsx('data/raw/tabulada/Aconcagua Alto_swei_1981-01-01-2024-04-01.xlsx')
fechas <- as.Date(swei$fecha)

años <- 2000:2022
meses <- 9:11

nombres <- file_path_sans_ext(basename(r_files))

for (i in 21:length(nombres)) {
  
  gc()
  
  print(nombres[i])
  
  r_index <- rast(r_files[i])
  
  r <- lapply(años,FUN = function(x) {
    r <- app(r_index[[which(year(fechas) == x)[9:11]]],
             function(x) {mean(x,na.rn=T)},cores=10)
    varnames(r) <- file_path_sans_ext(basename(sources(r_index)))
    names(r) <- x
    r
  }) |> 
    rast()
  
  r <- project(r,cob)
  r <- terra::mask(r,cob)
  
  writeRaster(r,glue::glue('data/processed/raster/indices/{nombres[i]}.tif'),
              overwrite=T)
  
}
