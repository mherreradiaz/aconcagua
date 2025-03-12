library(terra)
library(tidyterra)
library(tidyverse)

#pozos

pozos <- read_rds('data/processed/rds/pozo_estacion_filtrado.rds') |> 
  distinct(shac,codigo,nombre,lon,lat) |> 
  mutate(codigo = as.integer(codigo)) |> 
  vect(geom = c("lon", "lat"), crs = "EPSG:4326") |> 
  project('EPSG:32719')

writeVector(pozos, 'data/processed/vectorial/pozo_filtrado.shp',
            overwrite=T)

#cobertura vegetacional

cob_files <- grep('aux',list.files('data/raw/raster/cobertura/biomas/',full.names=T),value=T,invert=T)

shac <- vect('data/processed/vectorial/shac.shp')

cob_r <- rast(cob_files) |>
  classify(tibble(c(3,12,66,21,24),c(2,2,2,1,3)),others = NA)

r_base <- rast(list.files('data/raw/raster/indices/zcNDVI/zcNDVI-1/',full.names=T)[10]) |> 
  crop(shac)
values(r_base) <- 1
r_base <- r_base |> 
  mask(shac)

lapply(cob_files, \(tif) {
  
  año <- str_extract(tif, "\\d{4}")
  
  rast(tif) |> 
    crop(project(shac,'EPSG:4326')) |> 
    project('EPSG:32719',method = 'near') |> 
    mask(shac) |> 
    writeRaster(glue('data/processed/raster/cobertura/biomas/biomas_{año}.tif'))
  
})



r_base_disag <- disagg(r_base,method='near',fact = 20)

cob_r_disag <- project(cob_r,r_base_disag,method = 'near')
 
cob_r_proj <- aggregate(cob_r_disag, fact = 20, cores = 7, fun = \(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA)
  }
  t <- table(x)
  l <- sum(t)
  per <- t / l
  max_val <- names(which.max(per))
  max_per <- max(per)
  if (max_per >= 0.75) {
    return(as.numeric(max_val))
  } else {
    return(NA)
  }
})

writeRaster(cob_r_proj,'data/processed/raster/cobertura/cob_resampled.tif')

#tipo vegetacional

cob_r <- rast('data/processed/raster/cobertura/cob_resampled.tif')

cob_1 <- ifel(cob_r == 1,1,NA) |>
  app(fun=sum,na.rm=T)/23
cob_2 <- ifel(cob_r == 2,1,NA) |>
  app(fun=sum,na.rm=T)/23

cob_1_80 <- ifel(cob_1 >= .8,1,NA)
cob_2_80 <- ifel(cob_2 >= .8,2,NA)

cob <- app(c(cob_1_80,cob_2_80),sum,na.rm=T)
names(cob) <- 'cobertura'

writeRaster(cob,'data/processed/raster/cobertura/tipo_vegetacional.tif',
            overwrite=T)

