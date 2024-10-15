source('script/00_setup.R')
library(tidyterra)

# persistencia

cob_files <- list.files('data/raw/raster/cobertura/biomas/',full.names=T)[-24]

cob_r <- rast(cob_files) |>
  classify(tibble(c(3,12,66,21),c(2,2,2,1)),others = NA)

cob_1 <- ifel(cob_r == 1,1,NA) |>
  app(fun=sum,na.rm=T)/23
cob_2 <- ifel(cob_r == 2,1,NA) |>
  app(fun=sum,na.rm=T)/23

cob_1_80 <- ifel(cob_1 >= .8,1,NA)
cob_2_80 <- ifel(cob_2 >= .8,2,NA)

cob <- app(c(cob_1_80,cob_2_80),sum,na.rm=T) |>
  project('EPSG:4326')

writeRaster(cob,'data/processed/raster/cobertura/tipo_vegetacional.tif',
            overwrite=T)

# cobertura

veg <- rast('data/processed/raster/cobertura/tipo_vegetacional.tif')*1000

shac <- vect('data/raw/vectorial/SHAC_Aconcagua.shp') |>
  select(shac = OBJECTID) |>
  project('EPSG:4326') |>
  rasterize(cob,field = 'shac',fun =max)

cob <- app(c(veg,shac),fun = sum,na.rm=T) |>
  mask(veg) |>
  mask(shac)

writeRaster(cob,'data/processed/raster/cobertura/cobertura.tif',
            overwrite=T)
