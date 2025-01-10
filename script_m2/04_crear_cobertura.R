library(terra)
library(tidyterra)

#resample 75%

cob_files <- list.files('data/raw/raster/cobertura/biomas/',full.names=T)[-24]

cob_r <- rast(cob_files) |>
  classify(tibble(c(3,12,66,21),c(2,2,2,1)),others = NA)

r_base <- rast(grep('NDVI',list.files('data/raw/raster/indices/',full.names=T),value=T))[[1]]

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

# persistencia

cob_r <- rast('data/processed/raster/cobertura/cob_resampled.tif')

cob_1 <- ifel(cob_r == 1,1,NA) |>
  app(fun=sum,na.rm=T)/23
cob_2 <- ifel(cob_r == 2,1,NA) |>
  app(fun=sum,na.rm=T)/23

cob_1_80 <- ifel(cob_1 >= .8,1,NA)
cob_2_80 <- ifel(cob_2 >= .8,2,NA)

cob <- app(c(cob_1_80,cob_2_80),sum,na.rm=T)

writeRaster(cob,'data/processed/raster/cobertura/tipo_vegetacional.tif',
            overwrite=T)

# cobertura

cob <- rast('data/processed/raster/cobertura/tipo_vegetacional.tif')
veg <- cob*1000

shac <- vect('data/raw/vectorial/SHAC_Aconcagua.shp') |>
  select(shac = OBJECTID) |>
  rasterize(cob,field = 'shac',fun = 'max')

cob <- app(c(veg,shac),fun = sum,na.rm=T) |>
  mask(veg) |>
  mask(shac)

names(cob) <- 'id'

writeRaster(cob,'data/processed/raster/cobertura/cobertura.tif',
            overwrite=T)
