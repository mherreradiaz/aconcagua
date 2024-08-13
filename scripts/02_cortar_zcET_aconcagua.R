library(terra)
library(sf)
library(fs)

dir <- '/mnt/data_procesada/data/rasters/Procesados/MODIS/zcETeos.MOD16A2.006.chl/'

files <- dir_ls(dir)

zcet <- rast(files)

cuenca <- read_sf('data/processed/cuenca_aconcagua.gpkg')

zcet_cuenca <- crop(zcet,cuenca)
zcet_cuenca <- mask(zcet_cuenca,cuenca)
zcet_cuenca <- trim(zcet_cuenca)

writeRaster(zcet_cuenca,'data/raw/rasters/zcET_aconcagua_2001-2002_2021-2022.tif')
