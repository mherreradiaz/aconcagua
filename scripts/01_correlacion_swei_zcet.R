library(tidyverse)
library(terra)
library(sf)
library(fs)

zcet <- rast('data/raw/rasters/zcET_aconcagua_2001-2002_2021-2022.tif')
shac <- read_sf('data/raw/vectorial/SHAC_Aconcagua')

files <- dir_ls('data/raw/tabulada',regexp = 'xlsx$')

data_swei_alto <- readxl::read_xlsx(files[1]) |> 
  mutate(fecha = as_date(fecha)) |> 
  filter(between(fecha,ymd("2002-01-01"),ymd("2021-12-01"))) |> 
  mutate(mes = month(fecha)) |> 
  pivot_wider(names_from ='mes',values_from ='swei')

vals <- values(zcet)

cors <- lapply(1:12,\(i){
  a <- data_swei_alto[,i+1]
  a <- a[!is.na(a)]
  apply(vals,1,\(y,x=a){
    cor(x,y)
  })
})

cors_ras <- zcet[[1:12]]
values(cors_ras) <- NA

for (i in 1:12){
  values(cors_ras[[i]]) <- cors[[i]]
}

names(cors_ras) <- rev(month.name)
plot(cors_ras)

writeRaster(cors_ras,'data/processed/correlacion_zcET_vs_swei.tif')

library(tmap)

tm_shape(cors_ras) +
  tm_raster(style = 'jenks',palette = "RdYlBu") +
  tm_facets()

data_ex <- terra::extract(zcet,shac,fun = 'median',na.rm = TRUE)

data_ex |> 
  pivot_longer(-ID) |> 
  pivot_wider(names_from = 'ID',values_from = 'value') |> 
  set_names(c('name',shac$OBJECTID)) |> 
  mutate(year = 2002:2021) |> 
  relocate(year,.after = name) |> 
  select(-name)
