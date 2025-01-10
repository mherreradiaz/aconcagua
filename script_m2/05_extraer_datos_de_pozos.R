library(tidyverse)
library(terra)
library(tidyterra)
library(readxl)

cob <- rreadxlcob <- rast('data/processed/raster/cobertura/cobertura.tif')

pozos <- read_rds('data/processed/rds/gwdi.rds') |> 
  vect(geom = c('lon', 'lat'), crs = 'EPSG:4326') |> 
  project(cob) |> 
  distinct(shac,codigo)

# writeVector(pozos,'data/processed/vectorial/pozos_filtrados.shp',
#             overwrite=T)

pozos <- vect('data/processed/vectorial/pozos_filtrados.shp')

cob <- rast('data/processed/raster/cobertura/tipo_vegetacional.tif')
cob_ag <- classify(cob,matrix(c(1,1),ncol=2),others = NA)
cob_bn <- classify(cob,matrix(c(2,1),ncol=2),others = NA)

pozos_buffer <- buffer(pozos, width = 1500)

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

#gwdi

gwdi <- read_rds('data/processed/rds/gwdi.rds') |> 
  select(año,estacion,shac,codigo,GWDI)

#cobertura

cobertura <- terra::extract(cob,pozos_buffer,fun=table) |> 
  mutate(shac = pozos$shac,
         codigo = pozos$codigo) |> 
  select(shac,codigo,AG = `1`,BN = `2`) |> 
  mutate(AG = AG*756.47/10000,
         BN = BN*756.47/10000) |> 
  pivot_longer(cols=c(AG,BN),names_to='veg',values_to='sup_ha')

#ndvi

ndvi <- rast(grep('NDVI',list.files('data/raw/raster/indices/',full.names=T),value=T))

#swei

swei <- read_xlsx('data/raw/tabulada/Aconcagua Alto_swei_1981-01-01-2024-04-01.xlsx') |> 
  mutate(estacional(fecha),
         .before = fecha) |> 
  select(-fecha) |> 
  group_by(año,estacion) |> 
  reframe(swei = mean(swei,na.rm=T))

#demás indices satelitales

fechas <- read_xlsx('data/raw/tabulada/Aconcagua Alto_swei_1981-01-01-2024-04-01.xlsx') |> 
  mutate(estacional(fecha),
         .before = fecha) |> 
  select(año,estacion)

r_files <- list.files('data/raw/raster/indices/',full.names=T)[-19]

lapply(r_files,\(x) {
  
  r <- rast(x) |> 
    project(cob)
  nombre <- sub(".*indices/(.*)\\.tif", "\\1",x)
  
  extrato_ag <- terra::extract(mask(r,cob_ag),pozos,mean,cores=6)
  
})

fechas <- as.Date(swei$fecha)



lista <- lapply(r_files,function(x) {
  r <- rast(x) |> 
    project(cob)
  nombre <- sub(".*indices/(.*)\\.tif", "\\1",sources(rast(x)))
  extract <- extract(r,cob,fun=function(x){mean(x,na.rm=T)}) |>
    mutate(cob = cob |> pull(id),
           .before = ID) |> 
    select(-ID)
  names(extract)[2:ncol(extract)] <- fechas
  extract |>
    pivot_longer(cols=2:ncol(extract),names_to = 'fecha',values_to = nombre) |> 
    mutate(fecha = as.Date(as.numeric(fecha)))
})

data <- reduce(lista, left_join, by = c("cob", "fecha")) |> 
  left_join(swei |> mutate(fecha = as.Date(fecha)) |> rename(SWEI = swei)) |> 
  select(fecha,cob,SWEI,everything()) |> 
  mutate(shac = substr(cob,2,4),
         codigo = cob,
         cob = substr(codigo,1,1),
         cob = ifelse(cob == 1,'AG','NV')) |> 
  pivot_longer(cols=`SWEI`:`zcSM-6`,names_to = 'index',values_to = 'value') |> 
  mutate(value = ifelse(is.na(value),NA,value),
         index = as.factor(index)) |> 
  select(fecha,shac,cob,codigo,everything())

write_rds(data,'data/processed/rds/indices_satelitales.rds')

# graficar



