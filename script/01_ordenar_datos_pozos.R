library(tidyverse)
library(terra)
library(readxl)

gw <- read_xlsx('data/raw/tabulada/gw_chile.xlsx') |> 
  filter(Basin == 'RIO ACONCAGUA') |> 
  select(codigo = Code, fecha = Date_String,m = `Depth to water (m)`)

pozos <- vect('data/processed/vectorial/pozos_aconcagua.shp') |> 
  values() |>
  rowwise() |> 
  mutate(codigo = as.numeric(str_split(COD_BNA,'-')[[1]][1]),
         ID_IDE = gsub('SHAC_','',ID_IDE)) |> 
  rename(nombre = NOMBRE, shac = ID_IDE) |> 
  select(shac,codigo,nombre,lon,lat) |> 
  arrange(codigo)

data <- gw |> 
  left_join(pozos) |> 
  arrange(shac,codigo,fecha)

write_rds(data,'data/processed/rds/pozo_general.rds')

#agregación mensual

data <- read_rds('data/processed/rds/pozo_general.rds')

data_mes_raw <- data |> 
  mutate(año = year(fecha),
         mes = month(fecha)) |> 
  group_by(año,mes,shac,codigo,nombre,lon,lat) |> 
  reframe(m = mean(m,na.rm=T)) |> 
  select(año,mes,shac,codigo,nombre,m,lon,lat)

data_mes <- data |> 
  distinct(shac,codigo,nombre,lon,lat) |> 
  group_by(across(shac:lat)) |>
  reframe(año = 1982:2022) |> 
  group_by(across(shac:año)) |> 
  reframe(mes = 1:12) |> 
  left_join(data_estacion_raw) |> 
  select(año,mes,shac:nombre,m,lon,lat)

write_rds(data_mes,'data/processed/rds/pozo_mes.rds')

#agregación estacional

data <- read_rds('data/processed/rds/pozo_mes.rds')

estaciones <- c('Verano','Otoño','Invierno','Primavera')

data_estacion_raw <- data |> 
  mutate(estacion = factor(case_when(
           mes %in% c(12, 1, 2) ~ "Verano",
           mes %in% c(3, 4, 5) ~ "Otoño",
           mes %in% c(6, 7, 8) ~ "Invierno",
           mes %in% c(9, 10, 11) ~ "Primavera"), levels=estaciones),
         año = ifelse(mes == 12,año+1,año)) |> 
  group_by(año,estacion,shac,codigo,nombre,lon,lat) |> 
  reframe(m = mean(m,na.rm=T)) |> 
  select(año,estacion,shac,codigo,nombre,m,lon,lat)

data_estacion <- data |> 
  distinct(shac,codigo,nombre,lon,lat) |> 
  group_by(across(shac:lat)) |>
  reframe(año = 2000:2022) |> 
  group_by(across(shac:año)) |> 
  reframe(estacion = factor(estaciones,levels = estaciones)) |> 
  left_join(data_estacion_raw) |> 
  select(año,estacion,shac:nombre,m,lon,lat)

write_rds(data_estacion,'data/processed/rds/pozo_estacion.rds')
