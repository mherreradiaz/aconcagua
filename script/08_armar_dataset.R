source('script/00_setup.R')
library(tidyterra)

data_sequia <- read_rds('data/processed/rds/ncGWDI_shac_seleccion.rds') |> 
  group_by(across(everything())) |> 
  reframe(cob = c('AG','NV')) |> 
  select(año,periodo,estacion,shac,cob,ncGWDI_mean) |> 
  rename(ncGWDI = ncGWDI_mean)

estaciones <- c('Verano','Otoño','Invierno','Primavera')

data_indices <- read_rds('data/processed/rds/indices_satelitales.rds') |> 
  mutate(año = year(fecha),
         mes = month(fecha),
         estacion = factor(case_when(
           mes %in% c(12, 1, 2) ~ "Verano",
           mes %in% c(3, 4, 5) ~ "Otoño",
           mes %in% c(6, 7, 8) ~ "Invierno",
           mes %in% c(9, 10, 11) ~ "Primavera"), levels=estaciones),
         año = ifelse(mes == 12,año+1,año)) |> 
  group_by(año,estacion,shac,cob,index) |> 
  reframe(value = mean(value,na.rm=T)) |> 
  pivot_wider(names_from ='index',values_from = 'value')

data <- data_sequia |> 
  left_join(data_indices) |> 
  filter(periodo == '1982-2022',
         año >= 2000) |> 
  select(-periodo)

write_rds(data,'data/processed/rds/dataset.rds')
