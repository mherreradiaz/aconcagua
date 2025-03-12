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

data_ndvi <- read_rds('data/processed/rds/ndvi.rds') |> 
  group_by(across(año:value)) |> 
  reframe(estacion = estaciones) |> 
  mutate(estacion = factor(estacion,levels = estaciones)) |> 
  pivot_wider(names_from ='index',values_from = 'value') |> 
  select(-codigo)

data <- data_sequia |> 
  left_join(data_indices) |> 
  left_join(data_ndvi) |> 
  filter(periodo == '1982-2022',
         año >= 2000) |> 
  select(año:cob,ncGWDI,SWEI,zcNDVI,everything(),-periodo) |> 
  arrange(shac,cob,año,estacion) |> 
  group_by(shac,cob) |> 
  mutate(`ncGWDI_lag_1` = lag(ncGWDI,1),
         `ncGWDI_lag_2` = lag(ncGWDI,2),
         `ncGWDI_lag_3` = lag(ncGWDI,3),
         `ncGWDI_lag_4` = lag(ncGWDI,4),,
         `ncGWDI_lead_1` = lead(ncGWDI,1),
         `ncGWDI_lead_2` = lead(ncGWDI,2),
         `ncGWDI_lead_3` = lead(ncGWDI,3),
         `ncGWDI_lead_4` = lead(ncGWDI,4),
         .before = SWEI) |> 
  mutate(`SWEI_lag_1` = lag(SWEI,1),
         `SWEI_lag_2` = lag(SWEI,2),
         `SWEI_lag_3` = lag(SWEI,3),
         `SWEI_lag_4` = lag(SWEI,4),
         .before = zcNDVI) |> 
  mutate(`zcNDVI_lead_1` = lead(zcNDVI,4),
         .before = `EDDI-1`) |> 
  ungroup()

write_rds(data,'data/processed/rds/dataset.rds')
