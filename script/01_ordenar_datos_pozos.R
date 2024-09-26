source('script/00_setup.R')

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

estaciones <- c('Verano','Otoño','Invierno','Primavera')

data_estacion_raw <- data |> 
  mutate(año = year(fecha),
         mes = month(fecha),
         estacion = factor(case_when(
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
  reframe(año = 1982:2022) |> 
  group_by(across(shac:año)) |> 
  reframe(estacion = factor(estaciones,levels = estaciones)) |> 
  left_join(data_estacion_raw) |> 
  select(año,estacion,shac:nombre,m,lon,lat)

periodos <- list(c(1982,2022),c(1982,2000),c(2000,2022),
                 c(2000,2010),c(2010,2022))

data_list <- list()

for (i in seq_along(periodos)) {
  data_list[[i]] <- data_estacion |> 
    filter(between(año,periodos[[i]][1],periodos[[i]][2])) |> 
    mutate(periodo = glue::glue('{periodos[[i]][1]}-{periodos[[i]][2]}'),
           .before = shac)
}

data_periodo_estacion <- bind_rows(data_list) |> 
  mutate(periodo = factor(periodo,levels = c('1982-2022','1982-2000','2000-2022',
                                             '2000-2010','2010-2022')),
         codigo = as.factor(codigo)) |> 
  select(año,periodo,estacion,everything()) |> 
  arrange(shac,codigo,año,periodo,estacion)

write_rds(data_periodo_estacion,'data/processed/rds/pozo_periodo_estacion.rds')
