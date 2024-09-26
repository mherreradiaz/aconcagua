source('script/00_setup.R')

data <- read_rds('data/processed/rds/pozo_periodo_estacion.rds')
data_trend <- read_rds('data/processed/rds/pozo_trend_summary.rds')

filtro_0 <- read_xlsx('data/raw/tabulada/pozos_seleccion.xlsx') |> 
  pivot_longer(cols=everything(), names_to = 'shac',values_to = 'codigo') |> 
  na.omit() |> 
  mutate(seleccion = 0,
         codigo = as.factor(codigo))

filtro <- data |> 
  filter(periodo == '2000-2022') |> 
  distinct(shac,codigo) |> 
  left_join(filtro_0) |> 
  mutate(seleccion = ifelse(is.na(seleccion),1,seleccion))

data_seleccion <- data |>
  left_join(data_trend) |> 
  left_join(filtro) |> 
  filter(percent >= .5,
         seleccion == 1) |> 
  select(año:m,lon,lat)

write_rds(data_seleccion,'data/processed/rds/pozo_seleccion.rds')
