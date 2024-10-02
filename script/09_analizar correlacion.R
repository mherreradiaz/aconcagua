source('script/00_setup.R')
library(viridis)

# correlacion DI

data <- read_rds('data/processed/rds/dataset.rds')

data_cor <- data |> 
  select(año:cob,DI,SWEI,everything()) |> 
  group_by(estacion, shac, cob) |> 
  summarise(across(`SWEI`:`zcSM-6`, ~ cor(.x, DI, use = "complete.obs"), 
                   .names = "{.col}"), .groups = "drop") |> 
  pivot_longer(cols=`SWEI`:`zcSM-6`,names_to='indicator',values_to='cor')

write_rds(data_cor,'data/processed/rds/correlacion_di.rds')

# graficar correlacion DI

data_cor <- read_rds('data/processed/rds/correlacion_di.rds')

indicadores <- rev(unique(data_cor$indicator))

data_cor$indicator = factor(data_cor$indicator,levels = indicadores)

estaciones <- c('Verano','Otoño','Invierno','Primavera')

for (x in seq_along(estaciones)) {
  
  data_cor |> 
    filter(estacion == estaciones[x]) |> 
    mutate(shac = as.factor(shac),
           cob = as.factor(cob)) |> 
    ggplot(aes(shac,indicator,fill=cor)) +
    geom_tile() +
    geom_text(aes(label = round(cor, 2)), color = "white", size = 3) +
    scale_fill_viridis(option = "viridis", name = "r", 
                       limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
    facet_grid(~cob) +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white')) +
    labs(y = 'Indicador',
         x = 'Tipo de cobertura')
  
  ggsave(glue::glue('output/fig/analisis/correlacion_di/cor_{estaciones[x]}.png'), 
         width = 14, height = 8)
}

# correlacion SWEI

data <- read_rds('data/processed/rds/dataset.rds')

data_cor <- data |> 
  select(año:cob,SWEI,DI,everything()) |> 
  group_by(estacion, shac, cob) |> 
  summarise(across(`DI`:`zcSM-6`, ~ cor(.x, SWEI, use = "complete.obs"), 
                   .names = "{.col}"), .groups = "drop") |> 
  pivot_longer(cols=`DI`:`zcSM-6`,names_to='indicator',values_to='cor')

write_rds(data_cor,'data/processed/rds/correlacion_swei.rds')

# graficar correlacion DI

data_cor <- read_rds('data/processed/rds/correlacion_swei.rds')

indicadores <- rev(unique(data_cor$indicator))

data_cor$indicator = factor(data_cor$indicator,levels = indicadores)

estaciones <- c('Verano','Otoño','Invierno','Primavera')

for (x in seq_along(estaciones)) {
  
  data_cor |> 
    filter(estacion == estaciones[x]) |> 
    mutate(shac = as.factor(shac),
           cob = as.factor(cob)) |> 
    ggplot(aes(shac,indicator,fill=cor)) +
    geom_tile() +
    geom_text(aes(label = round(cor, 2)), color = "white", size = 3) +
    scale_fill_viridis(option = "viridis", name = "r", 
                       limits = c(-1, 1), breaks = seq(-1, 1, by = 0.5)) +
    facet_grid(~cob) +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white')) +
    labs(y = 'Indicador',
         x = 'Tipo de cobertura')
  
  ggsave(glue::glue('output/fig/analisis/correlacion_swei/cor_{estaciones[x]}.png'), 
         width = 14, height = 8)
}
