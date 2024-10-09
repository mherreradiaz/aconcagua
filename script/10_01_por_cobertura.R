source('script/00_setup.R')
library(viridis)
library(ggcorrplot)
library(patchwork)
library(RColorBrewer)
library(Hmisc)

# ncGWDI

data <- read_rds('data/processed/rds/dataset_limpio.rds')

data_r <- data |> 
  select(año:cob,ncGWDI,everything(),-contains('ncGWDI_')) |>
  group_by(estacion,shac,cob) |> 
  summarise(across(`SWEI`:`zcSM-36`, ~ cor(.x, ncGWDI, use = "complete.obs"), 
                   .names = "{.col}"), .groups = "drop") |> 
  pivot_longer(cols=`SWEI`:`zcSM-36`,names_to='indicator',values_to='r')

data_p <- data |> 
  select(año:cob,ncGWDI,everything(),-contains('ncGWDI_')) |>
  group_by(estacion,shac,cob) |> 
  summarise(across(`SWEI`:`zcSM-36`, ~ pluck(rcorr(.x, ncGWDI),'P')[1,2], 
                   .names = "{.col}"), .groups = "drop") |> 
  pivot_longer(cols=`SWEI`:`zcSM-36`,names_to='indicator',values_to='p_value')

data_cor <- data_r |> 
  left_join(data_p) |> 
  mutate(sign = ifelse(p_value < .05,'*',''))

indicadores <- unique(data_cor$indicator)
data_cor$indicator = factor(data_cor$indicator,levels = rev(indicadores))
shacs <- unique(data_cor$shac)
estaciones <- unique(data_cor$estacion)

for (x in seq_along(estaciones)) {
  
  data_cor |> 
    filter(estacion == estaciones[x]) |> 
    mutate(shac = as.factor(shac)) |> 
    ggplot(aes(shac,indicator,fill=r)) +
    geom_tile(color = 'grey80') +
    scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")),
                         limits = c(-1, 1),
                         breaks = seq(-1, 1, by = 0.5),
                         name = "r") +
    labs(y = NULL, x = NULL, title = 'ncGWDI') +
    geom_text(aes(label = round(r, 2)), color = "black", size = 3) +
    geom_text(aes(label = sign), color = "black", size = 5, vjust =2) +
    facet_grid(~cob) +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white'),
          aspect.ratio = 1.1,
          legend.position = 'none',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave(glue::glue('output/fig/analisis/correlacion_ncGWDI/cob_{estaciones[x]}.png'), 
         width = 14, height = 8)
  
}

# SWEI

data <- read_rds('data/processed/rds/dataset_limpio.rds')

data_r <- data |> 
  select(año:cob,SWEI,everything(),-contains('SWEI_'),-contains('ncGWDI_lag')) |>
  group_by(estacion,shac,cob) |> 
  summarise(across(ncGWDI:`zcSM-36`, ~ cor(.x, SWEI, use = "complete.obs"), 
                   .names = "{.col}"), .groups = "drop") |> 
  pivot_longer(cols=ncGWDI:`zcSM-36`,names_to='indicator',values_to='r')

data_p <- data |> 
  select(año:cob,SWEI,everything(),-contains('SWEI_'),-contains('ncGWDI_lag')) |>
  group_by(estacion,shac,cob) |> 
  summarise(across(ncGWDI:`zcSM-36`, ~ pluck(rcorr(.x, SWEI),'P')[1,2], 
                   .names = "{.col}"), .groups = "drop") |> 
  pivot_longer(cols=ncGWDI:`zcSM-36`,names_to='indicator',values_to='p_value')

data_cor <- data_r |> 
  left_join(data_p) |> 
  mutate(sign = ifelse(p_value < .05,'*',''))

indicadores <- unique(data_cor$indicator)
data_cor$indicator = factor(data_cor$indicator,levels = rev(indicadores))
shacs <- unique(data_cor$shac)
estaciones <- unique(data_cor$estacion)

for (x in seq_along(estaciones)) {
  
  data_cor |> 
    filter(estacion == estaciones[x]) |> 
    mutate(shac = as.factor(shac)) |> 
    ggplot(aes(shac,indicator,fill=r)) +
    geom_tile(color = 'grey80') +
    scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")),
                         limits = c(-1, 1),
                         breaks = seq(-1, 1, by = 0.5),
                         name = "r") +
    labs(y = NULL, x = NULL, title = 'SWEI') +
    geom_text(aes(label = round(r, 2)), color = "black", size = 3) +
    geom_text(aes(label = sign), color = "black", size = 5, vjust =2) +
    facet_grid(~cob) +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white'),
          aspect.ratio = 1.1,
          legend.position = 'none',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave(glue::glue('output/fig/analisis/correlacion_SWEI/cob_{estaciones[x]}.png'), 
         width = 14, height = 8)
  
}

# zcNDVI

data <- read_rds('data/processed/rds/dataset_limpio.rds')

data_r <- data |> 
  select(año:cob,zcNDVI,everything(),-contains('zcNDVI_'),-contains('lead')) |>
  group_by(estacion,shac,cob) |> 
  summarise(across(ncGWDI:`zcSM-36`, ~ cor(.x, zcNDVI, use = "complete.obs"), 
                   .names = "{.col}"), .groups = "drop") |> 
  pivot_longer(cols=ncGWDI:`zcSM-36`,names_to='indicator',values_to='r')

data_p <- data |> 
  select(año:cob,zcNDVI,everything(),-contains('zcNDVI_'),-contains('lead')) |>
  group_by(estacion,shac,cob) |> 
  summarise(across(ncGWDI:`zcSM-36`, ~ pluck(rcorr(.x, zcNDVI),'P')[1,2], 
                   .names = "{.col}"), .groups = "drop") |> 
  pivot_longer(cols=ncGWDI:`zcSM-36`,names_to='indicator',values_to='p_value')

data_cor <- data_r |> 
  left_join(data_p) |> 
  mutate(sign = ifelse(p_value < .05,'*',''))

indicadores <- unique(data_cor$indicator)
data_cor$indicator = factor(data_cor$indicator,levels = rev(indicadores))
shacs <- unique(data_cor$shac)
estaciones <- unique(data_cor$estacion)

for (x in seq_along(estaciones)) {
  
  data_cor |> 
    filter(estacion == estaciones[x]) |> 
    mutate(shac = as.factor(shac)) |> 
    ggplot(aes(shac,indicator,fill=r)) +
    geom_tile(color = 'grey80') +
    scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")),
                         limits = c(-1, 1),
                         breaks = seq(-1, 1, by = 0.5),
                         name = "r") +
    labs(y = NULL, x = NULL, title ='zcNDVI') +
    geom_text(aes(label = round(r, 2)), color = "black", size = 3) +
    geom_text(aes(label = sign), color = "black", size = 5, vjust =2) +
    facet_grid(~cob) +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white'),
          aspect.ratio = 1.1,
          legend.position = 'none',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave(glue::glue('output/fig/analisis/correlacion_zcNDVI/cob_{estaciones[x]}.png'), 
         width = 14, height = 8)
  
}
