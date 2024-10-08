source('script/00_setup.R')
library(viridis)
library(ggcorrplot)
library(patchwork)
library(RColorBrewer)
library(Hmisc)

data <- read_rds('data/processed/rds/dataset_limpio.rds')

data_r <- data |> 
  select(año:cob,ncGWDI,everything(),-contains('ncGWDI_')) |>
  group_by(estacion,shac) |> 
  summarise(across(`SWEI`:`zcSM-36`, ~ cor(.x, ncGWDI, use = "complete.obs"), 
                   .names = "{.col}"), .groups = "drop") |> 
  pivot_longer(cols=`SWEI`:`zcSM-36`,names_to='indicator',values_to='r')

data_p <- data |> 
  select(año:cob,ncGWDI,everything(),-contains('ncGWDI_')) |>
  group_by(estacion,shac) |> 
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

data_cor |> 
  mutate(shac = as.factor(shac)) |> 
  ggplot(aes(shac,indicator,fill=r)) +
  geom_tile(color = 'grey80') +
  scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")),
                       limits = c(-1, 1),
                       breaks = seq(-1, 1, by = 0.5),
                       name = "r") +
  labs(y = NULL, x = NULL, title = 'ncGWDI') +
  geom_text(aes(label = round(r, 2)), color = "black", size = 2.5) +
  geom_text(aes(label = sign), color = "black", size = 4, vjust =1.5) +
  facet_wrap(~estacion, strip.position = 'right',ncol = 2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'),
        aspect.ratio = 1.1,
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(glue::glue('output/fig/analisis/correlacion_ncGWDI/general.png'), 
       width = 14, height = 8)
  
# SWEI

data <- read_rds('data/processed/rds/dataset_limpio.rds')

data_r <- data |> 
  select(año:cob,SWEI,everything(),-contains('SWEI_'),-contains('ncGWDI_lag')) |>
  group_by(estacion,shac) |> 
  summarise(across(ncGWDI:`zcSM-36`, ~ cor(.x, SWEI, use = "complete.obs"), 
                   .names = "{.col}"), .groups = "drop") |> 
  pivot_longer(cols=ncGWDI:`zcSM-36`,names_to='indicator',values_to='r')

data_p <- data |> 
  select(año:cob,SWEI,everything(),-contains('SWEI_'),-contains('ncGWDI_lag')) |>
  group_by(estacion,shac) |> 
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

data_cor |> 
  mutate(shac = as.factor(shac)) |> 
  ggplot(aes(shac,indicator,fill=r)) +
  geom_tile(color = 'grey80') +
  scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")),
                       limits = c(-1, 1),
                       breaks = seq(-1, 1, by = 0.5),
                       name = "r") +
  labs(y = NULL, x = NULL, title = 'SWEI') +
  geom_text(aes(label = round(r, 2)), color = "black", size = 2.5) +
  geom_text(aes(label = sign), color = "black", size = 4, vjust =1.5) +
  facet_wrap(~estacion, strip.position = 'right',ncol = 2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'),
        aspect.ratio = 1.1,
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(glue::glue('output/fig/analisis/correlacion_SWEI/general.png'), 
       width = 14, height = 8)

# zcNDVI

data <- read_rds('data/processed/rds/dataset_limpio.rds')

data_r <- data |> 
  select(año:cob,zcNDVI,everything(),-contains('zcNDVI_'),-contains('lead')) |>
  group_by(estacion,shac) |> 
  summarise(across(ncGWDI:`zcSM-36`, ~ cor(.x, zcNDVI, use = "complete.obs"), 
                   .names = "{.col}"), .groups = "drop") |> 
  pivot_longer(cols=ncGWDI:`zcSM-36`,names_to='indicator',values_to='r')

data_p <- data |> 
  select(año:cob,zcNDVI,everything(),-contains('zcNDVI_'),-contains('lead')) |>
  group_by(estacion,shac) |> 
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

data_cor |> 
  mutate(shac = as.factor(shac)) |> 
  ggplot(aes(shac,indicator,fill=r)) +
  geom_tile(color = 'grey80') +
  scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")),
                       limits = c(-1, 1),
                       breaks = seq(-1, 1, by = 0.5),
                       name = "r") +
  labs(y = NULL, x = NULL, title = 'zcNDVI') +
  geom_text(aes(label = round(r, 2)), color = "black", size = 2.5) +
  geom_text(aes(label = sign), color = "black", size = 4, vjust =1.5) +
  facet_wrap(~estacion, strip.position = 'right',ncol = 2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = 'white'),
        aspect.ratio = 1.1,
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(glue::glue('output/fig/analisis/correlacion_zcNDVI/general.png'), 
       width = 14, height = 8)

# matriz de correlación entre indicadores por estacion

data <- read_rds('data/processed/rds/dataset_limpio.rds')

shacs <- data$shac |> unique()

for (x in seq_along(shacs)) {
  
  data_shac <- data |> 
    filter(shac == shacs[x])
  
  estaciones <- data_shac$estacion |> unique()
  
  plot_estacion <- list()
  
  for (i in seq_along(estaciones)) {
    
    data_cor <- data_shac |>
      filter(estacion == estaciones[i]) |>
      select(-(año:cob)) |> 
      cor(use = 'pairwise.complete.obs') |> 
      as.data.frame() |> 
      select(-matches('ncGWDI_|SWEI_|zcNDVI_')) |> 
      rownames_to_column(var = "indicator_x") |> 
      pivot_longer(cols = ncGWDI:`zcSM-36`,names_to = 'indicator_y',values_to = 'cor') |> 
      mutate(cor = ifelse(indicator_y == 'ncGWDI' & grepl('ncGWDI',indicator_x),NA,cor),
             cor = ifelse(indicator_y == 'SWEI' & (grepl('SWEI',indicator_x) |
                                                     grepl('ncGWDI_lag',indicator_x)),NA,cor),
             cor = ifelse(indicator_y == 'zcNDVI' & (grepl('zcNDVI',indicator_x) |
                                                       grepl('ncGWDI_lead',indicator_x)),NA,cor),
             cor = ifelse(cor == 1,NA,cor))
    
    indicadores_x <- data_cor |> 
      pull(indicator_x) |> 
      unique()
    
    indicadores_y <- data_cor |> 
      pull(indicator_y) |> 
      unique()
    
    plot_estacion[[i]] <- data_cor |> 
      mutate(indicator_x = factor(indicator_x,levels = indicadores_x),
             indicator_y = factor(indicator_y,levels = rev(indicadores_y))) |> 
      ggplot(aes(indicator_x, indicator_y, fill = cor)) +
      geom_tile(color = 'grey80') + 
      geom_text(aes(label = round(cor, 2)), color = "black", size = 3) +
      scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")),
                           limits = c(-1, 1),
                           breaks = seq(-1, 1, by = 0.5),
                           name = "r",
                           na.value = 'transparent') +
      labs(x = NULL, y = NULL) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            aspect.ratio = .32,
            legend.position = 'none')
    
    if ((i == 1 | i == 2) & length(estaciones) == 4) {
      plot_estacion[[i]] <- plot_estacion[[i]] +
        theme(axis.title.x = element_blank(), 
              axis.text.x = element_blank())  
    } 
    
    if ((i == 2 | i == 4) & length(estaciones) == 4) {
      plot_estacion[[i]] <- plot_estacion[[i]] +
        theme(axis.title.y = element_blank(),  
              axis.text.y = element_blank())
    }
  }
  
  if (i == 2) {
    combined_plot <- plot_estacion[[1]] + plot_estacion[[2]] + 
      plot_layout(ncol = 2) + theme(plot.margin = margin(0, 0, 0, 0))
  } else if (i == 4) {
    combined_plot <- plot_estacion[[1]] + plot_estacion[[2]] + 
      plot_estacion[[3]] + plot_estacion[[4]] + plot_layout(ncol = 2) + 
      theme(plot.margin = margin(0, 0, 0, 0))
  }
  
  ggsave(glue::glue('output/fig/analisis/correlacion_general/estaciones/cor_shac_{shacs[x]}.png'),
         width = 12, height = 8)
  
}

# matriz de correlación entre indicadores general

data <- read_rds('data/processed/rds/dataset_limpio.rds')

shacs <- data$shac |> unique()

for (x in seq_along(shacs)) {
  
  data_shac <- data |> 
    filter(shac == shacs[x])
  
  data_cor <- data_shac |>
    filter(estacion == estaciones[i]) |>
    select(-(año:cob)) |> 
    cor(use = 'pairwise.complete.obs') |>
    as.data.frame() |> 
    select(-matches('ncGWDI_|SWEI_|zcNDVI_')) |> 
    rownames_to_column(var = "indicator_x") |> 
    pivot_longer(cols = ncGWDI:`zcSM-36`,names_to = 'indicator_y',values_to = 'cor') |> 
    mutate(cor = ifelse(indicator_y == 'ncGWDI' & grepl('ncGWDI',indicator_x),NA,cor),
           cor = ifelse(indicator_y == 'SWEI' & (grepl('SWEI',indicator_x) |
                                                   grepl('ncGWDI_lag',indicator_x)),NA,cor),
           cor = ifelse(indicator_y == 'zcNDVI' & (grepl('zcNDVI',indicator_x) |
                                                     grepl('ncGWDI_lead',indicator_x)),NA,cor),
           cor = ifelse(cor == 1,NA,cor))
  
  data_pcor <- data_shac |> 
    filter(estacion == estaciones[i]) |>
    select(-(año:cob)) |>
    na.omit() |> 
    as.matrix() |> 
    rcorr() |>
    pluck("P") |> 
    as.data.frame() |> 
    select(-matches('ncGWDI_|SWEI_|zcNDVI_')) |> 
    rownames_to_column(var = "indicator_x") |> 
    pivot_longer(cols = ncGWDI:`zcSM-36`,names_to = 'indicator_y',values_to = 'p_value') |> 
    mutate(p_value = ifelse(indicator_y == 'ncGWDI' & grepl('ncGWDI',indicator_x),NA,p_value),
           p_value = ifelse(indicator_y == 'SWEI' & (grepl('SWEI',indicator_x) |
                                                       grepl('ncGWDI_lag',indicator_x)),NA,p_value),
           p_value = ifelse(indicator_y == 'zcNDVI' & (grepl('zcNDVI',indicator_x) |
                                                         grepl('ncGWDI_lead',indicator_x)),NA,p_value),
           p_value = ifelse(p_value == 1,NA,p_value))
  
  indicadores_x <- data_cor |> 
    pull(indicator_x) |> 
    unique()
  
  indicadores_y <- data_cor |> 
    pull(indicator_y) |> 
    unique()
  
  data_cor |> 
    mutate(indicator_x = factor(indicator_x,levels = indicadores_x),
           indicator_y = factor(indicator_y,levels = rev(indicadores_y))) |> 
    ggplot(aes(indicator_x, indicator_y, fill = cor)) +
    geom_tile(color = 'grey80') + 
    geom_text(aes(label = round(cor, 2)), color = "black", size = 3) +
    scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")),
                         limits = c(-1, 1),
                         breaks = seq(-1, 1, by = 0.5),
                         name = "r",
                         na.value = 'transparent') +
    labs(x = NULL, y = NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          aspect.ratio = .32,
          legend.position = 'none',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave(glue::glue('output/fig/analisis/correlacion_general/general/shac_{shacs[x]}_cor.png'),
         width = 12, height = 8)
  
  data_pcor |> 
    mutate(indicator_x = factor(indicator_x,levels = indicadores_x),
           indicator_y = factor(indicator_y,levels = rev(indicadores_y)),
           p_category = ifelse(p_value > 0.05, "above", "below"),) |>
    ggplot(aes(indicator_x, indicator_y, fill = p_category)) +
    geom_tile(color = 'grey80') + 
    geom_text(aes(label = round(p_value, 2)), color = "black", size = 3) +
    scale_fill_manual(values = c("below" = "floralwhite", "above" = "firebrick3"),
                      name = "p-value", na.value = 'transparent') +
    labs(x = NULL, y = NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          aspect.ratio = .32,
          legend.position = 'none',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  ggsave(glue::glue('output/fig/analisis/correlacion_general/general/shac_{shacs[x]}_pvalue.png'),
         width = 12, height = 8)
  
}
