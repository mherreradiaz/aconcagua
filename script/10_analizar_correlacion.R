source('script/00_setup.R')
library(viridis)
library(ggcorrplot)
library(patchwork)
library(RColorBrewer)

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
  
  ggsave(glue::glue('output/fig/analisis/correlacion_general/general/cor_shac_{shacs[x]}.png'),
         width = 12, height = 8)
  
}

# correlacion ncGWDI

data <- read_rds('data/processed/rds/dataset.rds')

data_cor <- data |> 
  select(año:cob,ncGWDI,everything(),-contains('ncGWDI-'),-contains('ncGWDI+')) |> 
  group_by(estacion, shac, cob) |> 
  summarise(across(`SWEI`:`zcSM-6`, ~ cor(.x, ncGWDI, use = "complete.obs"), 
                   .names = "{.col}"), .groups = "drop") |> 
  pivot_longer(cols=`SWEI`:`zcSM-6`,names_to='indicator',values_to='cor')

write_rds(data_cor,'data/processed/rds/correlacion_ncGWDI.rds')

# graficar correlacion ncGWDI

data_cor <- read_rds('data/processed/rds/correlacion_ncGWDI.rds')

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
    geom_text(aes(label = round(cor, 2)), color = "black", size = 3) +
    scale_fill_gradientn(colors = brewer.pal(11, "RdBu"),
                         limits = c(-1, 1),
                         breaks = seq(-1, 1, by = 0.5),
                         name = "r") +
    facet_grid(~cob) +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white')) +
    labs(y = 'Indicador',
         x = 'shac')
  
  ggsave(glue::glue('output/fig/analisis/correlacion_ncGWDI/cor_{estaciones[x]}.png'), 
         width = 14, height = 8)
}

# correlacion SWEI

data <- read_rds('data/processed/rds/dataset.rds')

data_cor <- data |> 
  select(año:cob,SWEI,everything(),-contains('SWEI-'),-contains('ncGWDI-')) |> 
  group_by(estacion, shac, cob) |> 
  summarise(across(`ncGWDI`:`zcSM-6`, ~ cor(.x, SWEI, use = "complete.obs"), 
                   .names = "{.col}"), .groups = "drop") |> 
  pivot_longer(cols=`ncGWDI`:`zcSM-6`,names_to='indicator',values_to='cor')

write_rds(data_cor,'data/processed/rds/correlacion_SWEI.rds')

# graficar correlacion SWEI

data_cor <- read_rds('data/processed/rds/correlacion_SWEI.rds')

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
    geom_text(aes(label = round(cor, 2)), color = "black", size = 3) +
    scale_fill_gradientn(colors = brewer.pal(11, "RdBu"),
                         limits = c(-1, 1),
                         breaks = seq(-1, 1, by = 0.5),
                         name = "r") +
    facet_grid(~cob) +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white')) +
    labs(y = 'Indicador',
         x = 'Tipo de cobertura')
  
  ggsave(glue::glue('output/fig/analisis/correlacion_SWEI/cor_{estaciones[x]}.png'), 
         width = 14, height = 8)
}
