source('script/00_setup.R')
library(viridis)
library(ggcorrplot)
library(patchwork)
library(RColorBrewer)

# matriz de correlación entre indicadores

data <- read_rds('data/processed/rds/dataset.rds')

indicadores <- data |>
  select(año:cob,ncGWDI,SWEI,everything()) |> 
  pivot_longer(cols=ncGWDI:`zcSM-6`,names_to='indicator',values_to='value') |> 
  pull(indicator) |> 
  unique() |> 
  rev()

data <- data |> 
  select(año:cob,ncGWDI,SWEI,everything()) |> 
  pivot_longer(cols=ncGWDI:`zcSM-6`,names_to='indicator',values_to='value') |>
  mutate(indicator = factor(indicator,levels = indicadores)) |> 
  arrange(año,estacion,shac,cob,indicator) |> 
  pivot_wider(names_from = 'indicator',values_from = 'value')

shacs <- data$shac |> unique()

estaciones <- c('Verano','Otoño','Invierno','Primavera')

for (i in seq_along(shacs)) {
  
  data_shac <- data |> 
    filter(shac == shacs[i])

  estaciones <- data_shac$estacion |> unique()
  
  plot_estacion <- list()
  
  for (x in seq_along(estaciones)) {
    
    data_estacion <- data_shac |> 
      filter(estacion == estaciones[x]) |>
      select(-año, -estacion, -shac, -cob) |> 
      cor(use = "complete.obs")
    
    plot_estacion[[x]] <- data_estacion |> 
      ggcorrplot(method = "square", 
                 type = "upper",
                 lab = TRUE,
                 lab_size = 2.2,
                 lab_col = 'white',
                 ggtheme = theme_bw()) +
      geom_text(aes(Inf,-Inf),label = estaciones[x],vjust = -1,hjust = 1.1,
                size = 5) +
      scale_fill_viridis_c(option = "D", 
                           limits = c(-1, 1),
                           breaks = seq(-1, 1, 0.5),
                           name = 'r') +
      theme(legend.position = 'none')
    
    if ((x == 1 | x == 2) & length(estaciones) == 4) {
      plot_estacion[[x]] <- plot_estacion[[x]] +
        theme(axis.title.x = element_blank(), 
              axis.text.x = element_blank())  
    } 
    
    if ((x == 2 | x == 4) & length(estaciones) == 4) {
      plot_estacion[[x]] <- plot_estacion[[x]] +
        theme(axis.title.y = element_blank(),  
              axis.text.y = element_blank())
    }
    
  }
  
  if (x == 2) {
    combined_plot <- plot_estacion[[1]] + plot_estacion[[2]] + 
      plot_layout(ncol = 2) + theme(plot.margin = margin(0, 0, 0, 0))
  } else if (x == 4) {
    combined_plot <- plot_estacion[[1]] + plot_estacion[[2]] + 
      plot_estacion[[3]] + plot_estacion[[4]] + plot_layout(ncol = 2) + 
      theme(plot.margin = margin(0, 0, 0, 0))
    }
  
  ggsave(glue::glue('output/fig/analisis/correlacion_general/cor_shac_{shacs[i]}.png'), 
         width = 12, height = 12)
  
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
