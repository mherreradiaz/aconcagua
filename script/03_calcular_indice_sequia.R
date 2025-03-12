library(tidyverse)
library(viridis)
library(ggforce)
library(scales)

pe <- function(x){
  if (!all(is.na(x))){
    r <- rank(x)
    out <- (r-0.33)/(length(x)+0.33)
  } else out <- rep(NA,length(x))
  out
}
eddi <- function(x){
  if (!all(is.na(x))){
    C0 = 2.515517
    C1 = 0.802853
    C2 = 0.010328
    d1 = 1.432788
    d2 = 0.189269
    d3 = 0.001308
    pe <- pe(x)
    
    data <- data.frame(x=x,pe=pe)
    
    out <- data |> 
      dplyr::mutate(W = dplyr::case_when(
        pe <= .5 ~ sqrt(-2*log(1-pe)),
        pe > .5 ~ sqrt(-2*log(pe))
      ),
      EDDI = W - (C0+C1*W+C2*W^2)/(1+d1*W+d2*W^2+d3*W^3),
      EDDI = dplyr::case_when(
        pe <= .5 ~ EDDI,
        pe > .5 ~ -EDDI)
      ) 
  } else out <- data.frame(EDDI = rep(NA,length(x)))
  
  return(out)
}

data <- read_rds('data/processed/rds/pozo_estacion_filtrado.rds')

data_index <- data |> 
  group_by(shac,codigo) |> 
  mutate(GWI = eddi(m)$EDDI) |> 
  ungroup() |> 
  mutate(GWI = ifelse(is.na(m),NA,GWI))

write_rds(data_index,'data/processed/rds/gwi.rds')

#visualizar

data_gwi <- read_rds('data/processed/rds/gwi.rds')
data_trend <- read_rds('data/processed/rds/pozo_trend.rds')

data <- data_gwi |>
  select(año,estacion,shac,codigo,m,GWI) |> 
  left_join(data_trend)

facet_labels <- setNames(c('-0.15 < S ≤ -0.1','-0.1 < S ≤ -0.05',
                           '-0.05 < S ≤ 0','0 < S ≤ 0.05'),
                         as.character(1:4))

data |> 
  mutate(fecha = año + case_when(estacion == 'Verano' ~ (.5/12)*1,
                                 estacion == 'Otoño' ~ (.5/12)*7,
                                 estacion == 'Invierno' ~ (.5/12)*13,
                                 estacion == 'Primavera' ~ (.5/12)*19)) |> 
  ggplot(aes(fecha,GWI)) +
  geom_line(aes(color = shac, group = as.factor(codigo)), alpha = .4,
            linewidth = .5) +
  geom_smooth(color = 'black') +
  facet_wrap(~class, labeller = labeller(class = facet_labels)) +
  theme_bw() +
  theme(strip.background = element_rect(fill='white')) +
  labs(x = NULL, y = 'GWI')

ggsave('output/fig/series/gwi.png',width = 11.78,height=7.42)

data |> 
  mutate(fecha = año + case_when(estacion == 'Verano' ~ (.5/12)*1,
                                 estacion == 'Otoño' ~ (.5/12)*7,
                                 estacion == 'Invierno' ~ (.5/12)*13,
                                 estacion == 'Primavera' ~ (.5/12)*19)) |> 
  ggplot(aes(fecha,GWI, color = as.factor(class))) +
  geom_smooth(aes(group = class),se = F, linewidth = .7) +
  scale_color_discrete(labels = facet_labels) +
  theme_bw() +
  labs(x = NULL, y = 'GWI', color = 'tendency groups')

ggsave('output/fig/series/gwi_smooth.png',width = 11.78*.7,height=7.42*.7)

## old

data_index <- read_rds('data/processed/rds/gwdi.rds')

data_index |> 
  mutate(año_estacion = as.numeric(factor(paste(año, estacion, sep = " "),
                               levels = unique(paste(año, estacion, sep = " "))))) |>
  ggplot(aes(año_estacion,as.factor(codigo),fill=GWDI)) +
  geom_tile() +
  scale_fill_viridis_c(na.value = "white") +
  facet_col(vars(shac), scales = "free_y", space = "free", strip.position = 'left') +
  theme_bw() +
  labs(x = NULL, fill = 'GWDI',y = 'well code') +
  scale_x_continuous(breaks = seq(1,164,by=32)-.5,
                     labels = seq(1982,2022,by=8),
                     expand = c(0,0)) +
  scale_y_discrete(expand = c(0, 0)) +
  geom_vline(xintercept = seq(1,164,by=4)-.5, color = "grey40") +
  guides(fill = guide_colorbar(title.hjust = 0.5))

col_values <- data_index |>
  distinct(shac,codigo) |> 
  group_by(shac) |> 
  mutate(col = hue_pal()(length(codigo))) |> 
  ungroup()
col_values <- setNames(col_values$col,col_values$codigo)

data_index |> 
  mutate(año_estacion = as.numeric(factor(paste(año, estacion, sep = " "),
                               levels = unique(paste(año, estacion, sep = " "))))) |>
  filter(shac == 221) |> 
  ggplot(aes(año_estacion,GWDI,color=as.factor(codigo),group=codigo)) +
  geom_smooth(se = FALSE,method = 'lm') +
  #geom_point() +
  #geom_line() +
  # facet_col(vars(shac), scales = "free_y", space = "free", strip.position = 'left') +
  scale_x_continuous(breaks = seq(1,164,by=32),
                     minor_breaks = seq(1,164,by=4),
                     labels = seq(1982,2022,by=8),
                     expand = c(.01,0)) +
  scale_color_manual(values = col_values) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(title = 221)

  