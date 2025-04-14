library(tidyverse)
library(viridis)
library(ggforce)

length.if <- function(vector, condition) {
  if (missing(condition)) {
    stop("Debes especificar una condición.")
  }
  condition_expr <- substitute(condition)
  filtered <- vector[eval(condition_expr, envir = list(. = vector))]
  return(length(filtered))
}

data_estacion <- read_rds('data/processed/rds/pozo_estacion.rds')

data_n_año_estacion <- data_estacion |>
  group_by(año,shac,codigo) |> 
  reframe(n_estacion = length.if(m,!is.na(.)))

filtro <- data_n_año_estacion |> 
  group_by(shac,codigo) |> 
  reframe(n_anual = length.if(n_estacion,!is.na(.) & .>2)) |> 
  filter(n_anual > 22*.75) |> # más del 75% entre 2000 - 2022  
  distinct(shac,codigo)

data_n_año_estacion |> 
  filter(año <=2022,
         codigo %in% filtro$codigo) |> 
  ggplot(aes(año,as.factor(codigo),fill=factor(n_estacion,level=4:0))) +
  geom_tile(color='black') +
  scale_fill_manual(values = setNames(c("white", rev(viridis(4))),0:4)) +
  facet_col(vars(shac), scales = "free_y", space = "free", strip.position = 'left') +
  theme_bw() +
  labs(x = NULL, fill = 'seasonal data count per year',y = 'well code') +
  scale_x_continuous(breaks = c(1982, 1990, 2000, 2010, 2022), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  guides(fill = guide_legend(title.hjust = 0.5))

data_filtrada <- data_estacion |> 
  filter(codigo %in% filtro$codigo) |> 
  mutate(m = -m)

write_rds(data_filtrada,'data/processed/rds/pozo_estacion_filtrado.rds')

#analisis tendencia

data_pozos <- read_rds('data/processed/rds/pozo_estacion_filtrado.rds') |> 
  group_by(shac,codigo,estacion) |> 
  mutate(m = as.numeric(scale(m))) |> 
  ungroup()

library(zyp)
library(Kendall)

mk <- function(x,y) {
  x_complete <- seq(min(x),max(x))
  y <- tibble(x = x_complete) |> 
    left_join(tibble(x,y)) |> 
    suppressMessages() |> 
    pull(y)
  tau = as.numeric(MannKendall(y)$tau)
  sl = as.numeric(MannKendall(y)$sl)
  
  return(list('tau' = tau, 'sl' = sl))
}
sen <- function(x,y) {
  data_sen <- tibble(x,y)
  ss <- as.numeric(zyp.sen(y ~ x,data=data_sen)$coefficients)
  
  return(list('int' = ss[1], 'slope' = ss[2]))
}

data_trend <- data_pozos |> 
  na.omit() |>
  group_by(shac,codigo,estacion) |>
  reframe(mk_p_estacional = mk(año,m)$sl,
          sen_slope_estacional = sen(año,m)$slope) |> 
  group_by(shac,codigo) |> 
  reframe(mk_p = max(mk_p_estacional,na.rm=T),
          sen_slope = median(sen_slope_estacional,na.rm=T)) |> 
  mutate(class = cut(sen_slope,seq(-.15,.05,by=.05),right=F,labels=F))

write_rds(data_trend,'data/processed/rds/pozo_trend.rds')
write_csv(data_trend,'data/processed/csv/pozo_trend.csv')

#visualizar

data_pozos <- read_rds('data/processed/rds/pozo_estacion_filtrado.rds') |> 
  group_by(shac,codigo,estacion) |> 
  mutate(m = as.numeric(scale(m))) |> 
  ungroup()

data_trend <- read_rds('data/processed/rds/pozo_trend.rds')

data <- data_pozos |>
  select(año,estacion,shac,codigo,m) |> 
  left_join(data_trend)

facet_labels <- setNames(c('-0.15 < S ≤ -0.1','-0.1 < S ≤ -0.05',
                           '-0.05 < S ≤ 0','0 < S ≤ 0.05'),
                         as.character(1:4))

data |> 
  mutate(fecha = año + case_when(estacion == 'Verano' ~ (.5/12)*1,
                                 estacion == 'Otoño' ~ (.5/12)*7,
                                 estacion == 'Invierno' ~ (.5/12)*13,
                                 estacion == 'Primavera' ~ (.5/12)*19)) |> 
  ggplot(aes(fecha,m)) +
  geom_line(aes(color = shac, group = as.factor(codigo)), alpha = .4,
            linewidth = .5) +
  geom_smooth(color = 'black') +
  facet_wrap(~class, labeller = labeller(class = facet_labels)) +
  theme_bw() +
  theme(strip.background = element_rect(fill='white')) +
  labs(x = NULL, y = 'scaled well depth (m)')

ggsave('output/fig/series/well_depth.png',width = 11.78,height=7.42)

data |> 
  mutate(fecha = año + case_when(estacion == 'Verano' ~ (.5/12)*1,
                                 estacion == 'Otoño' ~ (.5/12)*7,
                                 estacion == 'Invierno' ~ (.5/12)*13,
                                 estacion == 'Primavera' ~ (.5/12)*19)) |> 
  ggplot(aes(fecha,m, color = as.factor(class))) +
  geom_smooth(aes(group = class),se = F, linewidth = .7) +
  scale_color_discrete(labels = facet_labels) +
  theme_bw() +
  labs(x = NULL, y = 'scaled well depth (m)', color = 'tendency groups')

ggsave('output/fig/series/well_depth_smooth.png',width = 11.78*.7,height=7.42*.7)
