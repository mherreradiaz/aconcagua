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
  filter(between(año,1983,2021)) |>
  mutate(n2000_estacion = ifelse(año >= 2000,n_estacion,NA)) |> 
  group_by(shac,codigo) |> 
  reframe(n = length.if(n_estacion,.>2),
          n2000 = length.if(n2000_estacion,!is.na(.) & .>2)) |> 
  filter(n > 39*.5, # mitad de años entre 1982 - 2022
         n2000 > 22*.75) |> # más del 75% entre 2000 - 2022  
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
  filter(codigo %in% filtro$codigo)

write_rds(data_filtrada,'data/processed/rds/pozo_estacion_filtrado.rds')

##

#filtro tendencia

data_filtrada <- data_estacion |> 
  filter(codigo %in% filtro$codigo)

library(zyp)
library(Kendall)

sen <- function(x,y) {
  data_sen <- tibble(x,y)
  zyp.sen(y ~ x,data=data_sen)$coefficients
}
mk <- function(x,y) {
  x_complete <- seq(min(x),max(x))
  data_mk <- tibble(x = x_complete) |> 
    left_join(tibble(x,y)) |> 
    suppressMessages()
  as.numeric(MannKendall(data_mk$y)$sl)
}

data_trend <- data_filtrada |> 
  na.omit() |>
  group_by(shac,codigo) |>
  reframe(año_slope = sen(año,m)[2]) |>
  mutate(class = cut(año_slope, breaks = c(-Inf,0,.5,1,Inf), right = FALSE, labels = FALSE))

data_trend |> 
  ggplot(aes(x = año_slope)) +
  geom_histogram(breaks = c(-1,0,.25,.5,.75,1), color = 'black') +
  facet_wrap(~shac) +
  theme_bw()

data_filtrada |> 
  left_join(data_trend) |> 
  mutate(año_estacion = factor(paste(año, estacion, sep = " "),
                               levels = unique(paste(año, estacion, sep = " ")))) |> 
  na.omit() |> 
  filter(año >= 2000) |> 
  ggplot(aes(año_estacion,m,color=factor(class))) +
  geom_point(aes(group = codigo)) +
  geom_line(aes(group = codigo)) +
  facet_wrap(~shac, scales = 'free_y') +
  scale_x_discrete(breaks = paste0(c(1982,1990,2000,2010,2022),' Verano'),
                   labels = c(1982,1990,2000,2010,2022)) +
  scale_color_manual(values = setNames(c('dodgerblue3','gold2','orange','red2'),1:4),
                     labels = c('< 0','0 - 0.5','0.5 - 1','> 1')) +
  labs(x = 'año', color = "sen's slope trend") +
  theme_bw()
