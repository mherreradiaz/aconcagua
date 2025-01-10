library(tidyverse)
library(readxl)

data <- read_rds('data/processed/rds/pozo_periodo_estacion.rds') |> 
  filter(periodo == '1982-2022')

#filtro n

library(viridis)
library(ggforce)

col_values <- c("white", rev(viridis(4)))
names(col_values) = 0:4

data_n <- data |> 
  mutate(m = ifelse(!is.na(m),1,NA)) |>
  group_by(año,shac,codigo) |> 
  reframe(n = sum(m,na.rm=T))

data_n |> 
  ggplot(aes(año,codigo,fill=factor(n,level = 4:0))) +
  geom_tile(color = 'black') +
  scale_fill_manual(values = col_values) +
  facet_col(vars(shac), scales = "free_y", space = "free", strip.position = 'left') +
  theme_bw() +
  labs(x = NULL, fill = NULL,y = 'well code') +
  scale_x_continuous(breaks = c(1982, 1990, 2000, 2010, 2022), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0))

length.if <- function(vector, condition) {
  if (missing(condition)) {
    stop("Debes especificar una condición.")
  }
  condition_expr <- substitute(condition)
  filtered <- vector[eval(condition_expr, envir = list(. = vector))]
  return(length(filtered))
}

data_porcentaje <- data_n |> 
  mutate(n2000 = ifelse(año >= 2000,n,NA)) |> 
  group_by(shac,codigo) |> 
  reframe(años_con_mas_de_uno = length.if(n,.>1)/41,
          años2000_con_mas_de_uno = length.if(n2000,.>1 & !is.na(.))/22)

data_porcentaje |> 
  ggplot(aes(x = años_con_mas_de_uno)) +
  geom_histogram(breaks = seq(0,1,by=.05), color = 'black') +
  facet_wrap(~shac, scales = "free_y", ncol=3) +
  theme_bw()+
  scale_y_continuous(limits = c(0,5)) +
  scale_x_continuous(breaks = (0:10/10)) +
  geom_vline(xintercept = c(.5,.75,.8), linetype = 'dashed', col = 'red') +
  geom_text(data = data.frame(x = c(.5,.75,.8), y= 5), 
            aes(x = x,, y = y, label = x), hjust = -.2, size = 3, vjust = -.3)

data_porcentaje |> 
  filter(años_con_mas_de_uno > .5,
         años2000_con_mas_de_uno > .75) |>
  ggplot(aes(x = años2000_con_mas_de_uno)) +
  geom_histogram(breaks = seq(0,1,by=.05), color = 'black') +
  facet_wrap(~shac, ncol=3) +
  theme_bw() +
  # scale_y_continuous(limits = c(0,5)) +
  # scale_x_continuous(breaks = (0:10/10)) +
  geom_vline(xintercept = c(.5,.75), linetype = 'dashed', col = 'red') +
  geom_text(data = data.frame(x = c(.5,.75), y= 5), 
            aes(x = x,, y = y, label = x), hjust = -.2, size = 3, vjust = -.3) +
  labs(title = 'más de 50% de años con más de un dato estacional entre 1982-2022 y más del 75% entre 2000-2022',
       y = 'frecuencia', x = '% años con más de un dato estacional entre 2000-2022')

filtro_n <- data_porcentaje |> 
  filter(años_con_mas_de_uno > .5,
         años2000_con_mas_de_uno > .75) |> 
  select(shac,codigo)

#filtro tendencia

data_filtrada <- data |> 
  filter(codigo %in% filtro_n$codigo)

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
  
