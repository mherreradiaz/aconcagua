library(tidyverse)
library(glue)

dataset <- read_rds(glue('data/processed/rds/dataset_{buffer}.rds'))

predictores <- dataset |> 
  select(everything(),-c(shac:COB_INF),-contains('lag')) |> 
  names()

trend <- read_rds('data/processed/rds/pozo_trend.rds')

facet_labels <- setNames(c('-0.15 < S ≤ -0.1','-0.1 < S ≤ -0.05',
                           '-0.05 < S ≤ 0','0 < S ≤ 0.05'),
                         as.character(1:4))

data <- dataset |>
  select(shac,codigo,año,estacion,all_of(predictores)) |> 
  mutate(fecha = año + case_when(estacion == 'Verano' ~ (.5/12)*1,
                                 estacion == 'Otoño' ~ (.5/12)*7,
                                 estacion == 'Invierno' ~ (.5/12)*13,
                                 estacion == 'Primavera' ~ (.5/12)*19)) |> 
  left_join(trend) |> 
  suppressMessages()

data |> 
  ggplot(aes(fecha,SWEI)) +
  geom_line(aes(color = shac, group = as.factor(codigo)), alpha = .4,
            linewidth = .5) +
  geom_smooth(color = 'black') +
  facet_wrap(~class, labeller = labeller(class = facet_labels)) +
  theme_bw() +
  theme(strip.background = element_rect(fill='white')) +
  labs(x = NULL, y = 'GWI')
