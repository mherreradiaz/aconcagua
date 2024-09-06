source('script/00_setup.R')
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

data <- read_rds('data/processed/rds/pozo_periodo_estacion.rds')

estaciones <- as.character(unique(data$estacion))

data_trend_summary <- data |> 
  na.omit() |>
  group_by(periodo,estacion,shac,codigo) |>
  reframe(año_slope = sen(año,m)[2],
          sign = ifelse(length(as.numeric(na.omit(m))) > 5,
                        mk(año,m),1),
          sign_2 = ifelse(sign < .05,1,0),
          n = length(which(!is.na(m)))) |>
  rowwise() |> 
  mutate(años_totales = as.numeric(str_split(as.character(periodo),'-')[[1]][2])-
           as.numeric(str_split(as.character(periodo),'-')[[1]][1]) +1,
         percent = n/años_totales) |> 
  left_join(data |> distinct(periodo,estacion,shac,codigo,nombre,lon,lat)) |>
  select(periodo,estacion,shac,codigo,nombre,everything()) |> 
  arrange(periodo,shac,codigo,estacion)

write_rds(data_trend_summary,'data/processed/rds/pozo_trend_summary.rds')
write_csv(data_trend_summary,'data/processed/csv/pozo_trend_summary.csv')

# graficar puntos espacial

library(tidyterra)

data <- read_rds('data/processed/rds/pozo_trend_summary.rds')
shac <- vect('data/raw/vectorial/SHAC_Aconcagua.shp') |> 
  project('EPSG:4326')

periodos <- data$periodo |> unique()
estaciones <- data$estacion |> unique()

for (i in seq_along(periodos)) {
  for (x in seq_along(estaciones)) {
    data |> 
      vect(geom = c("lon", "lat"), crs = "EPSG:4326") |> 
      filter(periodo == periodos[[i]],
             estacion == estaciones[[x]],
             percent >= .5) |>
      ggplot() +
      geom_spatvector(data = shac) +
      geom_spatvector_text(data = shac, aes(label = ID_IDE), 
                           size = 3, color = "black", nudge_y = 0.001) +
      geom_spatvector(aes(fill = año_slope),shape=21,size = 2) +
      scale_fill_gradient2(    low = "blue",
                               mid = "white",
                               high = "red3",
                               midpoint = 0) +
      theme_bw() +
      labs(title = glue::glue('Pendientes (n >=  mitad de años) en {estaciones[[x]]} periodo {periodos[[i]]}'),
           fill = "Sen's slope",
           y = NULL, x = NULL)
    
    ggsave(glue::glue('output/fig/sen/{periodos[[i]]}_{estaciones[[x]]}.png'), width = 12, height = 8)
  }
}

# graficar barras 

data <- read_rds('data/processed/rds/pozo_trend_summary.rds')

for (i in seq_along(periodos)) {
  for (x in seq_along(estaciones)) {
    
    data |> 
      filter(periodo == periodos[[i]],
             estacion == estaciones[[x]],
             percent >= .5) |> 
      ggplot(aes(as.factor(codigo),año_slope)) +
      geom_col() +
      facet_wrap(~shac,scales='free') +
      theme_bw() +
      labs(title = glue::glue('Pendientes (n >=  mitad de años) en {estaciones[[x]]} periodo {periodos[[i]]}'),
           fill = "Sen's slope",
           y = NULL, x = NULL) +
      theme(strip.background = element_rect(fill = 'white'),
            axis.text.x = element_text(angle = 45,vjust = .6))

    ggsave(glue::glue('output/fig/bar/{periodos[[i]]}_{estaciones[[x]]}.png'), width = 12, height = 8)
    
  }
}


