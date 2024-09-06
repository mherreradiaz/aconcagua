source('script/00_setup.R')

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

data <- read_rds('data/processed/rds/pozo_seleccion.rds')

data_sequia <- data |>
  group_by(periodo,estacion,shac,codigo) |>
  mutate(di = eddi(m)$EDDI) |> 
  ungroup() |> 
  select(año:m,di,lon,lat)

write_rds(data_sequia,'data/processed/rds/di_pozo_seleccion.rds')

data_sequia_shac <- data_sequia |>
  group_by(año,periodo,estacion,shac) |> 
  reframe(di_mean = mean(di,na.rm=T),
          di_sd = sd(di,na.rm=T))

write_rds(data_sequia_shac,'data/processed/rds/di_shac_seleccion.rds')

# graficar

data_sequia <- read_rds('data/processed/rds/di_pozo_seleccion.rds')
data_sequia_shac <- read_rds('data/processed/rds/di_shac_seleccion.rds')

periodos <- data$periodo |> unique()
estaciones <- data$estacion |> unique()

for (i in seq_along(periodos)) {
  for (x in seq_along(estaciones)) {

    data_sequia |> 
      filter(periodo == periodos[[i]],
             estacion == estaciones[[x]]) |> 
      ggplot(aes(año,di,color = as.factor(nombre))) +
      geom_point(alpha=.5) + 
      geom_line(alpha=.5) +
      geom_point(data=data_sequia_shac |> filter(periodo == periodos[[i]],estacion == estaciones[[x]]),
                 aes(año,di_mean,color='MEAN'),color='black') +
      geom_line(data=data_sequia_shac |> filter(periodo == periodos[[i]],estacion == estaciones[[x]]),
                aes(año,di_mean,color='MEAN'),color='black') +
      facet_wrap(~shac,scales = 'free_x') +
      theme_bw() +
      theme(legend.position = "none",
            strip.background = element_rect(fill = 'white')) +
      labs(title = glue::glue('Índice de sequía (DI) en {estaciones[[x]]} periodo {periodos[[i]]}'),
           y = 'DI', x = NULL)

    ggsave(glue::glue('output/fig/di/{periodos[[i]]}_{estaciones[[x]]}.png'), width = 12, height = 8)
    
  }
}
