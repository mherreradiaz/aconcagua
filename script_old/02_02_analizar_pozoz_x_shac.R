source('scripts/00_setup.R')
library(patchwork)

gw <- read_xlsx('data/raw/tabulada/gw_chile.xlsx')
pozos <- values(vect('data/raw/vectorial/pozos.shp')) |> 
  select(-field_1,-Longitude_,-Latitude_G)

data <- gw |> 
  left_join(pozos,relationship = "many-to-many") |>
  mutate(fecha = Date_String,
         shac = ID_IDE,
         codigo = Code,
         nombre = Name,
         m = `Depth to water (m)`,
         lon = Longitude_GCS_WGS_1984,
         lat = Latitude_GCS_WGS_1984) |> 
  select(fecha:lat) |> 
  filter(!is.na(shac)) |> 
  arrange(shac,codigo,fecha)

data_completa <- data |> 
  distinct(shac,codigo,nombre) |> 
  group_by(shac,codigo,nombre) |> 
  reframe(año = min(year(data$fecha)):max(year(data$fecha))) |>
  group_by(across(shac:año)) |> 
  reframe(mes = 1:12) |> 
  left_join(data |> 
              mutate(año = year(fecha), mes = month(fecha)) |> 
              select(-fecha,-lon,-lat)) |> 
  mutate(shac = as.factor(shac),
         codigo = as.factor(codigo))

# analisis por año-mes

data_año <- data_completa |> 
  group_by(across(shac:mes)) |> 
  reframe(n = length(m[which(!is.na(m))])) |> 
  group_by(across(shac:año)) |> 
  reframe(n = length(n[which(n > 0)]))
  
data_año_todo <- data_año |>  
  mutate(clasificacion_n = 
           factor(case_when(n == 0 ~ '0',
                            n >= 1 & n < 3 ~ 'Entre 1 y 3',
                            n >= 3 & n < 6 ~ "Entre 3 y 6",
                            n >= 6 & n < 9 ~ "Entre 6 y 9",
                            n >= 9 & n < 12 ~ "Entre 9 y 12",
                            n == 12 ~ "12"),
                  levels=c("0",'Entre 1 y 3','Entre 3 y 6',"Entre 6 y 9","Entre 9 y 12","12")))

pal <- rev(hcl.colors(length(unique(data_año_todo$clasificacion_n)), palette = "Blues 3"))
pal[1] <- 'white'

data_año_todo |> 
  ggplot(aes(año, codigo, fill = clasificacion_n)) +
  geom_tile() +
  scale_fill_manual(values = pal) +
  facet_wrap(~shac, scales = "free_y") +
  labs(title = 'Cantidad de meses con datos por año' ,
       x = "año", y = "código_pozo", fill = "n") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(hjust = 0.5))
ggsave('output/figs/datos_por_año_mes.png', width = 13, height = 7)

data_6 <- data_año |> 
  mutate(clasificacion_n = 
           factor(case_when(n < 6 ~ "< 6",
                            n >= 6 & n < 9 ~ "Entre 6 y 9",
                            n >= 9 & n < 12 ~ "Entre 9 y 12",
                            n == 12 ~ "12"),
                  levels=c("< 6","Entre 6 y 9","Entre 9 y 12","12")))

pal <- rev(hcl.colors(length(unique(data_6$clasificacion_n)), palette = "Blues 3"))
pal[1] <- 'white'

data_6 |> 
  ggplot(aes(año, codigo, fill = clasificacion_n)) +
  geom_tile() +
  scale_fill_manual(values = pal) +
  facet_wrap(~shac, scales = "free_y") +
  labs(title = 'Cantidad de meses con datos por año' ,
       x = "año", y = "código_pozo", fill = "n") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(hjust = 0.5))
ggsave('output/figs/datos_por_año_mes_mayor6.png', width = 13, height = 7)

# por mes

meses <- c('Enero','Febrero','Marzo','Abril','Mayo','Junio','Julio',
           'Agosto','Septiembre','Octubre','Noviembre','Diciembre')

for (i in seq_along(meses)) {
  
  data_mes <- data_completa |>
    group_by(across(shac:mes)) |> 
    reframe(n = length(m[which(!is.na(m))])) |> 
    filter(mes == i) |> 
    mutate(n = as.factor(n))
  
  pal <- rev(hcl.colors(length(unique(data_mes$n)), palette = "Blues 3"))
  pal[1] <- 'white'
  
  data_mes |> 
    ggplot(aes(año,codigo,fill=n)) +
    geom_tile() +
    scale_fill_manual(values = pal) +
    facet_wrap(~shac, scales = "free_y") +
    labs(title = glue('Cantidad de datos en {meses[i]} por año'),
         x = "año", y = "código_pozo", fill = "n") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(hjust = 0.5))
  ggsave(paste0('output/figs/mes/',sprintf('%02d',i),'_',meses[i],'.png'), width = 13, height = 7)
}

# por estacion

estaciones <- c('Verano','Otoño','Invierno','Primavera')

for (i in seq_along(estaciones)) {
  
  data_estacion <- data_completa |>
    mutate(estacion = factor(case_when(
      mes %in% c(12, 1, 2) ~ "Verano",
      mes %in% c(3, 4, 5) ~ "Otoño",
      mes %in% c(6, 7, 8) ~ "Invierno",
      mes %in% c(9, 10, 11) ~ "Primavera"), levels=estaciones),
      .before = mes) |> 
    mutate(año = ifelse(mes == 12,año+1,año)) |> 
    group_by(across(shac:estacion)) |> 
    reframe(n = length(m[which(!is.na(m))])) |> 
    filter(estacion == estaciones[i]) |> 
    mutate(n = as.factor(n))
  
  pal <- rev(hcl.colors(length(unique(data_estacion$n)), palette = "Blues 3"))
  pal[1] <- 'white'
  
  data_estacion |> 
    ggplot(aes(año,codigo,fill=n)) +
    geom_tile() +
    scale_fill_manual(values = pal) +
    facet_wrap(~shac, scales = "free_y") +
    labs(title = glue('Cantidad de datos en {estaciones[i]} por año'),
         x = "año", y = "código_pozo", fill = "n") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(hjust = 0.5))
  ggsave(paste0('output/figs/estacion/',sprintf('%02d',i),'_',estaciones[i],'.png'), width = 13, height = 7)
}

# shac estacion

estaciones <- c('Verano','Otoño','Invierno','Primavera')

data_shac <- data_completa |>
  mutate(estacion = factor(case_when(
    mes %in% c(12, 1, 2) ~ "Verano",
    mes %in% c(3, 4, 5) ~ "Otoño",
    mes %in% c(6, 7, 8) ~ "Invierno",
    mes %in% c(9, 10, 11) ~ "Primavera"), levels=estaciones),
    .before = mes) |> 
  mutate(año = ifelse(mes == 12,año+1,año)) |> 
  group_by(across(shac:estacion)) |> 
  reframe(n = length(m[which(!is.na(m))])) |> 
  mutate(n = as.numeric(as.character(n))) |> 
  filter(año >= 2001) |>
  mutate(n = ifelse(n>0,1,NA)) |> 
  group_by(across(shac:estacion)) |> 
  reframe(n = mean(n,na.rm=F)) |> 
  group_by(shac,codigo,nombre,estacion) |> 
  reframe(n = sum(n,na.rm=T)) |> 
  mutate(label = ifelse(n == 21,'*',""))

shac_name <- unique(data_shac$shac)

plot <- list()

for (i in seq_along(shac_name)) {
  
  plot[[i]] <- data_shac |> 
    filter(shac == shac_name[i]) |> 
    ggplot(aes(codigo,n,fill=estacion)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = shac_name[i], x = 'codigo_pozo') +
    ylim(0,21) +
    geom_text(aes(codigo, 20, label = label), 
              size = 5, 
              position = position_dodge(width = 0.9), 
              vjust = -0.5) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none")
  
  if (shac_name[i]== 'SHAC_220') {
    
    plot[[i]] <- data_shac |> 
      filter(shac == shac_name[i]) |> 
      ggplot(aes(codigo,n,fill=estacion)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = shac_name[i], x = 'codigo_pozo') +
      ylim(0,21) +
      geom_text(aes(codigo, 20, label = label), 
                size = 5, 
                position = position_dodge(width = 0.9), 
                vjust = -0.5) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white", color = NA),
            plot.title = element_text(hjust = 0.5),
            legend.position = "bottom", 
            legend.direction = "horizontal")
  }
}

(plot[[1]]+(plot[[3]]+plot[[4]]))/(plot[[2]]+(plot[[7]]+plot[[8]]))/(plot[[6]]+plot[[9]])/plot[[5]]
ggsave('output/figs/estacion/2001-2021.png', width = 15, height = 10)


# por semestre

semestres <- c('Primer','Segundo')

for (i in seq_along(semestres)) {
  
  data_semestre <- data_completa |>
    mutate(semestre = factor(case_when(
      mes %in% c(1:6) ~ "Primer",
      mes %in% c(7:12) ~ "Segundo"), levels=semestres),
      .before = mes) |> 
    group_by(across(shac:semestre)) |> 
    reframe(n = length(m[which(!is.na(m))])) |> 
    filter(semestre == semestres[i]) |> 
    mutate(n = as.factor(n))
  
  pal <- rev(hcl.colors(length(unique(data_semestre$n)), palette = "Blues 3"))
  pal[1] <- 'white'
  
  data_semestre |> 
    ggplot(aes(año,codigo,fill=n)) +
    geom_tile() +
    scale_fill_manual(values = pal) +
    facet_wrap(~shac, scales = "free_y") +
    labs(title = glue('Cantidad de datos en el {semestres[i]} Semestre por año'),
         x = "año", y = "código_pozo", fill = "n") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(hjust = 0.5))
  ggsave(paste0('output/figs/semestre/',sprintf('%02d',i),'_',semestres[i],'.png'), width = 13, height = 7)
}

# analizar valores de m por shac/estacion ####

estaciones <- c('Verano','Otoño','Invierno','Primavera')

data_estacion_raw <- data |> 
  mutate(año = year(fecha),
         mes = month(fecha),
         estacion = factor(case_when(
           mes %in% c(12, 1, 2) ~ "Verano",
           mes %in% c(3, 4, 5) ~ "Otoño",
           mes %in% c(6, 7, 8) ~ "Invierno",
           mes %in% c(9, 10, 11) ~ "Primavera"), levels=estaciones),
         año = ifelse(mes == 12,año+1,año))

data_n <- data_estacion_raw |> 
  filter(año >= 2001) |>
  select(-c(fecha,nombre,lon,lat,mes)) |> 
  distinct(shac,codigo,año,estacion,.keep_all=T) |> 
  group_by(shac,codigo,estacion) |> 
  reframe(n = length(m[which(!is.na(m))]))

data_estacion <- data_estacion_raw |>
  complete(año = min(data_estacion_raw$año):max(data_estacion_raw$año), 
           nesting(shac, codigo, nombre, estacion), fill = list(m = NA, n = 0)) |> 
  group_by(shac,codigo,nombre,año,estacion) |> 
  reframe(m = mean(m,na.rm=T)) |> 
  left_join(data_n) |> 
  mutate(shac = as.factor(shac),
         codigo = as.factor(codigo))
  # group_by(shac,nombre,estacion) |>
  # mutate(m = scale(m)[,1]) |>
  # ungroup()

# series temporales
  
plot_estacion <- list()

for (x in seq_along(estaciones)) {
  
  plot_shac <- list()
  
  for (i in seq_along(unique(data_estacion$shac))) {
    
    data_shac <- data_estacion |> 
      filter(between(año,2000,2022),
             estacion == estaciones[x],
             n >= n_min,
             shac == unique(data_completa$shac)[i])
    
    if (nrow(data_shac) == 0) {
      plot_shac[[i]] <- ggplot() +
        labs(title = paste("No data para", unique(data_completa$shac)[i])) +
        theme_minimal()
    } else {
      plot_shac[[i]] <- data_shac |>  
      ggplot(aes(año,m,color=codigo)) +
      geom_line(alpha=.5,linewidth=.7) +
      geom_point(size = 1) +
      facet_wrap(~shac, strip.position = "right") +
      # labs(y='standardized m') +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white", color = NA),
            plot.title = element_text(hjust = 0.5),
            legend.position = "none")
    }
  }
  
  (plot_shac[[1]]+plot_shac[[2]]+plot_shac[[3]])/
    (plot_shac[[4]]+plot_shac[[5]]+plot_shac[[6]])/
    (plot_shac[[7]]+plot_shac[[8]]+plot_shac[[9]]) +
    plot_annotation(
      title = estaciones[x],
      theme = theme(plot.title = element_text(hjust = 0.5, size = 17)))
  
  ggsave(paste0('output/figs/analisis_m/mayores_',n_min,'/series/',estaciones[x],'.png'), width = 14, height = 9)
  # ggsave(paste0('output/figs/analisis_m/mayores_',n_min,'/series_std/',estaciones[x],'.png'), width = 14, height = 9)
  
}

# boxplot

plot_estacion <- list()

for (x in seq_along(estaciones)) {
  
  plot_shac <- list()
  
  for (i in seq_along(unique(data_estacion$shac))) {
    
    data_shac <- data_estacion |> 
      filter(between(año,2000,2022),
             estacion == estaciones[x],
             n >= n_min,
             shac == unique(data_completa$shac)[i]) |> 
      mutate(año = factor(año))
    
    if (nrow(data_shac) == 0) {
      plot_shac[[i]] <- ggplot() +
        labs(title = paste("No data para", unique(data_completa$shac)[i])) +
        theme_minimal()
    } else {
      plot_shac[[i]] <- data_shac |> 
        ggplot(aes(x = año, y = m)) +
        geom_boxplot() +
        scale_x_discrete(breaks = c("2000", "2005", "2010", "2015", "2020")) +
        facet_wrap(~shac, strip.position = "right") +
        # labs(y='standardized m') +
        theme_minimal() +
        theme(plot.background = element_rect(fill = "white", color = NA),
              plot.title = element_text(hjust = 0.5),
              legend.position = "none")
    }
  }
  
  (plot_shac[[1]]+plot_shac[[2]]+plot_shac[[3]])/
    (plot_shac[[4]]+plot_shac[[5]]+plot_shac[[6]])/
    (plot_shac[[7]]+plot_shac[[8]]+plot_shac[[9]]) +
    plot_annotation(
      title = estaciones[x],
      theme = theme(plot.title = element_text(hjust = 0.5, size = 17)))
  
  ggsave(paste0('output/figs/analisis_m/mayores_',n_min,'/boxplot/',estaciones[x],'.png'), width = 14, height = 9)
  # ggsave(paste0('output/figs/analisis_m/mayores_',n_min,'/boxplot_std/',estaciones[x],'.png'), width = 14, height = 9)
  
}

# mean y error

plot_estacion <- list()

for (x in seq_along(estaciones)) {
  
  plot_shac <- list()
  
  for (i in seq_along(unique(data_estacion$shac))) {
    
    data_shac <- data_estacion %>%
      filter(between(año, 2000, 2022),
             estacion == estaciones[x],
             n >= n_min,
             shac == unique(data_completa$shac)[i]) |> 
      mutate(año = factor(año))
    
    if (nrow(data_shac) == 0) {
      plot_shac[[i]] <- ggplot() +
        labs(title = paste("No data para", unique(data_completa$shac)[i])) +
        theme_minimal()
    } else {
      summary_stats <- data_shac %>%
        group_by(año) %>%
        summarize(
          mean_m = mean(m, na.rm = TRUE),
          sd_m = sd(m, na.rm = TRUE),
          .groups = 'drop'
        )
      
      plot_shac[[i]] <- summary_stats %>%
        ggplot(aes(x = año, y = mean_m)) +
        geom_point(size = 1.5) +
        geom_errorbar(aes(ymin = mean_m - sd_m, ymax = mean_m + sd_m), width = 0.2) +
        scale_x_discrete(breaks = c("2000", "2005", "2010", "2015", "2020")) +
        facet_wrap(~unique(data_completa$shac)[i], strip.position = "right") +
        labs(title = unique(data_completa$shac)[i]) +
        # labs(y='standardized m') +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none"
        )
    }
  }
  
  combined_plot <- (plot_shac[[1]] + plot_shac[[2]] + plot_shac[[3]]) /
    (plot_shac[[4]] + plot_shac[[5]] + plot_shac[[6]]) /
    (plot_shac[[7]] + plot_shac[[8]] + plot_shac[[9]]) +
    plot_annotation(
      title = estaciones[x],
      theme = theme(plot.title = element_text(hjust = 0.5, size = 17))
    )
  
  ggsave(paste0('output/figs/analisis_m/mayores_',n_min,'/mean_sd/', estaciones[x], '.png'), plot = combined_plot, width = 14, height = 9)
  # ggsave(paste0('output/figs/analisis_m/mayores_',n_min,'/mean_sd_std/', estaciones[x], '.png'), plot = combined_plot, width = 14, height = 9)
  
}
