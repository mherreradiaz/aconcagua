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
         mm = `Depth to water (m)`,
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
  reframe(n = length(mm[which(!is.na(mm))])) |> 
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
    reframe(n = length(mm[which(!is.na(mm))])) |> 
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
    reframe(n = length(mm[which(!is.na(mm))])) |> 
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
  reframe(n = length(mm[which(!is.na(mm))])) |> 
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
    reframe(n = length(mm[which(!is.na(mm))])) |> 
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


