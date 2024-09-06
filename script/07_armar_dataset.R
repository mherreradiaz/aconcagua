source('script/00_setup.R')
library(tidyterra)

# indices

cob <- rast('data/processed/raster/cobertura/COB.tif') |> 
  as.polygons()

swei <- read_xlsx('data/raw/tabulada/Aconcagua Alto_swei_1981-01-01-2024-04-01.xlsx')
fechas <- as.Date(swei$fecha)

r_files <- list.files('data/raw/raster/indices/',full.names=T)

lista <- lapply(r_files,function(x) {
  r <- rast(x)
  nombre <- sub(".*indices/(.*)\\.tif", "\\1",sources(r))
  extract <- extract(r,cob,fun=function(x){mean(x,na.rm=T)}) |>
    mutate(cob = cob |> pull(shac),
           .before = ID) |> 
    select(-ID)
  names(extract)[2:ncol(extract)] <- fechas
  extract |>
    pivot_longer(cols=2:ncol(extract),names_to = 'fecha',values_to = nombre) |> 
    mutate(fecha = as.Date(as.numeric(fecha)))
})

data <- reduce(lista, left_join, by = c("cob", "fecha")) |> 
  left_join(swei |> mutate(fecha = as.Date(fecha)) |> rename(SWEI = swei)) |> 
  select(fecha,cob,SWEI,everything()) |> 
  mutate(shac = substr(cob,2,4),
         codigo = cob,
         cob = substr(codigo,1,1),
         cob = ifelse(cob == 1,'AG','NV')) |> 
  pivot_longer(cols=`SWEI`:`zcSM-6`,names_to = 'index',values_to = 'value') |> 
  mutate(value = ifelse(is.na(value),NA,value),
         index = as.factor(index)) |> 
  select(fecha,shac,cob,codigo,everything())

write_rds(data,'data/processed/rds/dataset.rds')

# graficar

data <- read_rds('data/processed/rds/dataset.rds')

index_name <- data$index |> unique() |> 
  as.character() |> str_extract("^[^-]+") |> 
  unique()

index_unique <- data$index |> unique()

for (i in seq_along(index_unique)) {
  
  data |> 
    filter(year(fecha)>=2000) |> 
    # filter(grepl(index_name[i], index)) |>
    filter(index == index_unique[i]) |> 
    ggplot(aes(fecha,value,color=shac)) +
    geom_line(alpha = .5) +
    geom_point(size = .6) +
    facet_wrap(~cob,ncol=1) +
    labs(y = index_unique[i], x = NULL) +
    scale_x_date(date_breaks = '2 years',date_labels = '%Y') +
    theme_bw() +
    theme(strip.background = element_rect(fill='white'))
  
  ggsave(glue::glue('output/fig/index/ts/{index_unique[i]}.png'), width = 14, height = 8)
  
}


  

