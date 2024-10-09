source('script/00_setup.R')
library(tidyterra)
library(ggh4x)
library(ggforce)

ndvi <- rast(list.files('data/raw/raster/indices/',full.names=T)[19])
cob <- rast('data/processed/raster/cobertura/COB.tif') |> 
  as.polygons()

extract <- extract(ndvi,cob,fun=function(x){mean(x,na.rm=T)}) |>
  mutate(cob = cob |> pull(shac),
         .before = ID) |> 
  select(-ID)

names(extract)[2:ncol(extract)] <- 2001:2021

data <- extract |>
  pivot_longer(cols=2:ncol(extract),names_to = 'año',values_to = 'zcNDVI') |> 
  mutate(año = as.numeric(año),
         shac = substr(cob,2,4),
         codigo = cob,
         cob = substr(codigo,1,1),
         cob = ifelse(cob == 1,'AG','NV')) |>
  pivot_longer(cols=zcNDVI,names_to = 'index',values_to = 'value') |> 
  mutate(value = ifelse(is.na(value),NA,value),
         index = as.factor(index)) |> 
  select(año,shac,cob,codigo,everything())
  
write_rds(data,'data/processed/rds/ndvi.rds')

# graficar

data <- read_rds('data/processed/rds/ndvi.rds')

cob_unique <- data$cob |> unique()

for (x in seq_along(cob_unique)) {
  
  data |> 
    filter(cob == cob_unique[x]) |> 
    ggplot(aes(año,value)) +
    geom_link2(aes(colour = after_stat(ifelse(y > 0, '+', '-'))),alpha = .7) +
    ggh4x::stat_difference(aes(ymin = 0, ymax = value),alpha = .7) +
    facet_wrap(~cob,ncol=1) +
    labs(y = 'zcNDVI', x = NULL) +
    facet_wrap(~shac) +
    theme_bw() +
    theme(strip.background = element_rect(fill='white'),
          legend.position = 'none') +
    scale_fill_manual(values = c('-' = 'firebrick2','+' = 'dodgerblue3')) +
    scale_color_manual(values = c('-' = 'firebrick2','+' = 'dodgerblue3'))
  
  ggsave(glue::glue('output/fig/index/ts/{cob_unique[x]}/zcNDVI.png'), width = 14, height = 8)
  
}
