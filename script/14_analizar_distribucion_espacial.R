source('script/00_setup.R')

# correlación

cob <- rast('data/processed/raster/cobertura/COB.tif')
cor_files <- list.files('data/processed/raster/correlacion/',full.names=T)

nombre <- sub(".*/([^/]+)\\.tif$", "\\1", cor_files)

cor_r <- rast(cor_files[i])

data <- as_tibble(values(cob)) |> 
  bind_cols(as_tibble(values(cor_r))) |> 
  na.omit() |> 
  separate(shac,1, into = c('cob','shac')) |> 
  mutate(cob = ifelse(cob == 1,'AG','NV'),
         sign = ifelse(p_value < .05,'significativa','no significativa'),
         sign = factor(sign,levels = c('significativa','no significativa'))) |> 
  distinct()

if (nrow(data) == 18) {

  data |> 
    ggplot(aes(shac,r,fill = sign)) +
    geom_col() +
    ylim(-1,1) +
    geom_abline(slope = 0, intercept = 0, linetype = "dashed", col = 'grey60') +
    facet_grid(~cob) +
    labs(y = 'Pearson r', fill = NULL, title = nombre[i]) +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white'))
  
} else {
  
  data |> 
    filter(sign == 'significativa') |> 
    ggplot(aes(shac,r,fill = cob)) +
    geom_violin(position = position_dodge(width = 0.75)) +
    ylim(-1,1) +
    geom_abline(slope = 0, intercept = 0, linetype = "dashed", col = 'grey60') +
    labs(y = 'Pearson r', fill = 'cobertura', title = nombre[i]) +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white'))
  
}
