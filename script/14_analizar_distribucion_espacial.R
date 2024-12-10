source('script/00_setup.R')

# correlación

cob <- rast('data/processed/raster/cobertura/cobertura.tif')
cor_files <- list.files('data/processed/raster/correlacion/',full.names=T)

nombres <- sub(".*/([^/]+)\\.tif$", "\\1", cor_files)



for (i in seq_along(nombres)) {
  
  cor_r <- rast(cor_files[i])
  
  data <- as_tibble(values(cob)) |> 
    bind_cols(as_tibble(values(cor_r))) |> 
    na.omit() |> 
    separate(id,1, into = c('cob','shac')) |> 
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
      labs(y = 'Pearson r', fill = NULL, title = nombres[i]) +
      theme_bw() +
      theme(strip.background = element_rect(fill = 'white'))
    
  } else {
    
    data |> 
      filter(sign == 'significativa') |> 
      ggplot(aes(shac,r,fill = cob)) +
      geom_violin(position = position_dodge(width = 0.75)) +
      ylim(-1,1) +
      geom_abline(slope = 0, intercept = 0, linetype = "dashed", col = 'grey60') +
      labs(y = 'Pearson r', fill = 'cobertura', title = nombres[i]) +
      theme_bw() +
      theme(strip.background = element_rect(fill = 'white'))
    
  }
  
  ggsave(glue::glue('output/fig/analisis/correlacion_espacial/{nombre[i]}.png'),
         width = 10,height =6)
  
}

