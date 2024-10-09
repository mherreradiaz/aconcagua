source('script/00_setup.R')
library(tidyterra)
library(tmap)
library(tmaptools)
library(leaflet)
library(RColorBrewer)

grupos <- list(c('ncGWDI','zcNDVI'),
               c('ncGWDI','zcNDVI_lead'),
               c('SWEI','zcNDVI'),
               c('SWEI','zcNDVI_lead'),
               c('ncGWDI','SWEI_lag_1'),
               c('ncGWDI','SWEI_lag_2'),
               c('ncGWDI','SWEI_lag_3'),
               c('ncGWDI','SWEI_lag_4'))

r_files <- list.files('data/processed/raster/indices/',full.names=T)

for (i in seq_along(grupos)) {
  
  r1 <- rast(grep(paste0(grupos[[i]][1],'.tif'),r_files,value=T))
  r2 <- rast(grep(paste0(grupos[[i]][2],'.tif'),r_files,value=T))
  
  cor_r <- app(c(r1, r2), fun = function(x) {
    serie1 <- x[1:23]
    serie2 <- x[24:46]
    
    completos <- complete.cases(serie1, serie2)
    
    if (sum(completos) > 2) {
      test <- cor.test(serie1[completos], serie2[completos], use = "complete.obs")
      r_value <- test$estimate
      p_value <- test$p.value   
      return(c(r_value, p_value))
    } else {
      return(c(NA, NA))
    }
  })
  
  names(cor_r) <- c('r','p_value')
  
  filename <- paste0(file_path_sans_ext(basename(sources(r1))),
                      '_x_',
                      file_path_sans_ext(basename(sources(r2))))
  
  writeRaster(cor_r,glue::glue('data/processed/raster/correlacion/{filename}.tif'),
              overwrite=T)
  
  gc()
  
}

# graficar

cor_files <- list.files('data/processed/raster/correlacion/',full.names=T)
nombres <- gsub('.tif','',list.files('data/processed/raster/correlacion/'))

shac <- vect('data/raw/vectorial/SHAC_Aconcagua.shp') |> 
  project('EPSG:4326') |> 
  st_as_sf()

for (i in seq_along(cor_files)) {
  
  tif <- rast(cor_files[i])
  
  coef <- tif[[1]]
  p <- ifel(tif[[2]] < .05,1,NA)
  
  r <- mask(coef,p)
  
  tmap_mode("view")

  tm <- tm_basemap(server = "Esri.WorldImagery") +
    tm_shape(shac) +                
    tm_fill(alpha = 0, id = "ID_IDE") +
    tm_borders(col = 'white', lwd = 1.5) +
    tm_shape(r) +                       
    tm_raster(palette = rev(brewer.pal(11, "RdBu")),
              breaks = seq(-1, 1, by = 0.5),
              style = "cont",
              alpha = 1,
              title = "r",
              legend.show = T)

  tmap_save(tm, glue::glue('output/html/correlacion/{nombres[i]}.html'))
  
}


