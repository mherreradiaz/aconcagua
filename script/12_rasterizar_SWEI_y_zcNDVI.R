source('script/00_setup.R')
library(tidyterra)

# SWEI

cob <- rast('data/processed/raster/cobertura/COB.tif')

data <- read_rds('data/processed/rds/dataset_limpio.rds') |> 
  mutate(codigo = as.numeric(paste0(ifelse(cob == 'AG',1,2),shac)),
         .before=ncGWDI) |> 
  filter(estacion == 'Primavera') |> 
  select(año:codigo,contains('SWEI'))

nombres <- data |> select(contains('SWEI')) |> names()

for (i in seq_along(nombres)) {
  
  data_var <- data |> 
    select(año:codigo,all_of(nombres[i]))
  
  años <- unique(data_var$año)
  
  r_stack <- lapply(años, function(x) {
    value <- data_var |> 
      filter(año == x) |> 
      pull(all_of(nombres[i])) |> 
      unique()
    ifel(!is.na(cob), value, NA)
  }) |> 
    rast()
  
  names(r_stack) <- años
  varnames(r_stack) <- nombres[i]
  
  r_stack <- project(r_stack,cob) 
  
  output_path <- glue::glue('data/processed/raster/indices/{nombres[i]}.tif')
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  writeRaster(r_stack, output_path, overwrite = TRUE)
  gc()
  
}

# NDVI

ndvi_r <- rast(list.files('data/raw/raster/indices/',full.names=T)[19])

ndvi <- rast(list(ifel(!is.na(ndvi_r[[1]]),NA,NA),
                  ndvi_r,
                  ifel(!is.na(ndvi_r[[1]]),NA,NA)))
names(ndvi) <- 2000:2022
varnames(ndvi) <- 'zcNDVI'

ndvi <- project(ndvi, cob)
ndvi <- mask(ndvi,cob)

writeRaster(ndvi,'data/processed/raster/indices/zcNDVI.tif',
            overwrite=T)

ndvi_lead <- rast(list(ndvi_r,
                       ifel(!is.na(ndvi_r[[1]]),NA,NA),
                       ifel(!is.na(ndvi_r[[1]]),NA,NA)))

names(ndvi_lead) <- 2000:2022
varnames(ndvi_lead) <- 'zcNDVI_lead'

ndvi_lead <- project(ndvi_lead, cob)
ndvi_lead <- mask(ndvi_lead,cob)

writeRaster(ndvi_lead,'data/processed/raster/indices/zcNDVI_lead.tif',
            overwrite=T)
