source('script/00_setup.R')

cob <- rast('data/processed/raster/cobertura/cobertura.tif')

data <- read_rds('data/processed/rds/dataset_limpio.rds') |> 
  mutate(codigo = as.numeric(paste0(ifelse(cob == 'AG',1,2),shac)),
         .before=ncGWDI) |> 
  select(año:codigo,contains('ncGWDI'))

nombres <- data |> select(contains('ncGWDI')) |> names()

for (i in seq_along(nombres)) {
  
  print(nombres[i])
  
  data_var <- data |> 
    select(año:codigo,all_of(nombres[i]))
  
  estaciones <- unique(data_var$estacion)
  
  data_estacion <- data_var |> 
    filter(estacion == 'Primavera')
  
  años <- unique(data_estacion$año)
  
  r_stack <- lapply(años, function(x) {
    data_año <- data_estacion |> 
      filter(año == x)
    r <- classify(cob,data_año |> select(5:6))
    ifel(r > 1000, NA, r)
  }) |> 
    rast()
  
  names(r_stack) <- años
  varnames(r_stack) <- nombres[i]
  
  output_path <- glue::glue('data/processed/raster/indices/{nombres[i]}.tif')
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  writeRaster(r_stack, output_path, overwrite = TRUE)
  gc()
  
}
