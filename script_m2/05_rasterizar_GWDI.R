library(terra)
library(tidyterra)

cob <- rast('data/processed/raster/cobertura/cobertura.tif')

points <- read_rds('data/processed/rds/gwdi.rds') |> 
  vect(geom = c('lon', 'lat'), crs = 'EPSG:4326') |> 
  project(cob)

shac <- vect('data/raw/vectorial/SHAC_Aconcagua.shp') |>
  select(shac = OBJECTID) |>
  rasterize(cob,field = 'shac',fun =max)

año_estacion <- points |> 
  as_tibble() |> 
  distinct(año,estacion) |> 
  arrange(año,estacion)

gwdi_shac[[1]] <- lapply(216, \(x) {
  shac_x <- classify(shac,matrix(c(x,1),ncol=2),others = NA)
  voronoi_x <- lapply(1:nrow(año_estacion),\(y) {
    y_points <- points |> 
      filter(año == año_estacion[y,]$año, estacion == año_estacion[y,]$estacion)
    if (all(is.na(y_points$GWDI))) {
      r <- init(shac_x, fun = NA)
    } else {
      r <- voronoi(y_points) |> 
        rasterize(shac_x, field = 'GWDI')
    }
    names(r) <- paste0(año_estacion[y,]$año,' ',año_estacion[y,]$estacion)
    return(r)
  }) |> 
    rast()
})

writeRaster(gwdi_shac[[1]][[1]],'prueba.tif')
