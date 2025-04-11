library(terra)
library(tidyterra)
library(tidyverse)
library(parallel)
ncores = detectCores()

#pozos

pozos <- read_rds('data/processed/rds/pozo_estacion_filtrado.rds') |> 
  distinct(shac,codigo,nombre,lon,lat) |> 
  mutate(codigo = as.integer(codigo)) |> 
  vect(geom = c("lon", "lat"), crs = "EPSG:4326") |> 
  project('EPSG:32719')

writeVector(pozos, 'data/processed/vectorial/pozo_filtrado.shp',
            overwrite=T)

#cobertura vegetacional

cob_files <- grep('aux',list.files('data/raw/raster/cobertura/biomas/',full.names=T),value=T,invert=T)

shac <- vect('data/processed/vectorial/shac.shp')

cob_r <- rast(cob_files) |>
  classify(tibble(c(3,12,66,21,24),c(2,2,2,1,3)),others = NA)

r_base <- rast(list.files('data/raw/raster/indices/zcNDVI/zcNDVI-1/',full.names=T)[10]) |> 
  crop(shac)
values(r_base) <- 1
r_base <- r_base |> 
  mask(shac)

lapply(cob_files, \(tif) {
  
  año <- str_extract(tif, "\\d{4}")
  
  rast(tif) |> 
    crop(project(shac,'EPSG:4326')) |> 
    project('EPSG:32719',method = 'near') |> 
    mask(shac) |> 
    writeRaster(glue('data/processed/raster/cobertura/biomas/biomas_{año}.tif'))
  
})



r_base_disag <- disagg(r_base,method='near',fact = 20)

cob_r_disag <- project(cob_r,r_base_disag,method = 'near')
 
cob_r_proj <- aggregate(cob_r_disag, fact = 20, cores = 7, fun = \(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA)
  }
  t <- table(x)
  l <- sum(t)
  per <- t / l
  max_val <- names(which.max(per))
  max_per <- max(per)
  if (max_per >= 0.75) {
    return(as.numeric(max_val))
  } else {
    return(NA)
  }
})

writeRaster(cob_r_proj,'data/processed/raster/cobertura/cob_resampled.tif')

#tipo vegetacional

cob_r <- rast('data/processed/raster/cobertura/cob_resampled.tif')

cob_1 <- ifel(cob_r == 1,1,NA) |>
  app(fun=sum,na.rm=T)/23
cob_2 <- ifel(cob_r == 2,1,NA) |>
  app(fun=sum,na.rm=T)/23

cob_1_80 <- ifel(cob_1 >= .8,1,NA)
cob_2_80 <- ifel(cob_2 >= .8,2,NA)

cob <- app(c(cob_1_80,cob_2_80),sum,na.rm=T)
names(cob) <- 'cobertura'

writeRaster(cob,'data/processed/raster/cobertura/tipo_vegetacional.tif',
            overwrite=T)

# poligonos de thiessen

shac <- vect('data/processed/vectorial/shac.shp') |> 
  select(shac = OBJECTID)
pozos <- vect('data/processed/vectorial/pozo_filtrado.shp')

shac_id <- shac$shac[!(shac$shac %in% c(222,224))]

vor_list <- lapply(shac_id, \(x) {
  
  pol <- filter(shac,shac == x)
  pts <- filter(pozos,shac == x)
  
  vor <- voronoi(pts, bnd = shac)
  
  vor_crop <- crop(vor, pol)
  
})

vor_shac <- do.call(rbind, vor_list)

plot(vor_shac)

vor_intersect <- \(v1,v2,distance,output = NULL) {
  
  lista <- lapply(1:nrow(v1), \(x) {
    
    pt <- v1[x]
    buffer <- buffer(pt, distance)
    
    crop(v2,buffer) |> 
      filter(codigo == pt$codigo)
    
  })
  
  vector <- do.call(rbind, lista)
  
  if (!is.null(output)) writeVector(vector,output) else vector
  
}

# colores <- sample(rainbow(nrow(pozos)))
# plot(vor_intersect(pozos,vor_shac,8000),
#      border = adjustcolor("black", alpha.f = 0), 
#      col = colores, alpha = .3)
# plot(vor_intersect(pozos,vor_shac,5000),
#      border = adjustcolor("black", alpha.f = 0), 
#      col = colores, alpha = .3,
#      add = T)
# plot(vor_intersect(pozos,vor_shac,3000),
#      border = adjustcolor("black", alpha.f = 0), 
#      col = colores, alpha = .3,
#      add = T)
# plot(pozos,cex = .8, alpha=.7, add=T)
# plot(shac,add=T)

vor_intersect(pozos,vor_shac,8000,
             'data/processed/vectorial/unidades_8000.shp')
vor_intersect(pozos,vor_shac,5000,
             'data/processed/vectorial/unidades_5000.shp')
vor_intersect(pozos,vor_shac,3000,
             'data/processed/vectorial/unidades_3000.shp')

# cobertura

mayor_70 <- function(x) {
  l = length(x)
  t = table(x)
  n <- max(t)
  prop <- n/l
  if (prop >= .7) as.numeric(names(t)[which.max(t)]) else NA
}

cob <- list.files('data/processed/raster/cobertura/biomas/',full.names=T) |> 
  rast() |> 
  classify(matrix(c(21,3,12,66,1,2,2,2),nrow=4,ncol=2), others = NA)

r_base <- grep('SETI',list.files('data/processed/raster/indices',full.names=T),value=T)[1] |> 
  rast() |> 
  subset(1) |> 
  setValues(NA)
names(r_base) <- 'cobertura'
varnames(r_base) <- 'cobertura'

r_base_disagg <- r_base |> 
  disagg(100)

cob_reproj <- cob |> 
  project(r_base_disagg)

cob_modal <- cob_reproj |> 
  aggregate(100, fun = mayor_70,cores = ncores-1)

writeRaster(cob,'data/processed/raster/cobertura/cobertura.tif',overwrite=T)
writeRaster(cob_modal,'data/processed/raster/cobertura/cobertura_70.tif',overwrite=T)
