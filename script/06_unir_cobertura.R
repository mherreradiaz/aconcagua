source('script/00_setup.R')
library(tidyterra)

cob_ag <- rast('data/processed/raster/cobertura/SHAC_AG.tif')
cob_nv <- rast('data/processed/raster/cobertura/SHAC_NV.tif')

values(cob_ag) <- values(cob_ag)*0+1000
values(cob_nv) <- values(cob_nv)*0+2000

cob_merge <- merge(cob_ag,cob_nv,first = T,na.rm=T)

shac <- vect('data/raw/vectorial/SHAC_Aconcagua.shp') |> 
  select(ID_IDE) |> 
  rename(shac = ID_IDE) |> 
  mutate(shac = as.numeric(substr(shac,6,8))) |> 
  rasterize(cob_merge,field='shac')

cob <- shac+cob_merge

writeRaster(project(cob, "EPSG:4326",method='near'),'data/processed/raster/cobertura/COB.tif',overwrite=T)