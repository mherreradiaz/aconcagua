library(tidyverse)
library(classInt)
library(tidyverse)
library(sf)

data <- readxl::read_xlsx('data/raw/tabulada/gw_chile.xlsx')

shac <- read_sf('data/raw/vectorial/SHAC_Aconcagua') |> 
  select(ID_IDE)

data_estas_acon <- data |> 
  filter(str_detect(Sub_Basin,'Aconcagua')) |> 
  select(Code,Date_String,Elevation,`Depth to water (m)`,Longitude_GCS_WGS_1984,Latitude_GCS_WGS_1984) |> 
  set_names(c('code','date','elevation','dtw','lon','lat')) |> 
  mutate(date = as_date(date))

estas_sf <- data_estas_acon |> 
  distinct(code,lat,lon) |> 
  st_as_sf(coords = c('lon','lat'),crs=4326) |> 
  st_transform(32719)

estas_sf <- estas_sf |> st_intersection(shac)

data_estas_acon <- data_estas_acon |> left_join(estas_sf |> st_drop_geometry())

####
data_estas_acon |> 
  filter(ID_IDE == 'SHAC_217') |> 
  ggplot(aes(date,dtw,color=factor(code))) +
  geom_point() +
  geom_line()

data_estas_acon |> 
  group_by(ID_IDE,mes =floor_date(date,'1 month')) |>
  summarize(dtw = mean(dtw,na.rm = TRUE)) |> 
  ggplot(aes(mes,-dtw,color = ID_IDE)) + 
  geom_point() + 
  geom_line() +
  facet_grid(ID_IDE~.,scales = 'free')

data_estas_acon |> 
  filter(code == 5428010) |> arrange(date) |> 
ggplot(aes(date,dtw)) + 
  geom_point() + 
  geom_line()

library(sf)
library(tmap)



tmap_mode('view')
tm_shape(estas_sf) + 
  tm_markers(clustering = FALSE)
