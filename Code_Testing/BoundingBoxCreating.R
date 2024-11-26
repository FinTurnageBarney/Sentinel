# Code to create a bounding box around study areas to give to seninel data pulling script.

library(sf)

sites = utils::read.csv("Data/Site_lat_long.CSV")
sites

sites = sf::st_as_sf(sites,coords = c("Long","Lat"),crs=4326)
sites

sites |>
  sf::st_transform(5070) |>
  sf::st_buffer(dist=10000) |>
  sf::st_transform(4326) |>
  sf::st_bbox() ->
  box

library(tmap)
tm_shape(sf::st_as_sfc(box))+
  tm_borders()+
  tm_shape(sites)+
  tm_dots(size=1)+
  tm_basemap("OpenStreetMap")

box
