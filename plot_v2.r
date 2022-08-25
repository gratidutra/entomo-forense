library(spData)
library(spDataLarge)
library(raster)
library(tidyverse)
library(sf)

sf_with_cases

mapa_casos <- tm_shape(sf_with_cases) + 
  tm_fill(col ='n', title= expression("Número de cadáveres"), palette = 'BuGn') +
  tm_borders()+
  tm_compass(type = '8star', position = c("right", "top"), size = 2.5) 
  #tm_scale_bar(breaks = c(0,50,100), text.size = 0.5, position = c("right", "bottom"))

tmap_mode("view")
mapa_casos
