library(raster)
library(tidyverse)
library(ggthemes)
library(patchwork)
library(maptools)
library(ggmap)
library(ggthemes)

min_long <- -89.463765
max_long <- -89.458854
min_lat <- 37.131004
max_lat <- 37.133508

base <- get_stadiamap(
  bbox = c(
      min_long,
      min_lat,
      max_long,
      max_lat),
    zoom = 4,
    maptype = "stamen_terrain_background"
  )

