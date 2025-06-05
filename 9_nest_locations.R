# Run 1_read_data.R before this script.

# Overlay nest locations on map of Miller Reserve.

library(raster)
library(tidyverse)
library(ggmap)
library(basemaps)
library(sf)
library(terra)
library(tidyterra)

min_lat <- 37.130991
min_long <- -89.463757
max_lat <- 37.133492
max_long <- -89.458964


#basemap token: pk.eyJ1IjoibXRheWwyMiIsImEiOiJjbTc0ejQwaW0wZWtvMmxvaTIyNGNudWc5In0.HEHFvCEs92NroWNvnte8iA

# Zoom in and save the rectangle with Miller Reserve.
# ext1 <- draw_ext()

# Esri.WorldImagery

#save(ext, file = "miller_reserve_mapbox_extent.RData")
load(file = "miller_reserve_mapbox_extent.RData")

set_defaults(map_service = "maptiler", map_type = "satellite", map_token = "SlNC0NZ3k7kR4MIhgPwi")


#the_map <- basemap_raster(ext)
the_map <- basemap_terra(ext)
#rast_map <- rast(the_map)
# save(the_map, file = "miller_reserve_base_map.RData")
# load(file = "miller_reserve_base_map.RData")

# Works!
base_plot <- ggplot() +
  geom_spatraster_rgb(data = the_map) +
  coord_sf(
    xlim = c(max_long, min_long),
    ylim = c(min_lat, max_lat),
    crs = 4326) +
  scale_fill_identity() +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme(
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank())


base_plot +
  geom_point(
    data = nest_raw,
    aes(x = longitude,
        y = latitude,
        color = common_bird,
        fill = common_bird,
        shape = common_bird),
    size = 1) +
  scale_color_brewer(palette = "Set2", name = NULL) +
  scale_fill_brewer(palette = "Set2", name = NULL) +
  scale_shape_manual(values = c(21:25), name = NULL) +
  theme(text = element_text(family = "Linux Libertine O")) +
  guides(
    colour = guide_legend(position = "bottom", nrow = 2),
    fill   = guide_legend(position = "bottom", nrow = 2),
    shape  = guide_legend(position = "bottom", nrow = 2)
  ) +
  theme(
    legend.box.margin = unit(-0.5, "cm"),
    legend.key.spacing.x = unit(0.25, "cm"),
    legend.key.spacing.y = unit(2, "pt"),
    legend.text = element_text(margin = margin(l = unit(1, "pt"))),
    plot.margin = margin(-1,0,0,0, "cm")
  )


ggsave(
  filename = "map_of_nests.png",
  dpi = 288,
  units = "px",
  width = 1050,
  height = 900
)




# 
# get_maptypes()
# 
# 
# 
# the_map <- basemap_gglayer(
#   ext, 
#   map_service = "mapbox", 
#   map_type = "satellite",
#   map_token = "pk.eyJ1IjoibXRheWwyMiIsImEiOiJjbTc0ejQwaW0wZWtvMmxvaTIyNGNudWc5In0.HEHFvCEs92NroWNvnte8iA",
#   map_dir = "/Users/mtaylor/Documents/r/paige_temp")



