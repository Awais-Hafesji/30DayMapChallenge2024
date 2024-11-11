# libraries
library(tidyverse)
library(sf)
library(rnaturalearth)

# Populated places dataset
populated_places <- ne_download(scale = 10,
  type = "populated_places_simple"
)

# Places in the Arctic Circle
arctic_circle <- filter(populated_places, latitude > 66.5)

world_coordinates <- map_data("world")

arctic_circle <- st_as_sf(arctic_circle,
                          coords = c("longitude",
                                     "latitude",
                                     crs = 4326))

# Plot the map
ggplot() +
  geom_polygon(data = world_coordinates, aes(x=long, y = lat, group = group)) +
  geom_sf(fill = 'lightgrey',
          alpha = 1,
          colour = "black") +
  geom_point(data = arctic_circle,
             aes(x = longitude,
                 y = latitude,
                 colour = 'red'),
             size = 3,
             alpha = 1) +
  theme_bw() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    x = "", 
    y = "", 
    title = "Populated Locations in the Arctic", 
    caption = "Data: RNaturalEarth",
    colour = "Locations") +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  coord_sf(xlim = c(-170, 190), ylim = c(30, 100), expand = FALSE) +
  annotation_north_arrow(location = 'tr', style =  north_arrow_fancy_orienteering)
  
ggsave("Outputs/Day11_Arctic.png", dpi = 320, height = 7, width = 14, units = "in")