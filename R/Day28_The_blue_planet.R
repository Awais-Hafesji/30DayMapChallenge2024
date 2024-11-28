# Load the libraries
library(tidyverse)
library(sf)
library(cowplot)
library(ggspatial)
library(osmdata)
library(ggmap)
library(basemaps)

# assign the bounding box
yorks_bbox <- getbb(place_name = "Yorkshire")

bounds <- opq(bbox = yorks_bbox)

# Get features of the map
rivers <- bounds %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

# Plot the map!
plot <- ggplot() +
  annotation_map_tile("osm", zoom = 8) +
  geom_sf(data = rivers$osm_lines,
          colour = "royalblue",
          lwd = 0.7) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white",
                                       colour = NA),
        plot.margin = unit(c(2, 1, 1, 1), "cm"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  annotation_north_arrow(location = "bl",
                         pad_y = unit(0.6, "cm"),
                         style = north_arrow_nautical) +
  annotation_scale(location = "bl")

plot2 <- ggdraw(plot) + 
  draw_label(x=0.5,
             y=0.94,
             hjust=0.5, 
             "Rivers Running Through The North",
             colour = "black",
             size = 20,
             fontfamily="Giphurs ExtraBlack") +
  draw_label(x=0.5,
             y=0.03,
             hjust=0.5, 
             "Data: OpenStreetMap",
             color = "black",
             size = 12,
             fontfamily="Giphurs ExtraBlack") +
  theme(panel.background = element_rect(fill = "white",
                                        colour = "white"))
plot2

# Save the output
ggsave("Outputs/Day28_The_blue_planet.png", plot = plot2, dpi = 320, height = 6.5, width = 5.5, units = "in")