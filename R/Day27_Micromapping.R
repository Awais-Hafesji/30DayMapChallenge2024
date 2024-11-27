# Load the libraries
library(tidyverse)
library(sf)
library(osmdata)
library(cowplot)
library(ggspatial)

# Set the bounding box
bbox <- c(xmin = -1.535585, ymin = 53.789906,
          xmax = -1.531943, ymax = 53.792952)

# grab the features
bounds <- opq(bbox = bbox)

water <- bounds %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf()

buildings <- bounds %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf()

amenities <- bounds %>%
  add_osm_feature(key = "amenity") %>%
  osmdata_sf()

grass <- bounds %>%
  add_osm_feature(key = "landuse", value = "grass") %>%
  osmdata_sf()

roads <- bounds %>%
  add_osm_feature(key = "highway", value = c("secondary",
                                             "secondary_link",
                                             "tertiary",
                                             "tertiary_link",
                                             "residential",
                                             "living street",
                                             "unclassified",
                                             "service")) %>%
  osmdata_sf()

paths <- bounds %>%
  add_osm_feature(key = "highway", value = c("footway", "path", "track", "steps", "pedestrian")) %>%
  osmdata_sf()

# Plot the map!
plot <- ggplot() +
  geom_sf(data = water$osm_polygons,
          fill = "#7fc0ff",
          colour = "#819CBD") +
  geom_sf(data = grass$osm_polygons,
          fill = "#CCDEB8") +
  geom_sf(data = buildings$osm_polygons,
          fill = "#D4D4D4",
          color = "#A9A9A9") +
  geom_sf(data = roads$osm_lines,
          color = "#424242",
          size = 1.0) +
  geom_sf(data = paths$osm_lines,
          color = "#666666",
          size = 0.3,
          linetype = "dashed") +
  geom_sf(data = amenities$osm_points,
          color = "#000000",
          size = 0.5) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_void() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.caption = element_text(size = 8, colour = "black"),
        panel.background = element_rect(fill = "#F8F0E3"),
        plot.background = element_rect(fill = "#F8F0E3",
                                       colour = NA),
        plot.margin = unit(c(5, 0, 1, 0), "cm"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  annotation_north_arrow(location = "bl",
                         pad_y = unit(0.6, "cm"),
                         style = north_arrow_nautical) +
  annotation_scale(location = "bl")


plot2 <- ggdraw(plot) + 
  draw_label(x=0.5,
             y=0.93,
             hjust=0.5, 
             "Leeds Dock",
             colour = "black",
             size = 28,
             fontfamily="Giphurs ExtraBlack") +
  draw_label(x=0.5,
             y=0.88,
             hjust=0.5, 
             "53.7913° N, 1.5333° W",
             colour = "black",
             size = 20,
             fontfamily="Giphurs ExtraBlack") +
  draw_label(x=0.5,
             y=0.84,
             hjust=0.5, 
             "Data: OpenStreetMap",
             color = "black",
             size = 12,
             fontfamily="Giphurs ExtraBlack") +
  theme(panel.background = element_rect(fill = "#F8F0E3",
                                        colour = "#F8F0E3"))
plot2

# Save the output
ggsave("Outputs/Day27_Micromapping.png", dpi = 320, height = 9, width = 5.5, units = "in")