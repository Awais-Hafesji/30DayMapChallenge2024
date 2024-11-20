# Load libraries
library(tidyverse)
library(osmdata)
library(sf)
library(cowplot)

# Select the city, Bradford
Bradfordbb <- getbb("Bradford, UK")

# Grab all of the main roads
roads <- Bradfordbb %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("motorway",
                            "trunk",
                            "primary",
                            "secondary",
                            "tertiary",
                            "motorway_link",
                            "trunk_link",
                            "primary_link",
                            "secondary_link",
                            "tertiary_link")) %>%
  osmdata_sf()

# Grab anything that isn't main roads
streets <- Bradfordbb %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("residential",
                            "living_street",
                            "service",
                            "unclassified",
                            "pedestrian",
                            "footway",
                            "track",
                            "path")) %>%
  osmdata_sf()

# Set the centre point of the map
centre = c(long = -1.7532782820924049,
           lat = 53.792481115275265)

# put the centre in a tibble and set as sf with WGS 84
centre_proj <- tibble(lat = centre["lat"],
                      long = centre["long"]) %>%
  st_as_sf(coords = c("long",
                      "lat"),
           crs = 4326)

# Create circle
circle <- tibble(lat = centre["lat"],
                 long = centre["long"]) %>%
  st_as_sf(coords = c("long",
                      "lat"),
           crs = 4326) %>%
  st_transform(crs = 27700) %>%
  st_buffer(dist = 3000) %>%
  st_transform(crs = 4326)

# Fix the roads and streets to the circle
streets_lines <- st_intersection(circle, streets$osm_lines)
roads_lines <- st_intersection(circle, roads$osm_lines)

# Plot the map!
plot <- ggplot() +
  geom_sf(data = streets_lines,
          col = "#4e463f",
          size = .01) +
  geom_sf(data = roads_lines,
          col = "#4e463f",
          size = 2) +
  geom_sf(data = circle, 
          color = "#4e463f", 
          fill = NA, 
          linewidth = 2) +
  theme(plot.background = element_rect(fill = "#F8F0E3",
                                       color = NA),
        panel.background = element_rect(fill = "#F8F0E3",
                                        color = NA),
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(-0.1, 0.5, 5, 0.5), "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot

plot2 <- ggdraw(plot) + 
  draw_label(x=0.5,
             y=0.2,
             hjust=0.5, 
             "City of Bradford",
             colour = "#4e463f",
             size = 28,
             fontface="bold",
             fontfamily="Redaction") +
  draw_label(x=0.5,
             y=0.15,
             hjust=0.5, 
             "53.792° N, 1.754° W",
             colour = "#4e463f",
             size = 20,
             fontfamily="Redaction") +
  draw_label(x=0.5,
             y=0.05,
             hjust=0.5, 
             "Data: OpenStreetMap",
             color = "#4e463f",
             size = 12,
             fontfamily="Redaction") +
  theme(panel.background = element_rect(fill = "#F8F0E3",
                                        colour = "#F8F0E3"))
plot2

# Save the output
ggsave("Outputs/Day20_OSM.png",
       dpi = 320,
       height = 10,
       width = 7,
       units = "in")