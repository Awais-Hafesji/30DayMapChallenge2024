# Load the libraries
library(arrow)
library(tidyverse)
library(sf)
library(mapgl)
library(classInt)

# Get the buildings data
buildings <- open_dataset('s3://overturemaps-us-west-2/release/2024-10-23.0/theme=buildings?region=us-west-2')

# Create a custom bounding box for London
londonbb <- c(-0.201603,51.479074,0.019586,51.532966) %>%
  as.vector()

# Filter the buildings data to the London bbox
sf_buildings <- buildings %>%
  filter(bbox$xmin > londonbb[1],
         bbox$ymin > londonbb[2],
         bbox$xmax < londonbb[3],
         bbox$ymax < londonbb[4]) %>%
  select(id, geometry, height) %>%
  collect() %>%
  st_as_sf(crs = 4326) %>% 
  mutate(height = ifelse(is.na(height), 2, height))

# Identify the kmeans intervals
classIntervals(sf_buildings$height, style = "kmeans", n = 5)

# Plot the map!
sf_map <- mapboxgl(
  style = mapbox_style("light")) %>%
  fly_to(center = c(-0.09306, 51.51556),
         zoom = 14,
         bearing = 50,
         pitch = 76) %>%
  add_fill_extrusion_layer(id = "buildings",
                           source = sf_buildings,
                           fill_extrusion_height = get_column("height"),
                           fill_extrusion_color = interpolate(
                             column = "height",
                             values = c(0.05,
                                        6.142573,
                                        17.79811,
                                        47.5,
                                        112.55,
                                        310),
                             stops = viridisLite::mako(6, direction = -1)),
                           fill_extrusion_opacity = 1,
                           tooltip = "height",
                           hover_options = list(
                           fill_extrusion_color = "red"))%>%
  add_legend("Building Heights in London",
             values = c("0.05m", "310m"),
             colors = viridisLite::mako(6, direction = -1))

sf_map

# Save the map output
htmlwidgets::saveWidget(sf_map, "Outputs/Day30_The_final_map.html", selfcontained = TRUE)