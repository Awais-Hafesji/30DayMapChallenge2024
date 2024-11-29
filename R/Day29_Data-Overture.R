# Load the libraries
library(tidyverse)
library(sf)
library(overturemapsr)
library(mapgl)
library(arrow)
library(osmdata)
library(viridisLite)
library(classInt)
library(rdeck)

# Get the data - CDRC Overture derived POI dataset and ONS boundaries
poi_uk <- st_read("Data/poi_uk.gpkg")
boundaries <- st_read("Data/Local_Authority_Districts_May_2024_Boundaries_UK_BUC_-3799209068982948111.gpkg")

# Find the libraries
lib <- poi_uk %>%
  filter(main_category == c("library"))

# Find the Cafes
cafe <- poi_uk %>%
  filter(main_category == c("cafe")) %>%
  filter(grepl("coffee_shop", alternate_category, ignore.case = TRUE))

# Join them up
lib_cafe <- rbind(lib, cafe)

# Filter to Bradford
bfd_boundary <- filter(boundaries, LAD24NM %in% c("Bradford"))

bfdlib_cafe <- st_intersection(lib_cafe, bfd_boundary)

# Set to WGS84 for Mapbox compatibility
bfdlib_cafe <- st_transform(bfdlib_cafe, crs = 4326)

# Plot the map!
map <- mapboxgl(style = mapbox_style("dark"),
                bounds = bfd_boundary) %>%
  add_circle_layer(id = "poi-layer",
                   source = bfdlib_cafe,
                   circle_color = match_expr("main_category",
                                             values = c("cafe",
                                                        "library"),
                                             stops = c("#1f78b4",
                                                       "#33a02c")),
    circle_radius = 7,
    circle_stroke_color = "#ffffff",
    circle_stroke_width = 2,
    circle_opacity = 0.8,
    tooltip = "primary_name",
    hover_options = list(circle_radius = 12,
                         circle_color = "#ffff99")) %>%
  add_categorical_legend(legend_title = "Potential Study Spots",
                         values = c("Cafe",
                                    "Library"),
                         colors = c("#1f78b4",
                                    "#33a02c"),
                         circular_patches = TRUE)

# Save the output
htmlwidgets::saveWidget(map, "Outputs/Day29_Data-Overture.html", selfcontained = TRUE)