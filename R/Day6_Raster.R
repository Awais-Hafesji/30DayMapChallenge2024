# Load libraries
library(sf)
library(tidyverse)
library(ggspatial)
library(MASS)

# Load wifi points
Wifi_points <- read_csv("Data/Wifi Locations July 2023.csv") %>%
  dplyr::filter(., Stage == "Active") %>%
  dplyr::select(., c(1:3))

# set to WGS 84 for Easting Northing conversion
Wifi_points <- st_as_sf(Wifi_points, coords = c("Longitude", "Latitude"), crs =4326)

# set to 27700 to match ward layer
Wifi_points <- st_transform(Wifi_points, crs = 27700)

# Load wards
wards <- st_read("Data/Wards_May_2024_Boundaries_UK_BFE__4726407479218445330.gpkg") 

# Load ward lookup
ward_lkp <- read_csv("Data/Ward_to_Westminster_Parliamentary_Constituency_to_LAD_to_UTLA_(July_2024)_Lookup_in_UK.csv")

# Filter to show Leeds only
wards_filtered <- left_join(wards, ward_lkp, by = "WD24CD") %>%
  dplyr::filter(., LAD24NM == "Leeds") %>%
  dplyr::select(., -c(9:21))

# Ensure wards layer is set to 27700
wards_filtered <- wards_filtered %>% st_transform(crs = 27700)

# Extract the coordinates
coords = data.frame(st_coordinates(Wifi_points))

# KDE function
kde_sf = function (pts, h, n = 200, lims = NULL) 
{
  xy = st_coordinates(pts)
  crs_val = st_crs(pts)
  if (missing(h)) 
    h = c(bandwidth.nrd(xy[, 1]), bandwidth.nrd(xy[, 2]))
  if (is.null(lims)) {
    lims = c(range(xy[, 1]), range(xy[, 2]))
  } else {
    lims = st_bbox(lims)[c(1, 3, 2, 4)]
  }
  kd = kde2d(xy[, 1], xy[, 2], h, n, lims)
  kd_df = data.frame(expand.grid(kd$x, kd$y), kde = array(kd$z, length(kd$z)))
  kd_sf <- st_as_sf(kd_df, coords = c("Var1", "Var2"), crs = crs_val)
  return(kd_sf)
}
# Create the KDE
kde_wifi = kde_sf(Wifi_points, lims = wards_filtered) 

# Plot the KDE
kde_wifi %>% filter(kde > 10^(-9)) %>%
  ggplot(aes(col = kde)) + 
  annotation_map_tile(zoomin=0, type='osm') +
  geom_sf(alpha = 0.5) +
  scale_colour_viridis_c(name = "") +
  theme_bw() +
  theme(legend.position="none",
        plot.title = element_text(size = 26),
        plot.caption = element_text(size = 14)) +
  labs(
    x = "", 
    y = "", 
    title = "KDE of Public WiFi Locations in Leeds", 
    caption = "Data: Leeds City Council, 2023; ONS, 2024") +
  annotation_north_arrow(location = 'tr', style =  north_arrow_fancy_orienteering) +
  annotation_scale(location = 'bl')

# Save the output
ggsave("Outputs/Day6_Raster.png", dpi = 320, height = 13, width = 13, units = "in")