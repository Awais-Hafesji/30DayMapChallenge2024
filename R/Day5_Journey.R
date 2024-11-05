# Load the libraries
library(tidyverse)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(geosphere)

# Set city coordinates
points <- data.frame(
  location = c("Bradford", "Manchester", "Dubai", "Jeddah", "Makkah", "Medina"),
  lon = c(-1.754685, -2.271369, 55.371017, 39.164517, 39.826198, 39.611280),
  lat = c(53.792415, 53.357917, 25.251489, 21.715896, 21.422461, 24.469922)
)

# Assign world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Convert to sf
st_as_sf(world, crs = 4326)

# Create pairs of consecutive points
pairs_df <- data.frame(
  origin_lon = points$lon[-nrow(points)],        
  origin_lat = points$lat[-nrow(points)],        
  dest_lon = points$lon[-1],                     
  dest_lat = points$lat[-1]
)

# Check for invalid longitude and latitude values
summary(points$lon)
summary(points$lat)

# Generate great circle paths for each pair
paths <- do.call(rbind, apply(pairs_df, 1, function(row) {
  gc_points <- gcIntermediate(
    c(as.numeric(row["origin_lon"]), as.numeric(row["origin_lat"])),
    c(as.numeric(row["dest_lon"]), as.numeric(row["dest_lat"])),
    n = 100,  # points along the path for smoothness
    addStartEnd = TRUE,
    breakAtDateLine = TRUE
  )
  path_df <- as.data.frame(gc_points)
  path_df$group <- paste(row["origin_lon"], row["origin_lat"], 
                         row["dest_lon"], row["dest_lat"], sep = "_")
  return(path_df)
}))


# Plot the map
ggplot() +
  geom_sf(data = world, fill = "grey90") +
  geom_line(data = paths, aes(x = lon, y = lat, group = group), color = "red", linewidth = 1) +
  geom_point(data = points, aes(x = lon, y = lat), colour = "darkgreen",size = 2.5) +
  geom_label(hjust = 0, vjust = 0) +
  theme_void() +
  theme(
    plot.title = element_text(colour = "white",
                              hjust = 0.5,
                              vjust = 8,
                              size = 30),
    plot.subtitle = element_text(colour = "white",
                                 hjust = 0.5,
                                 vjust = 13,
                                 size = 16),
    plot.caption = element_text(colour = "white",
                                face = "plain",
                                hjust = 0),
    text = element_text(family = "palatino",
                        face = "bold"),
    plot.background = element_rect(fill = "#1090ab",
                                   colour = "#1090ab"),
    panel.background = element_rect(fill = "#1090ab",
                                    colour = "#1090ab"),
    plot.margin = unit(c(4, 0, 0, 0), "cm"),
  ) +
  labs(title = "My Umrah Journey",
       subtitle = "August 2023",
       x = "Longitude",
       y = "Latitude") +
  coord_sf(xlim = c(-11, 60), ylim = c(10, 65), expand = FALSE) +
  annotation_north_arrow(location = "tr",
                         style = north_arrow_fancy_orienteering)

ggsave("Outputs/Day5_Journey.png", dpi = 320, width = 4.75, height = 7, units = "in")