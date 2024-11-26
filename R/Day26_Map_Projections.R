# Load the libraries
library(tidyverse)
library(sf)
library(rnaturalearth)

# Assign world
world <- ne_countries(scale = "medium", returnclass = "sf")

# Assign rectangular polyconic projection
rpoly_crs <- "+proj=rpoly"

# Apply the above projection to the world
world_rpoly <- st_transform(world, crs = rpoly_crs)

# Plot the map!
ggplot(world_rpoly) +
  geom_sf(fill = "tan", colour = "black") +
  coord_sf(crs = rpoly_crs) +
  theme_dark() +
  theme(plot.title = element_text(hjust = 0.5,
                              vjust = 1.5,
                              size = 25,
                              colour = "black",
                              family = "Redaction 35",
                              face = "bold"),
        plot.caption = element_text(colour = "black",
                                family = "Redaction 35",
                                hjust = 0.5,
                                vjust = (-1.5)),
        plot.background = element_rect(fill = "lightblue",
                                   colour = "lightblue"),
        panel.background = element_rect(fill = "lightblue",
                                    colour = "lightblue"),
    axis.text.x = element_text(colour = "white"),
    axis.text.y = element_text(colour = "white"),
    plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Rectangular Polyconic Projection of the World",
       caption = "Day 26, Map Projections - Awais Hafesji")

# Save the output
ggsave("Outputs/Day26_Map_Projections.png", dpi = 320, height = 6, width = 9, units = "in")