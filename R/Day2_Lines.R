# Load libraries
library(tidyverse)
library(spdep)
library(ggspatial)

# Read the boundary file
# https://geoportal.statistics.gov.uk/datasets/8e5561d496d74f9796d7fda2f2bc4065_0/explore
icb_boundaries <- read_sf("Data/Integrated_Care_Boards_April_2023_EN_BFE_-267572681801887132.gpkg")

# Apply KNN
icb_boundaries2 <- knearneigh(st_centroid(icb_boundaries),
                              k = 4) %>%
  knn2nb()

# Create a line layer showing Queen's case contiguities
gg.net <- nb2lines(icb_boundaries2,
                   coords=st_geometry(st_centroid(icb_boundaries)),
                   as_sf = F) 

# Plot the contiguity and the ICB layer
plot_nn = 
  ggplot(icb_boundaries) + geom_sf(lwd = 0.5,
                                   fill = "lightgrey",
                                   col = "#231f20") + 
  geom_sf(data = gg.net, col='#005EB8',
          alpha = 1,
          lwd = 0.6) +
  theme_void() + 
  theme(
    plot.title = element_text(colour = "#231f20",
                              hjust = 0.5,
                              vjust = 0.1,
                              size = 20),
    plot.subtitle = element_text(colour = "#231f20",
                                 hjust = 0.5,
                                 vjust = 0.1,
                                 size = 16),
    plot.caption = element_text(colour = "#231f20",
                                hjust = 0.5,
                                vjust = 0.9,
                                size = 12,
                                face = "plain"),
    text = element_text(family = "sans",
                        face = "bold"),
    plot.background = element_rect(fill = "#FFC196",
                                   colour = "#FFC196"),
    panel.background = element_rect(fill = "#FFC196",
                                    colour = "#FFC196")
  ) +
  annotation_north_arrow(location = "tr",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br") +
  labs(title = "Neighbouring Integrated Care Boards in England",
       subtitle = "Using K Nearest Neighbour",
       caption = "ICB Boundaries: ONS Open Geography Portal, 2023")
plot_nn

# Save the output
ggsave("Outputs/Day2Lines.png", dpi = 320, width = 7.3, height = 6.85, units = "in")