# Load the libraries
library(tidyverse)
library(sf)
library(ggspatial)

# Read in the data
#https://www.data.gov.uk/dataset/650db146-1e18-41fa-912a-33edacd8ac85/street-lighting
street_lamps <- read.csv("Data/Street_Lighting.csv")
#https://geoportal.statistics.gov.uk/datasets/62eb9df29a2f4521b5076a419ff9a47e_0/explore
ward_to_city_lkp <- read.csv("Data/Ward_to_Westminster_Parliamentary_Constituency_to_LAD_to_UTLA_(July_2024)_Lookup_in_UK.csv")
ward_boundaries <- read_sf("Data/Wards_May_2024_Boundaries_UK_BFE__4726407479218445330.gpkg")
#https://geoportal.statistics.gov.uk/datasets/3f28a3b7919b4a579b34c0823b386a51_0/explore?location=53.774785%2C-1.711037%2C11.29

# Pre-process the data
bfd_wards <- right_join(select(ward_to_city_lkp,
                               'LAD24CD',
                               'LAD24NM',
                               'WD24CD'),
                        ward_boundaries,
                        by = 'WD24CD') %>%
  filter(LAD24NM == "Bradford") %>%
  distinct() %>%
  st_as_sf()

broken_lamps <- street_lamps %>%
  filter(., X != 0) %>%
  filter(., DEFECT == "OpenDefect")

# Plot the street lights across Bradford
ggplot(data = bfd_wards) +
  geom_sf(fill = 'darkblue', alpha = 1, colour = "lightgrey") +
  geom_point(data = broken_lamps, aes(x = X, y = Y, colour = 'red'), size = 1, alpha = 0.5) +
  theme_bw() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    x = "", 
    y = "", 
    title = "Open Defective Street Lights in Bradford", 
    caption = "Ward Boundaries: ONS, 2024; Street Light Data: CBMDC, 2024",
    colour = "Street Lamps") +
  guides(colour = guide_legend(override.aes = list(size = 5))) +
  annotation_north_arrow(location = 'tr', style =  north_arrow_fancy_orienteering) +
  annotation_scale(location = 'bl')

# Save the output
ggsave("Outputs/Map1.png", dpi = 320)