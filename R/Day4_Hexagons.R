# Load the libraries
library(tidyverse)
library(sf)
library(ggspatial)
library(readxl)
library(janitor)
library(scales)
library(cowplot)
library(cartogram)
library(geogrid)

# Read in the datasets
cars <- read_xlsx("Data/census_2021_cars_msoa.xlsx")
msoa_boundaries <- read_sf("Data/Middle_layer_Super_Output_Areas_December_2021_Boundaries_EW_BFE_V8_-1492394885063627823.gpkg")

# Create a filter for the next code chunk
my_patterns <- c("Leeds")

# Pre-process data
cars_msoa <- cars %>%
  row_to_names(row_number = 7,
               remove_row = TRUE,
               remove_rows_above = TRUE) %>%
  clean_names() %>%
  rename(.,
         MSOA21NM = "x2021_super_output_area_middle_layer",
         MSOA21CD = "na",
         `Total Cars or Vans` = "x2021") %>%
  left_join(.,
            msoa_boundaries,
            by = 'MSOA21CD') %>%
  select(-c('MSOA21NM.y',
            'MSOA21NMW')) %>%
  na.omit() %>%
  filter(., grepl(paste(my_patterns, collapse='|'), MSOA21NM.x))

# Convert to sf and set CRS
cars_msoa<- st_as_sf(cars_msoa,
                    crs = 27700)

# Create hex grid
hg = calculate_grid(cars_msoa, learning_rate = 0.05, 
                    grid_type = "hexagonal", verbose = F)
# Extract the spatial objects
hg_points = st_as_sf(hg[[1]])
hg_polys = st_as_sf(hg[[2]])

# Assign data to hex
hg = assign_polygons(cars_msoa, hg)

# Map the results
plot <- ggplot(hg) +
  geom_sf(aes(fill = Total.Cars.or.Vans)) +
  scale_fill_continuous(labels = comma,
                        high = "#132B43",
                        low = "#56B1F7") +
  theme_void() +
  theme(
    plot.title = element_text(colour = "#231f20",
                              hjust = 0.05,
                              vjust = 0.2,
                              size = 20),
    plot.subtitle = element_text(colour = "#231f20",
                                 hjust = 0.2,
                                 vjust = 0.2,
                                 size = 16),
    plot.caption = element_text(colour = "#231f20",
                                face = "plain",
                                hjust = 0),
    text = element_text(family = "sans",
                        face = "bold"),
    plot.background = element_rect(fill = "#EDE7D7",
                                   colour = "#EDE7D7"),
    panel.background = element_rect(fill = "#EDE7D7",
                                    colour = "#EDE7D7"),
    legend.position = "right",
    plot.margin = unit(c(1, 4, 1, 1), "cm")
  ) +
  labs(fill = "Total Cars or Vans") +
  annotation_north_arrow(location = "tr",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br") +
  labs(title = "Total Number of Cars or Vans per MSOA in Leeds",
       caption = "Local Authority Boundaries: ONS Open Geography Portal, 2021; Data: Nomis Census 2021")

plot1 <- ggdraw(plot) + 
  draw_label(x=0.98,
             y=0.75,
             hjust=1, 
             "Census 2021 statistics includes data on car \nor van availability for household members. \n \nThe Local Authority in Yorkshire with the highest \ncar or van count is Leeds at 373,013. The \nbreakdown of which is shown here.", 
             color = "#231f20",
             size = 10,
             fontfamily="sans") +
  theme(panel.background = element_rect(fill = "grey90",
                                        colour = "grey90"))
plot1

# Save the output
ggsave("Outputs/Day4_Hexagons.png", dpi = 320, width = 11.75, height = 8.75, units = "in")