# Load the libraries
library(tidyverse)
library(sf)
library(ggspatial)
library(readxl)
library(janitor)
library(cartogram)
library(scales)

# Load the datasets
data <- read_xlsx("Data/nomis_2024_11_24_235141.xlsx")
boundaries <- st_read("Data/Regions_December_2023_Boundaries_EN_BUC_4553132085147889919.gpkg")

# Tidy the table
population <- data %>%
  row_to_names(.,
               row_number = 6,
               remove_row = TRUE,
               remove_rows_above = TRUE) %>%
  clean_names() %>%
  rename(., region_code = na) %>%
  rename(., Population = x2023) %>%
  filter(., Population != "-")

# Join to boundaries
population_joined <- left_join(population,
                               boundaries,
                               by = c("region_code" = "RGN23CD")) %>%
  select(., -c(4)) %>%
  na.omit()

# Convert to numeric
population_joined$Population <- as.numeric(population_joined$Population)

# Set as sf
population_sf <- st_as_sf(population_joined)

# Set up dorling cartogram
pop_cartogram <- cartogram_dorling(population_sf,
                                   weight = "Population")

pop_cartogram$region <- str_wrap(pop_cartogram$region, width = 5)

# Plot the map!
ggplot(pop_cartogram) +
  geom_sf(aes(fill = Population), colour = "grey50") +
  geom_sf_text(aes(label = region),
               size = 3,
               colour = "white",
               family = "Giphurs") +
  scale_fill_continuous(labels = comma) +
  theme_classic() +
  theme(plot.title = element_text(colour = "#231f20",
                              hjust = 0.05,
                              vjust = 0.4,
                              size = 20),
    plot.subtitle = element_text(colour = "#231f20",
                                 hjust = 0.2,
                                 vjust = 0.2,
                                 size = 16),
    plot.caption = element_text(colour = "#231f20",
                                face = "plain",
                                hjust = 0),
    text = element_text(family = "Giphurs",
                        face = "bold"),
    plot.background = element_rect(fill = "#EDE7D7",
                                   colour = "#EDE7D7"),
    panel.background = element_rect(fill = "#EDE7D7",
                                    colour = "#EDE7D7"),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) +
  labs(title = "Population Estimates per Region in England",
       caption = "Data: Nomis, 2023; Boundaries: ONS Open Geography Portal: 2023")

# Save the output
ggsave("Outputs/Day24_Only_circular_shapes.png", dpi = 320, height = 10, width = 8, units = "in")