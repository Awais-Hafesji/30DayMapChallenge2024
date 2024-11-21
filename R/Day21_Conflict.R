# Load the libraries
library(tidyverse)
library(sf)
library(readxl)
library(scales)

# Load the data
data <- read_xlsx("Data/palestine_hrp_civilian_targeting_events_and_fatalities_by_month-year_as-of-21nov2024.xlsx",
                  sheet = 2)
boundaries <- st_read("Data/pse_adm_pamop_20231019_shp/pse_admbnda_adm2_pamop_20231019.shp")

# Filter dataset
plot_data <- data %>%
  filter(Year == 2024) %>%
  filter(Admin1 == "Gaza Strip")

# Calculate total fatalities between Jan and Nov 2024
plot_data_total <- plot_data %>%
  group_by(`Admin2 Pcode`) %>% 
  summarise(`Total Fatalities` = sum(Fatalities)) %>%
  left_join(., boundaries, by = c(`Admin2 Pcode` = "ADM2_PCODE"))

plot_data_total <- st_as_sf(plot_data_total)

# Plot the map!
ggplot(data = plot_data_total) +
  geom_sf(aes(fill = `Total Fatalities`)) +
  scale_fill_viridis_c(option = "plasma") +
  theme_dark() +
  theme(
    legend.position = "right",
    legend.background = element_rect(fill = "#424242"),
    legend.text = element_text(colour = "white",
                               family = "Giphurs"),
    legend.title = element_text(colour = "white",
                                family = "Giphurs"),
    plot.title = element_text(hjust = 0.5,
                              vjust = 3,
                              size = 20,
                              colour = "white",
                              family = "Giphurs",
                              face = "bold"),
    plot.caption = element_text(colour = "white",
                                family = "Giphurs",
                                hjust = 0.5,
                                vjust = (-10)),
    plot.background = element_rect(fill = "#424242",
                                   colour = "#424242"),
    panel.background = element_rect(fill = "#424242",
                                    colour = "#424242"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.margin = unit(c(1, 2, 1, 1), "cm")
  ) +
  labs(
    title = "Total Civilian Fatalities in Gaza, between January and November 2024", 
    caption = "Data: ACLED, 2024\nBoundaries: UN OCHA, 2023")

# Save the output
ggsave("Outputs/Day21_Conflict.png", dpi = 320, height = 9, width = 12, units = "in")