# Load libraries
library(tidyverse)
library(sf)
library(ggspatial)
library(scales)
library(rnaturalearth)

# Load data
Data <- read.csv("Data/CME.csv")
world <- map_data("world")
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter dataset
plot_data <- Data %>%
  select(.,
         Code = REF_AREA,
         region = Geographic.area,
         Year = TIME_PERIOD,
         `Neonatal Mortality Rate` = OBS_VALUE) %>%
  filter(Year == 2022)

# Join the data
Plot_joined <- inner_join(world, plot_data, by = c("iso_a3_eh" = "Code"))

# Plot the map
ggplot(data = Plot_joined) +
  geom_sf(aes(fill = `Neonatal Mortality Rate`)) +
  scale_fill_viridis_c(option = "plasma") +
  theme_dark() +
  theme(
    legend.position = "right",
    legend.background = element_rect(fill = "#424242"),
    legend.text = element_text(colour = "white",
                               family = "Arial"),
    legend.title = element_text(colour = "white",
                                family = "Arial"),
    plot.title = element_text(hjust = 0.5,
                              vjust = 6,
                              size = 20,
                              colour = "white",
                              family = "Arial",
                              face = "bold"),
    plot.caption = element_text(colour = "white",
                                family = "Arial",
                                hjust = 0.5,
                                vjust = (-10)),
    plot.background = element_rect(fill = "#424242",
                                   colour = "#424242"),
    panel.background = element_rect(fill = "#424242",
                                    colour = "#424242"),
    axis.text.x = element_text(colour = "white"),
    axis.text.y = element_text(colour = "white"),
    plot.margin = unit(c(1, 2, 1, 1), "cm")
  ) +
  labs(
    title = "Neonatal Mortality Rates per Country, 2022", 
    caption = "Data: UNICEF, 2024 (Obtained from HDX)")

# Save the output
ggsave("Outputs/Day14_A_World_Map.png", dpi = 320, height = 5, width = 10, units = "in")