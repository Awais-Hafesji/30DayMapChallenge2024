# Load libraries
library(tidyverse)
library(sf)
library(ggspatial)
library(gganimate)
library(scales)
library(ggthemes)
library(gifski)

# Load in dataset
data <- read.csv("Data/Average-prices-2024-08.csv")
boundaries <- st_read("Data/Local_Authority_Districts_May_2024_Boundaries_UK_BUC_-3799209068982948111.gpkg")

# Tidy the data
data <- tibble(data)
data$Date <- as.Date(data$Date)

LAD_data <- filter(data, grepl("^E0", Area_Code))

LAD_data <- LAD_data %>%
  group_by(Year = year(Date), `Region Name` = Region_Name, `Area Code` = Area_Code) %>%
  summarize(`Average Price` = mean(Average_Price, na.rm = TRUE))

LAD_joined <- left_join(LAD_data, boundaries, join_by(`Area Code` == `LAD24CD`)) %>%
  select(.,-c(LAD24NMW)) %>%
  filter(., Year >= 2014) %>%
  filter(., `Region Name` %in% c("Bradford",
                                 "Leeds",
                                 "Kirklees",
                                 "Wakefield",
                                 "Calderdale",
                                 "York",
                                 "City of Kingston upon Hull",
                                 "East Riding of Yorkshire",
                                 "Doncaster",
                                 "Barnsley",
                                 "North East Lincolnshire",
                                 "Rotherham",
                                 "Sheffield",
                                 "North Lincolnshire",
                                 "North Yorkshire"))

LAD_sf <- st_as_sf(LAD_joined, crs = 27700)

# Plot the data
plot <- ggplot(LAD_sf) +
  geom_sf(aes(fill = `Average Price`)) +
  scale_fill_viridis_c(labels = comma,
                       name = "Average Price (£)",
                        guide = guide_colorbar(
                        direction = "vertical",
                        barheight = unit(100, units = "mm"),
                        barwidth = unit(2, units = "mm"),
                        draw.ulim = FALSE,
                        title.position = 'top',
                        title.hjust = 0.5,
                        title.vjust = 0.5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(colour = "#231f20",
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
    text = element_text(family = "sans",
                        face = "bold"),
    legend.position = "right"
  ) +
  annotation_north_arrow(location = "tr",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br") +
  labs(title = "Average House Prices per Local Authority in {frame_time}",
       caption = "Local Authority Boundaries: ONS Open Geography Portal, 2024; Data: HM Land Registry 2024") +
  transition_time(as.integer(Year))

# gganimate
map_animated <- animate(plot,
                        height = 776, width = 976)

map_animated

#Save the output
anim_save("Outputs/Day12_Avg_House_Prices_England.gif", map_animated)