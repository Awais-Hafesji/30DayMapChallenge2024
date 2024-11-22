# Load libraries
library(tidyverse)
library(sf)
library(readxl)
library(ggspatial)

# Load datasets
data <- read_csv("Data/HoC-GE2024-results-by-constituency.csv")
boundaries <- st_read("Data/Westminster_Parliamentary_Constituencies_July_2024_Boundaries_UK_BUC_2763307678252453745.gpkg")

# Join the datasets

data$LabGain <- grepl("Lab gain", data$Result)

data_joined <- left_join(data, boundaries, by = c("ONS ID" = "PCON24CD"))

data_sf <- st_as_sf(data_joined)

# Plot the map!
ggplot(data_sf) +
  geom_sf(aes(fill = LabGain)) +
  scale_fill_manual(values = c("TRUE" = "red",
                               "FALSE" = "grey"),
                    labels = c("TRUE" = "Constituencies gained",
                               "FALSE" = "Constituencies not gained")) +
  theme_bw() +
  theme(plot.title = element_text(colour = "black",
                                  hjust = 0.6,
                                  vjust = 3,
                                  size = 15.5),
        plot.caption = element_text(colour = "black",
                                    hjust = 1,
                                    face = "plain",
                                    size = 12),
        plot.subtitle = element_text(colour = "black",
                                     hjust = 0,
                                     vjust = 0,
                                     size = 10),
        plot.background = element_rect(fill = "white",
                                       colour = "white"),
        panel.background = element_rect(fill = "white",
                                        colour = "white"),
        text = element_text(family = "Redaction"),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.title.position = "top",
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  guides(fill=guide_legend(nrow=1)) +
  labs(title = "Constituencies Labour Gained in the 2024 General Election",
       caption = "\nData: House of Commons Library, 2024\nBoundaries: ONS Open Geography Portal, 2024",
       fill = "Constituency status") +
  annotation_north_arrow(location = "tr",
                         style = north_arrow_nautical(fill = c("black", "white")))+
  annotation_scale(location = "br",
                   bar_cols = c("black"),
                   style = "ticks",
                   pad_y = unit(0.45, "cm"))

# Save the output
ggsave("Outputs/Day22_two_colours.png", dpi = 320, height = 10, width = 7, units = "in")