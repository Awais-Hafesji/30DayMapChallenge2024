# Load libraries
library(tidyverse)
library(ggspatial)
library(sf)
library(readxl)
library(janitor)

# Load datasets
data <- read_excel("Data/nomis_2024_11_16_214837.xlsx")
boundaries <- st_read("Data/Local_Authority_Districts_May_2023_UK_BUC_V2_8757178717458576320.gpkg")

# Tidy and join
Level4_LAD <- data %>%
  row_to_names(row_number = 7,
               remove_row = TRUE,
               remove_rows_above = TRUE) %>%
  clean_names() %>%
  rename(.,
         LAD23NM = "local_authority_district_unitary_as_of_april_2023",
         LAD23CD = "na",
         `Percentage with Level 4 or above` = "na_3") %>%
  left_join(.,
            boundaries,
            by = 'LAD23CD') %>%
  select(-c(3:5, 7:8)) %>%
  na.omit()

# Set as numeric
Level4_LAD$`Percentage with Level 4 or above` <- as.numeric(Level4_LAD$`Percentage with Level 4 or above`)

# Change to sf and set crs to 27700 (OSGB)
Level4_sf <- st_as_sf(Level4_LAD,
                      crs = 27700)
# Map the results
plot <- ggplot(Level4_sf) +
  geom_sf(aes(fill = `Percentage with Level 4 or above`),
          col = "#EDE7D7",
          lwd = 0.01) +
  scale_fill_viridis_c(guide = guide_legend()) +
  theme_void() +
  theme(plot.title = element_text(colour = "black",
                                  hjust = 0,
                                  vjust = 0,
                                  size = 15.5),
        plot.caption = element_text(colour = "black",
                                    hjust = 1,
                                    face = "plain",
                                    size = 12),
        plot.subtitle = element_text(colour = "black",
                                     hjust = 0,
                                     vjust = 0,
                                     size = 10),
        plot.background = element_rect(fill = "#EDE7D7",
                                       colour = "#EDE7D7"),
        panel.background = element_rect(fill = "#EDE7D7",
                                        colour = "#EDE7D7"),
        text = element_text(family = "Redaction"),
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.title.position = "top",
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  guides(fill=guide_legend(nrow=1)) +
  labs(title = "Percentage of population with Level 4 or above qualifications\nper local authority in England",
       subtitle = "Based on Census 2021 survey data",
       caption = "\nData: Nomis Census 2021\nBoundaries: ONS Open Geography Portal, 2023") +
  annotation_north_arrow(location = "tr",
                         style = north_arrow_nautical(fill = c("black", "#EDE7D7")))+
  annotation_scale(location = "br",
                   bar_cols = c("black"),
                   style = "ticks",
                   pad_y = unit(0.45, "cm"))

plot

# Save the output
ggsave("Outputs/Day16_Choropleth.png", dpi = 320, width = 6.75, height = 9.5, units = "in")