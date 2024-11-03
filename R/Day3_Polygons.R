# Load the libraries
library(tidyverse)
library(sf)
library(ggspatial)
library(readxl)
library(janitor)
library(scales)
library(cowplot)

# Read in the datasets
cars <- read_xlsx("Data/census_2021_cars.xlsx")
LAD_boundaries <- read_sf("Data/Local_Authority_Districts_December_2022_UK_BFE_V2_3938606180292329390.gpkg")

# Pre-process data
cars_LAD <- cars %>%
  row_to_names(row_number = 7,
               remove_row = TRUE,
               remove_rows_above = TRUE) %>%
  clean_names() %>%
  rename(.,
         LAD22NM = "x2022_local_authorities_district",
         LAD22CD = "na",
         `Total Cars or Vans` = "x2021") %>%
  left_join(.,
            LAD_boundaries,
            by = 'LAD22CD') %>%
  select(-c('LAD22NM.y')) %>%
  na.omit() %>%
  filter(., LAD22NM.x %in% c("Bradford",
                             "Leeds",
                             "Kirklees",
                             "Wakefield",
                             "Calderdale",
                             "York",
                             "Hull",
                             "East Riding of Yorkshire",
                             "Doncaster",
                             "Barnsley",
                             "North East Lincolnshire",
                             "Rotherham",
                             "Sheffield",
                             "North Lincolnshire",
                             "Craven",
                             "Hambleton",
                             "Harrogate",
                             "Richmondshire",
                             "Ryedale",
                             "Scarborough",
                             "Selby"))

cars_LAD<- st_as_sf(cars_LAD,
                    crs = 27700)

# Map the results
plot <- ggplot(cars_LAD) +
  geom_sf(aes(fill = `Total Cars or Vans`)) +
  scale_fill_continuous(labels = comma,
                        high = "#132B43",
                        low = "#56B1F7") +
  theme_void() +
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
    plot.background = element_rect(fill = "#EDE7D7",
                                   colour = "#EDE7D7"),
    panel.background = element_rect(fill = "#EDE7D7",
                                    colour = "#EDE7D7"),
    legend.position = "right"
  ) +
  annotation_north_arrow(location = "tr",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br") +
  labs(title = "Total Number of Cars or Vans per Local Authority in Yorkshire",
       caption = "Local Authority Boundaries: ONS Open Geography Portal, 2022; Data: Nomis Census 2021")

plot1 <- ggdraw(plot) + 
  draw_label(x=0.92,
             y=0.82,
             hjust=1, 
             "Census 2021 statistics includes data on car or van \navailability for household members. The Local \nAuthority in Yorkshire with the highest car or van \ncount is Leeds at 373,013. The Local Authority \nwith the lowest count is Richmondshire at 31,889.", 
             color = "#231f20",
             size = 12,
             fontfamily="sans") +
  theme(panel.background = element_rect(fill = "grey90",
                                        colour = "grey90"))
plot1

# Save the output
ggsave("Outputs/Day3_Polygons.png", dpi = 320, width = 13.5, height = 8.5, units = "in")