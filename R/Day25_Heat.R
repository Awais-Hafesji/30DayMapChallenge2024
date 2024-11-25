# Load libraries
library(tidyverse)
library(sf)
library(geogrid)
library(ggspatial)
library(cowplot)

# Load the data
data <- read_csv("Data/TS046-2021-4-filtered-2024-11-25T21-20-26Z.csv")
boundaries <- st_read("Data/Local_Authority_Districts_May_2023_UK_BUC_V2_8757178717458576320.gpkg")

# Filter data
heating <- data %>%
  filter(.,
         `Type of central heating in household (13 categories) Code` == 9) %>%
  filter(.,
         `2023 Lower tier local authorities` %in% c("Barking and Dagenham",
                                                    "Barnet",
                                                    "Bexley",
                                                    "Brent",
                                                    "Bromley",
                                                    "Camden",
                                                    "City of London",
                                                    "Croydon",
                                                    "Ealing",
                                                    "Enfield",
                                                    "Greenwich",
                                                    "Hackney",
                                                    "Hammersmith and Fulham",
                                                    "Haringey",
                                                    "Harrow",
                                                    "Havering",
                                                    "Hillingdon",
                                                    "Hounslow",
                                                    "Islington",
                                                    "Kensington and Chelsea",
                                                    "Kingston upon Thames",
                                                    "Lambeth",
                                                    "Lewisham",
                                                    "Merton",
                                                    "Newham",
                                                    "Redbridge",
                                                    "Richmond upon Thames",
                                                    "Southwark",
                                                    "Sutton",
                                                    "Tower Hamlets",
                                                    "Waltham Forest",
                                                    "Wandsworth",
                                                    "Westminster"))



heating_LAD <- left_join(heating,
                             boundaries,
                             by = c("2023 Lower tier local authorities Code" = "LAD23CD")) %>%
  select(., -c(6:7))

# Convert to sf
heating_sf <- st_as_sf(heating_LAD)

# Create hex grid
hg = calculate_grid(heating_sf, learning_rate = 0.05, 
                    grid_type = "hexagonal", verbose = F)
# Extract the spatial objects
hg_points = st_as_sf(hg[[1]])
hg_polys = st_as_sf(hg[[2]])

# Assign data to hex
hg_heating = assign_polygons(heating_sf, hg)

# Plot the map!
plot <- ggplot(hg_heating) +
        geom_sf(aes(fill = Observation),
                col = "white",
                lwd = 0.1) +
        scale_fill_viridis_c(option = "magma",
                             name = "Number of Households") +
        theme_classic() +
        theme(legend.position = "right",
              legend.background = element_rect(fill = "#424242"),
              legend.text = element_text(colour = "white",
                                         family = "Giphurs"),
              legend.title = element_text(colour = "white",
                                          family = "Giphurs"),
              plot.title = element_text(hjust = 0.5,
                                        vjust = 6,
                                        size = 20,
                                        colour = "white",
                                        family = "Giphurs",
                                        face = "bold"),
              plot.subtitle = element_text(hjust = 0.5,
                                           vjust = 6,
                                           size = 12,
                                           colour = "white",
                                           family = "Giphurs"),
              plot.caption = element_text(colour = "white",
                                          family = "Giphurs",
                                          hjust = 0.5,
                                          vjust = (-10)),
              plot.background = element_rect(fill = "#242424",
                                             colour = "#242424"),
              panel.background = element_rect(fill = "#242424",
                                              colour = "#242424"),
              axis.ticks = element_line(colour = "white"),
              axis.line = element_line(colour = "white"),
              axis.text.x = element_text(colour = "white",
                                         family = "Giphurs"),
              axis.text.y = element_text(colour = "white",
                                         family = "Giphurs"),
              plot.margin = unit(c(2, 0, 2, 1), "cm")) +
        labs(title = "Number of Households Using \n\"District or Communal Heat Networks Only\"\nper Local Authority in London",
             subtitle = "Based on Census 2021 survey data",
             caption = "\nData: ONS Census 2021\nBoundaries: ONS Open Geography Portal, 2023",
             ) +
        annotation_scale(aes(line_col = "white", text_col = "white"),
                         location = "br",
                         bar_cols = c("white"),
                         style = "ticks",
                         pad_y = unit(0.45, "cm"),
                         text_family = "Giphurs")
  
plot

# Save the output
ggsave("Outputs/Day25_Heat.png", dpi = 320, height = 8, width = 9, units = "in")