# Load the libraries
library(tidyverse)
library(sf)
library(mapgl)
library(readxl)
library(janitor)

# Load datasets
data <- read_excel("Data/nomis_2024_11_13_193257.xlsx")
boundaries <- st_read("Data/Local_Authority_Districts_May_2023_UK_BUC_V2_8757178717458576320.gpkg")

# Clean and join data
data_cleaned <- data %>%
  row_to_names(row_number = 8,
               remove_row = TRUE,
               remove_rows_above = TRUE) %>%
  clean_names() %>%
  rename(.,
         `Local Authority` = "na",
         LAD23CD = "na_2",
         Percentage = "percent_2") %>%
  left_join(.,
            boundaries,
            by = 'LAD23CD') %>%
  select(-c('number',
            'percent',
            'number_2',
            'LAD23NM',
            'LAD23NMW')) %>%
  na.omit()

# change to numeric
data_cleaned$Percentage <- as.numeric(data_cleaned$Percentage)

# Set as sf
data_sf <- st_as_sf(data_cleaned,
                    crs = 27700)

# Set up the mapbox config
health_map <- mapboxgl(mapbox_style("standard"),
                       bounds = data_sf)

# Set up popup
data_sf$popup <- glue::glue(
  "<strong>Code: </strong>{data_sf$LAD23CD}<br><strong>Local Authority: </strong>{data_sf$`Local Authority`}<br><strong>Percentage: </strong>{data_sf$Percentage}"
)

# Set up map output
map_output <- health_map %>% 
  add_fill_layer(
    id = "Local Authorities",
    source = data_sf,
    fill_color = interpolate(column = "Percentage",
      values = c(0.5,
                 2.4),
      stops = c("lightblue",
                "darkblue")),
    fill_opacity = 1,
    popup = "popup",
    tooltip = "Percentage",
    hover_options = list(
      fill_color = "lightgrey",
      fill_opacity = 1)) %>% 
  add_legend("Percentage of population with very bad health per local authority",
    values = c(0.5,
               2.4),
    colors = c("lightblue",
               "darkblue"))

# Produce map output
map_output

# Save map output
htmlwidgets::saveWidget(map_output, "Outputs/Day13_NewTool.html", selfcontained = TRUE)