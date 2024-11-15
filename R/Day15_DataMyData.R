# Load the libraries
library(tidyverse)
library(sf)
library(mapgl)
library(readxl)
library(janitor)

points <- data.frame(
  location = c("Cairo, Egypt",
               "Luxor, Egypt",
               "Bardoli, India",
               "Surat, India",
               "Goa, India",
               "Mumbai, India",
               "Agra, India",
               "Jaipur, India",
               "Marsa Alam, Egypt",
               "Sharm el Sheikh, Egypt",
               "Jerusalem",
               "Hebron (أل خليل), Palestine",
               "Bethlehem (بيت لحم), Palestine",
               "Istanbul, Turkey",
               "Makkah, Saudi Arabia",
               "Medina, Saudi Arabia",
               "Catania, Italy"),
  lon = c(31.1324808337068,
          32.601293278975746,
          73.11266848775624,
          72.82778585093585,
          73.76148217413456,
          72.87979834036558,
          78.0421314677319,
          75.81679027548545,
          34.89996954139787,
          34.360251544105616,
          35.235780196714096,
          35.09978303354599,
          35.202313702956204,
          28.976760014080757,
          39.82619251048436,
          39.611095423570234,
          15.087402396924894),
  lat = c(29.9774488798557,
          25.74051188537761,
          21.12627587468097,
          21.1726269992242,
          15.526944223245287,
          19.082177515685473,
          27.175221132696123,
          26.94271870140356,
          25.068282679333727,
          27.965502264853534,
          31.776205984793148,
          31.533007079424333,
          31.706074887403393,
          41.00552135661531,
          21.42248570633113,
          24.468599836517583,
          37.50323122019948))

points_sf <- st_as_sf(points, coords = c("lon", "lat"), crs = 4326)

travel_map <- mapboxgl(center = c(40,40),
                       bearings = 40) %>%
  set_config_property(
    "basemap",
    "lightPreset",
    "dusk" # Swap in "dawn", "day", or "dusk"
  ) %>%
  add_markers(
    data = points_sf, 
    color = "royalblue",
    popup = "location"
  )
  

travel_map

htmlwidgets::saveWidget(travel_map, "Outputs/Day15_DataMyData.html", selfcontained = TRUE)