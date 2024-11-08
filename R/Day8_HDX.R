# Load libraries
library(tidyverse)
library(sf)
library(ggspatial)
library(readxl)
library(janitor)
library(scales)
library(stringi)

# Load the data
data <- read_excel("Data/ind_hot.xlsx", sheet = 2)
boundaries <- st_read("Data/geoBoundaries-IND-ADM1-all/geoBoundaries-IND-ADM1.shp")

# Tidy the data
mpi <- data %>%
  select(c(8:9, 11)) %>%
  row_to_names(.,
               row_number = 7,
               remove_row = TRUE,
               remove_rows_above = TRUE) %>%
  clean_names() %>%
  rename(.,
         Year = "year",
         Region = "na",
         "Multidimensional Poverty Index Score" = "range_0_to_1") %>%
  na.omit() %>%
  filter(., Year == "2019-2021")

mpi$Region <- gsub("Jammu & Kashmir", "Jammu and Kashmir", mpi$Region)

boundaries$shapeName <- stringi::stri_trans_general(boundaries$shapeName, "Latin-ASCII")

india_mpi <- boundaries %>%
  left_join(., mpi, by = join_by("shapeName" == "Region"))

india_mpi$`Multidimensional Poverty Index Score` <- as.numeric(india_mpi$`Multidimensional Poverty Index`)

india_mpi$`Multidimensional Poverty Index Score` <- format(india_mpi$`Multidimensional Poverty Index`, scientific = FALSE)

india_mpi <- st_as_sf(india_mpi,
                      crs = "WGS 84")

india_mpi$`Multidimensional Poverty Index Score` <- as.numeric(india_mpi$`Multidimensional Poverty Index`)

# Plot map
plot <- ggplot(data = india_mpi) +
  geom_sf(aes(fill = `Multidimensional Poverty Index Score`)) +
  scale_fill_continuous(labels = comma,
                        high = "#BA0001",
                        low = "#F7B4BB",
                        na.value="white") +
  theme_classic() +
  theme(plot.title = element_text(colour = "black",
                                  hjust = 0,
                                  vjust = 0,
                                  size = 17),
        plot.caption = element_text(colour = "black",
                                    hjust = 1,
                                    face = "plain",
                                    size = 9),
        plot.subtitle = element_text(colour = "black",
                                     hjust = 0,
                                     vjust = 0,
                                     size = 13),
        plot.background = element_rect(fill = "#EDE7D7",
                                       colour = "#EDE7D7"),
        panel.background = element_rect(fill = "#EDE7D7",
                                        colour = "#EDE7D7"),
        text = element_text(family = "AppleGothic"),
        legend.position = "right",
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Harmonised Multidimensional Poverty Index scores across regions in India",
       subtitle = "Based on survey data between 2019 and 2021",
       caption = "*Blank/White spaces indicate no data is available for the region. \n\nSources found on HDX Website: \n\n Data: Alkire, S., Kanagaratnam, U., and Suppa, N. (2023). A methodological note on the global Multidimensional \nPoverty Index (MPI) 2023 changes over time results for 84 countries. OPHI MPI Methodological Note 57, \nOxford Poverty and Human Development Initiative. ©2018 University of Oxford \n\nBoundaries: geoBoundaries, https://www.geoboundaries.org") +
  annotation_north_arrow(location = "tr",
                         style = north_arrow_nautical(fill = c("black", "white")))

plot

# Save the output
ggsave("Outputs/Day8_HDX.png", dpi = 320, width = 11.1, height = 10.1, units = "in")