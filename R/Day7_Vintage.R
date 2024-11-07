# Load libraries
library(tidyverse)
library(sf)
library(ggspatial)
library(readxl)
library(janitor)
library(scales)

# Load in files
gdhi <- read_xlsx("Data/nomis_2024_11_07_202816.xlsx")
LAD <- st_read("Data/Local_Authority_Districts_December_2021_UK_BFE_2022_-2286327633816853381.gpkg")

# Join the data
gdhi_LAD <- gdhi %>%
  row_to_names(row_number = 6,
               remove_row = TRUE,
               remove_rows_above = TRUE) %>%
  clean_names() %>%
  rename(.,
         LAD21NM = "local_authority_district_unitary_as_of_april_2021",
         LAD21CD = "na",
         `GDHI (£m)` = "x2021") %>%
  left_join(.,
            LAD,
            by = 'LAD21CD') %>%
  select(-c('LAD21NM.y')) %>%
  na.omit()

# Convert to sf with CRS 27700
gdhi_LAD <- st_as_sf(gdhi_LAD,
                     crs = 27700)

# Plot map
plot <- ggplot(data = gdhi_LAD) +
        geom_sf(aes(fill = `GDHI (£m)`)) +
  scale_fill_continuous(labels = comma,
                        high = "#3b342a",
                        low = "beige") +
  theme_void() +
  theme(plot.title = element_text(colour = "black",
                                  hjust = 0,
                                  vjust = 0,
                                  size = 17),
        plot.caption = element_text(colour = "black",
                                    hjust = 1,
                                    face = "plain",
                                    size = 9),
        plot.background = element_rect(fill = "#D2BD96",
                                       colour = "#D2BD96"),
        panel.background = element_rect(fill = "#D2BD96",
                                        colour = "#D2BD96"),
        text = element_text(family = "Times New Roman",
                            face = "bold"),
        legend.position = "right",
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Gross Disposable Household Income (GDHI) \nin UK Local Authority Districts",
       caption = "Data: Nomis, 2021; Boundaries: ONS, 2021") +
  annotation_north_arrow(location = "tr",
                         style = north_arrow_nautical(fill = c("black", "beige"))) +
  annotation_scale(location = "bl",
                   bar_cols = c("black", "beige"),
                   line_width = 0.5,
                   text_family = "Times New Roman")

plot

# Save the output 
ggsave("Outputs/Day7_Vintage.png", dpi = 320, width = 6.5, height = 9.75, units = "in")