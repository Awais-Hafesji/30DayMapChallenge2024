# Load the libraries
library(tidyverse)
library(sf)
library(ggtext)

# Load in boundaries
eng_boundaries <- st_read("Data/Regions_December_2023_Boundaries_EN_BUC_-2773882830541122124.gpkg")

# Create the bread names column
bread_names <- c("Bun",
                 "Barmcake/Barm/Muffin/Oven Bottom/",
                 "Teacake/Breadcake/Stotty/Scuffler/",
                 "Cob",
                 "Bap",
                 "Batch",
                 "Roll",
                 "Roll",
                 "Bun/Roll/Batch/Cob/")

# Join the bread names column to the boundaries
eng_boundaries$Bread = bread_names

# Create the grid out of the boundaries
grid <- st_make_grid(eng_boundaries,
                     n = c(55,55)) %>% 
  st_centroid() %>%
  st_as_sf() %>%
  st_filter(eng_boundaries,
            .predicate = st_intersects)

# Join the boundaries and grid together
grid_regions <- grid %>% 
  st_join(eng_boundaries,
          join = st_intersects)

# Spread the bread names across each county's part of the grid
grid_bread <- grid_regions %>% 
  group_split(Bread,
              .keep = TRUE) %>% 
  map(function(x) {
    x %>% mutate(row = row_number(),
                 row_rest = (row_number() - 1) %% str_length(Bread) + 1,
                 foo = Bread,
                 names = unlist(str_split(Bread,
                                          pattern = ""))[row_rest])}) %>% 
  bind_rows()

# Plot the map!
ggplot(grid_bread) + 
  geom_sf_text(
    aes(label = toupper(names),
                        colour = Bread), 
        size = 3.5,
        family = "Redaction",
        fontface = "bold",
        show.legend = FALSE) +
  labs(caption = "Data: The Great British Bread Debate; Baked to Taste; Stephen Liddell's Blog",
       title = "Different Names of Bread Across Regions in England") +
  theme_void(base_family = "Redaction") +
  theme(plot.background = element_rect(color = "white",
                                       fill = "white"),
        plot.caption = element_text(size = 8),
        plot.margin = margin(rep(4,4)))

# Save the output
ggsave("Outputs/Day19_Typography.png", dpi = 320, width = 6, height = 8, units = "in")