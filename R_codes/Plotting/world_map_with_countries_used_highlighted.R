library(ggplot2)
library(maps)
library(mapdata)

# Load world map data
world_map <- map_data("world")

# List of countries to color
countries_to_color <- c("USA", "France", "Canada", "Hungary", "Italy", "Belgium", "Japan", "China")

# Create a new column in the world map dataframe, indicating whether the country should be colored
world_map$country_colored <- ifelse(world_map$region %in% countries_to_color, world_map$region, "Other")

# Define the colors for the countries
country_colors <- c("USA" = "firebrick1", "France" = "deepskyblue4", "Canada" = "darkorchid1", "Hungary" = "orange",
                    "Italy" = "seagreen3", "Belgium" = "gold1", "Japan" = "forestgreen", "China" = "cyan", "Other" = "gainsboro")

# Create the ggplot world map, coloring the specified countries with different colors
ggplot(data = world_map, aes(x = long, y = lat, group = group, fill = country_colored)) +
  geom_polygon(color = "dimgrey") +
  scale_fill_manual(values = country_colors, guide = "none") +
  theme_minimal() +
  labs(title = "World Map with Selected Countries Colored") +
  theme(plot.title = element_text(hjust = 0.5))

