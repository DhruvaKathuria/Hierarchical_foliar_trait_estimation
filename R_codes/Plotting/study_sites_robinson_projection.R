# Load necessary libraries
library(terra)
library(stars)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# Example data frame, replace this with reading your actual data
# Assuming the file is a CSV named "coordinates.csv" with columns "longitude" and "latitude"
location_file <- readr :: read_csv("data/locations_of_study_sites.csv")

location_file <- location_file |> 
  mutate(trait = case_when(
    data_split_type == "test" ~ "test",
    TRUE ~ trait
  )) |> 
  unique()

# Convert the data frame to an sf object
points_sf <- st_as_sf(location_file, coords = c("longitude", "latitude"), crs = 4326)

# Transform the coordinates to Robinson projection
points_rob <- st_transform(points_sf, crs = "+proj=robin")

# Create a world map in Robinson projection
world <- ne_countries(scale = "medium", returnclass = "sf")
world_rob <- st_transform(world, crs = "+proj=robin")

points_coords <- st_coordinates(points_rob)
points_df <- data.frame(points_coords, location_file)

# points_df$data_split_type <-  factor(points_df$data_split_type,
#                                                    levels = c("train", "test"))
# Plot the world map and the points
study_sites <- ggplot() +
  geom_sf(data = world_rob, fill = "grey", color = "black") +
  geom_point(data = points_df, aes(x = X, y = Y, color =  trait, fill = trait), 
             size = 2, shape = 21,
             position = position_jitter(width = 120000, height = 120000)) +
  theme_minimal() +
  labs(title = "",
       x = "Longitude",
       y = "Latitude") +
  coord_sf(ylim = c(-5000000, 8340316), expand = FALSE)  +
  scale_color_manual(values=c("#F8766D", "#00BA38", "#619CFF", "#800080"),
                    name="",
                    #breaks=c("f", "r", "4"),
                    labels=c("Carotenoid", "LMA", "Nitrogen")
                    ) +
  scale_fill_manual(values=c("#F8766D80", "#00BA3880", "#619CFF80", "#80008080"),
                    name="",
                    #breaks=c("f", "r", "4"),
                    labels=c("Carotenoid", "LMA", "Nitrogen")
                    ) +
  theme(legend.position = "none",
        legend.text = element_text(size = 12), # Increase legend text size
        legend.title = element_text(size = 14),
        plot.tag = element_text(size = 16, 
                                face = "bold",
                                family = "serif")
        )  +
  labs(tag = "(a)")

study_sites

ggsave(filename = "paper_draft/figures/study_sites.png",
       study_sites,
       dpi = 600,
       width = 9,
       height = 4,
       units = "in")
