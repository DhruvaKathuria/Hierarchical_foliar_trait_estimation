library(plotbiomes)
library(ggplot2)

location_file <- readr :: read_csv("data/locations_of_study_sites.csv") 
plot_1 <- ggplot() +
  # add biome polygons
  geom_polygon(data = Whittaker_biomes,
               aes(x    = temp_c,
                   y    = precp_cm,
                   fill = biome),
               # adjust polygon borders
               colour = "gray98",
               linewidth   = 1) +
  theme_bw()
plot_1


# example with map --------------------------------------------------------

library(terra)
library(maptools)

# ===== Prepare raster stack
# Read temperature and precipitation as raster stack.
# Low resolution raster datasets come with 'plotbiomes' package.
path <- system.file("extdata", "temp_pp.tif", package = "plotbiomes")
temp_pp <- raster::stack(path)
names(temp_pp) <- c("temperature", "precipitation")

# ===== Generate random locations
data(wrld_simpl) # load world polygons from maptools
# Eliminate Antarctica - it is not represented in the raster datasets.
wrld_simpl <- wrld_simpl[wrld_simpl$NAME != "Antarctica", ]
# Create random locations within world's polygons.
set.seed(66) # random number generator
points <- sp::spsample(x = wrld_simpl, n = 50, type = "random")
  points <- location_file |> 
  select(longitude, latitude) 

# ===== Extract from raster stack
# Extract temperature and precipitation values from the raster datasets
extractions <- terra::extract(temp_pp, points, df = TRUE)
# Adjust temperature values to normal scale because WorldClim temperature data
# has a scale factor of 10 (integer storage for saving space).
extractions$temperature <- extractions$temperature/10
# Convert precipitation from mm to cm
extractions$precipitation <- extractions$precipitation/10

plot(temp_pp[[1]]/10); points(points)
plot(temp_pp[[2]]); points(points)

whittaker_base_plot() +
  # add the temperature - precipitation data points
  geom_point(data = extractions, 
             aes(x = temperature, 
                 y = precipitation), 
             size   = 3,
             shape  = 21,
             colour = "gray95", 
             fill   = "black",
             stroke = 1,
             alpha  = 0.5) +
  theme_bw()
