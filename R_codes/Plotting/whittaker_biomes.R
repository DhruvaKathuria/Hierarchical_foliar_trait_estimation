library(plotbiomes)
library(ggplot2)
library(terra)

# reference website: https://rawgit.com/valentinitnelav/plotbiomes/master/html/Whittaker_biomes_examples.html


# data --------------------------------------------------------------------

location_file <- readr :: read_csv("data/locations_of_study_sites.csv")


# prepare the raster stack for precipitation and temperature --------------
# Low resolution raster datasets come with 'plotbiomes' package.
path <- system.file("extdata", "temp_pp.tif", package = "plotbiomes")
temp_pp <- raster::stack(path)
names(temp_pp) <- c("temperature", "precipitation")

# get the location of points to be represented in the whittaker biome
points <- location_file |> 
  select(longitude, latitude, trait) 

# ===== Extract from raster stack
# Extract temperature and precipitation values from the raster datasets
extractions <- terra::extract(temp_pp, 
                              points |> select(longitude, latitude), 
                              df = TRUE)
# Adjust temperature values to normal scale because WorldClim temperature data
# has a scale factor of 10 (integer storage for saving space).
# Convert precipitation from mm to cm
extractions <- extractions |> 
  mutate(temperature = temperature/10,
         precipitation  = precipitation/10,
         trait = points$trait)

#plot(temp_pp[[1]]/10); points(points)
#plot(temp_pp[[2]]); points(points)


# whittaker plot ----------------------------------------------------------

plot_3 <- whittaker_base_plot() +
  geom_point(data = extractions,
             aes(x = temperature,
                 y = precipitation),
             shape  = 21,
             stroke = 0, # acts as the thickness of the boundary line
             colour = "gray95", # acts as the color of the boundary line
             size   = 3.5) +
  geom_point(data = extractions,
             aes(x = temperature,
                 y = precipitation,
                 color = trait),
             shape = 16,
             size  = 3,
             alpha = 1,
             position = position_jitter(width = 0.01 * diff(range(extractions$temperature)),
                                        height = 0.01 * diff(range(extractions$precipitation)))) +
  scale_color_manual(values=c("#F8766D80", "#00BA3880", "#619CFF80"),
                     name="",
                     #breaks=c("f", "r", "4"),
                     labels=c("Carotenoid", "LMA", "Nitrogen")
                     ) 

# adding the tag (b) for paper figure -------------------------------------


plot_4 <- plot_3 + 
  theme_bw() +
  theme(plot.tag = element_text(size = 16, 
                                face = "bold",
                                family = "serif"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)
  ) +
  labs(tag = "(b)") 
  


# Uncomment below If you want to have the legend inside the box ---------------------------

# my_plot <- plot_3 +
#   # Optional - Overwrite axis ranges (the scale warning is expected):
#   # - set range on OY axes and adjust the distance (gap) from OX axes
#   scale_y_continuous(name = 'Precipitation (cm)',
#                      limits = c(min = -5, max = ceiling(max(460, extractions$precipitation)/10)*10) ,
#                      expand = c(0, 0)) +
#   # - set range on OX axes and adjust the distance (gap) from OY axes
#   scale_x_continuous(name = expression("Temperature " ( degree*C)),
#                      limits = c(min = floor(min(-20, extractions$temperature)/5)*5, max = 30.5),
#                      expand = c(0, 0)) +
#   coord_fixed(ratio = 1/10) + # aspect ratio, expressed as y / x
#   theme_bw() +
#   theme(
#     legend.justification = c(0, 1), # pick the upper left corner of the legend box and
#     legend.position = c(0, 1), # adjust the position of the corner as relative to axis
#     legend.background = element_rect(fill = NA), # transparent legend background
#     legend.box = "horizontal", # horizontal arrangement of multiple legends
#     legend.spacing.x = unit(0.5, units = "cm"), # horizontal spacing between legends
#     panel.grid = element_blank(), # eliminate grids,
#     #legend.text = element_text(size = 8), # Increase legend text size
#     #legend.title = element_text(size = 8)
#     plot.tag = element_text(size = 16, 
#                             face = "bold",
#                             family = "serif")
#   ) + 
#   labs(tag = "(b)")

ggsave(filename = "paper_draft/figures/whittaker_biome.png",
       plot_4,
       dpi = 600,
       width = 9,
       height = 4.5,
       units = "in")
