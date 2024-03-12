# code for wavelength selection

library(bayesplot)
library(ggplot2)
library(brms)
library(projpred)
library(viridis)  # for the viridis color scale
library(ggrepel)  # for geom_text_repel

# Setting global parameters -----------------------------------------------
source("R_codes/input_parameter_file.R")
source("R_codes/Regression_algorithms/data_preprocessing_for_algorithms.R")
data_folder <- "/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/Github_data/Hierarchical_foliar_trait_estimation"


# get the means of spectra

spectra_means <- colMeans(data_train_for_hierarchical_analysis[, 11: ncol(data_train_for_hierarchical_analysis)])
spectra_means <- cbind(400:2400, spectra_means)
spectra_names <- paste0("x", spectra_means[, 1])

spectra_means <- data.frame(cbind(spectra_means, 
                       spectra_names))


trait_names_for_plot <- c("LMA" = "Leaf Mass per Area",
                          "Nitrogen" = "Nitrogen",
                          "Carotenoid_Area" = "Carotenoid")
date_vector = c("LMA" = "2023-12-26",
                "Nitrogen" = "2023-12-21",
                "Carotenoid_Area" = "2024-02-12")

nsel_vector <- c("LMA" = "30",
                 "Nitrogen" = "28" ,
                 "Carotenoid_Area" = "14")

out1_list <- list()
k = 0
for(trait_name1 in c("LMA",  "Nitrogen", "Carotenoid_Area"))
{
  k = k + 1
  date_for_brms_file <- date_vector[trait_name1] #this is the date the brms file was saved
  nsel <- nsel_vector[trait_name1]
  vsel <- readRDS(str_glue("{data_folder}/data/code_output_data/projpred_files/vsel_{trait_name1}_nsel_{nsel}_{date_for_brms_file}.rds"))
  vsel_df <- data.frame(spectra_names = vsel,
                         spectra_rank  = 1: length(vsel))
  
  out1 <- left_join(spectra_means, 
                    vsel_df,
                    by = join_by(spectra_names))
  
  # Create the plot with improved text label positioning
  out1$V1 <- as.numeric(out1$V1)
  out1$spectra_means <- round(as.numeric(out1$spectra_means), 3)
  
  out1$trait_name = trait_names_for_plot[trait_name1]
  out1_list[[k]] = out1
}

out2 <- data.table ::  rbindlist(out1_list)
options(ggrepel.max.overlaps = Inf)
ggplot(data = out2, aes(x = V1, y = spectra_means)) +
  geom_point(data = subset(out2, !is.na(spectra_rank)), aes(color = spectra_rank), size = 3, shape = 19, alpha = 0.6, na.rm = TRUE) +  # Colored points
  geom_point(data = subset(out2, is.na(spectra_rank)), color = "grey", size = 1, shape = 16, alpha = 0.6) +  # Grey points for NA 'spectra_rank'
  geom_text_repel(data = subset(out2, !is.na(spectra_rank)), aes(label = spectra_rank), size = 3, box.padding = 0.5, point.padding = 0.5, segment.color = 'grey50') +  # Repel text labels
  scale_color_viridis_c(option = "D", direction = -1, begin = 0, end = 1, space = "Lab", na.value = "grey50", guide = "colourbar") +  # Inverted Viridis color scale
  facet_wrap(~ trait_name, ncol = 2) +
  labs(
       x = "Spectra (nm)",
       y = "Reflectance",
       color = "Spectral Importance Rank") +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 14))

ggsave(filename = "paper_draft/figures/spectra_importance.png")

