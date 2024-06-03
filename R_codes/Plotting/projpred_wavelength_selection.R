# projpred files for each covariate
# Carotenoid
library(projpred)

projpred_folder <- "/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/Github_data/Hierarchical_foliar_trait_estimation/data/code_output_data/projpred_files/"

car_file <- readRDS(str_glue("{projpred_folder}/Hierarchical_foliar_trait_estimationCarotenoid_Area_2024-02-14_projpred_cv.rds"))
n_file <- readRDS(str_glue("{projpred_folder}/projpred_object_Nitrogen_raw_spectra_2023-12-21.rds"))
#rk <- ranking(car_file)[["fulldata"]]
#rk2 <- ranking(car_file)[["foldwise"]]


p1 <- plot(car_file,
     deltas = T,
     baseline = "ref",
     stat = "rmse",
     ranking_abbreviate = F,
     #ranking_repel = "text",
     #ranking_repel_args = list(box.padding  = 0.5),
     show_cv_proportions = F,
     text_angle = 90,
     size_position = "secondary_x") 
p1 +
  xlab("Predictor wavelength(nm)") +
  ylab ("Difference between submodel and full model") +
  labs(title = "Carotenoid", subtitle = NULL) + 
  theme(strip.text = element_text(size = 12))

ggsave(filename = "paper_draft/figures/carotenoid_cv_results.png")

#suggest_size(car_file, stat = "mse")

#nitrogen

n_file <- readRDS(str_glue("{projpred_folder}/projpred_object_Nitrogen_raw_spectra_2023-12-21.rds"))

p2 <- plot(n_file,
           deltas = T,
           baseline = "ref",
           stat = "rmse",
           ranking_abbreviate = F,
           #ranking_repel = "text",
           #ranking_repel_args = list(box.padding  = 0.5),
           show_cv_proportions = F,
           text_angle = 90,
           size_position = "secondary_x") 

p2 +
  xlab("Predictor wavelength(nm)") +
  ylab ("Difference between submodel and full model") +
  labs(title = "Nitrogen", subtitle = NULL) + 
  theme(strip.text = element_text(size = 12))

ggsave(filename = "paper_draft/figures/nitrogen_cv_results.png")

suggest_size(n_file,
             baseline = "ref",
             stat = "mse",
             type = "upper")

#lma

lma_file <- readRDS("/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/Github_data/Hierarchical_foliar_trait_estimation/data/code_output_data/projpred_files/twenty_clusters_LMA.rds")
p3 <- plot(lma_file,
           deltas = T,
           baseline = "ref",
           stat = "rmse",
           ranking_abbreviate = F,
           #ranking_repel = "text",
           #ranking_repel_args = list(box.padding  = 0.5),
           show_cv_proportions = F,
           text_angle = 90,
           size_position = "secondary_x") 
p3 +
  xlab("Predictor wavelength(nm)") +
  ylab ("Difference between submodel and full model") +
  labs(title = "Carotenoid", subtitle = NULL) + 
  theme(strip.text = element_text(size = 12))