prj_mat <- as.matrix(prj)

bayesplot_theme_set(ggplot2::theme_bw())
mcmc_intervals(prj_mat,
               pars = "b_x1191") +
  ggplot2::coord_cartesian()

refm_mat <- as.matrix(brms_normal)
mcmc_plot(brms_normal, type = "intervals", variable = "b_x1191") +
  ggplot2::coord_cartesian()

library(projpred)

# Carotenoid
p_carotenoid <- readRDS("/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/Github_data/Hierarchical_foliar_trait_estimation/data/code_output_data/projpred_files/Hierarchical_foliar_trait_estimationCarotenoid_Area_2024-02-14_projpred_cv.rds")

# Nitrogen
p_nitrogen <- readRDS("/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/Github_data/Hierarchical_foliar_trait_estimation/data/code_output_data/projpred_files/projpred_object_Nitrogen_raw_spectra_2023-12-21.rds")

#LMA
cv_out <- readRDS("/Users/dhruvakathuria/Downloads/twenty_clusters_LMA.rds")# LMA


/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/Github_data/Hierarchical_foliar_trait_estimation/data/code_output_data/brms_full_model_files/brms_object_LMA_raw_spectra_2023-12-26.rds
/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/Github_data/Hierarchical_foliar_trait_estimation/data/code_output_data/brms_full_model_files/brms_object_LMA_raw_spectra_2023-12-16.rds