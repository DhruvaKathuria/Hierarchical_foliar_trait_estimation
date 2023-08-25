library(posterior)
library(bayesplot)
# Global parameters -------------------------------------------------------

trait_name1 = "Carotenoid_Area"
prediction_algorithm <- "raw_spectra"
date_for_brms_file <- "2023-06-29" #this is the date the brms file was saved
# in folder code data/code_output_data. brms
# files are saved using supervised_pc_and....R

cv_out <- readRDS(paste0("data/code_output_data/proj_pred_object_",
                         trait_name1, 
                         "_",
                         prediction_algorithm,
                         "_",
                         date_for_brms_file,
                         ".rds"))


prj <- project(brms_normal, solution_terms = vsel)
prj_mat <- as.matrix(prj)

bayesplot_theme_set(ggplot2::theme_bw())
mcmc_intervals(prj_mat) +
  ggplot2::coord_cartesian()

refm_mat <- as.matrix(brms_normal)
mcmc_intervals(refm_mat, pars = colnames(prj_mat)) +
  ggplot2::coord_cartesian(xlim = c(-5, 3))

prj_linpred <- proj_linpred(prj, newdata = data_test_for_analysis, integrated = TRUE)
prediction_bayesian_mean_projpred <- as.numeric(prj_linpred$pred) * sd(data_train_for_hierarchical_analysis$trait) + mean(data_train_for_hierarchical_analysis$trait)

plot(prediction_bayesian_mean_projpred, data_test_for_hierarchical_analysis$trait, pch = 19)
abline(0, 1)

RMSE_function(prediction_bayesian_mean_projpred, data_test_for_hierarchical_analysis$trait)
cor_function(prediction_bayesian_mean_projpred, data_test_for_hierarchical_analysis$trait)
