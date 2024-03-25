library(bayesplot)
library(ggplot2)
library(brms)
library(projpred)

# Setting global parameters -----------------------------------------------
source("R_codes/input_parameter_file.R")
source("R_codes/Regression_algorithms/data_preprocessing_for_algorithms.R")


date_for_brms_file <- date_vector[trait_name1] #this is the date the brms file was saved
# in folder code data/code_output_data. brms
# files are saved using supervised_pc_and....R

# for personal macbooks
data_folder <- "/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/Github_data/Hierarchical_foliar_trait_estimation"

# Analysis for full model -------------------------------------------------
brms_normal <- readRDS(paste0(data_folder,  
                              "/data/code_output_data/brms_full_model_files/brms_object_",
                              trait_name1, 
                              "_",
                              prediction_algorithm,
                              "_",
                              date_for_brms_file,
                              ".rds"))


# Bayesian predictions full model -----------------------------------------

prediction_bayesian <- predict(brms_normal, data_test_for_analysis, probs = c(0.1, 0.9))
prediction_bayesian_df <- prediction_bayesian[, -2] * sd(data_train_for_hierarchical_analysis$trait) + 
  mean(data_train_for_hierarchical_analysis$trait) 

prediction_bayesian_df <- as.matrix(prediction_bayesian_df) |> data.frame()
prediction_bayesian_df <- rename(prediction_bayesian_df, 
                                 all_of(c(`bayesian (mean)` = "Estimate",
                                          `bayesian 10th %tile` = "Q10",
                                          `bayesian 90th %tile` = "Q90")))

## Mean predictions
data_test_for_analysis_Bayesian <- data_test_for_analysis |> bind_cols(prediction_bayesian_df)
data_test_for_analysis_Bayesian <- data_test_for_analysis_Bayesian |> 
  mutate(trait = trait * sd(data_train_for_hierarchical_analysis$trait) + 
           mean(data_train_for_hierarchical_analysis$trait) )

data_test_for_analysis_Bayesian <- data_test_for_analysis_Bayesian |> 
  select(!(starts_with("x")))

#CHANGE THIS LATER, DONT FORGET
if(trait_name1 == "LMA")
{
  data_frame_with_PLSR_predictions <- readRDS(str_glue("{data_folder}/data/code_output_data/PLSR_object_{trait_name1}.rds"))
}else{
  data_frame_with_PLSR_predictions <- readRDS(str_glue("{data_folder}/data/code_output_data/PLSR_object_LOO_{trait_name1}.rds"))
}
data_frame_with_PLSR_predictions <- janitor::clean_names(data_frame_with_PLSR_predictions)
data_test_predictions <- dplyr :: left_join(data_test_for_analysis_Bayesian, 
                                           data_frame_with_PLSR_predictions, 
                                           join_by(genus_species1, 
                                                   family1,
                                                   growth_form,
                                                   phenology,
                                                   leaf,
                                                   leaf_classification,
                                                   manufacturer,
                                                   model,
                                                   trait,
                                                   site_name))
data_test_predictions <- rename(data_test_predictions, PLSR = prediction_plsr)
data_test_predictions_long <- data_test_predictions |> 
  pivot_longer(cols = c(`bayesian (mean)`, PLSR, 
                        `bayesian 10th %tile`, `bayesian 90th %tile`),
               names_to = "prediction",
               values_to = "value")
data_test_predictions_long <- mutate(data_test_predictions_long,
                                     trait_name = trait_name1,
                                     model = "full model")




readr :: write_csv(data_test_predictions_long, 
                   str_glue("{data_folder}/data/code_output_data/predictions/prediction_file_full_model_{trait_name1}.csv"))



# reduced model -----------------------------------------------------------

# Reduced model predictions -----------------------------------------------
# cv_out <- readRDS(paste0(data_folder, "/data/code_output_data/proj_pred_object_",
#                          trait_name1, 
#                          "_",
#                          prediction_algorithm,
#                          "_",
#                          date_for_brms_file,
#                          ".rds"))

nsel <- nsel_vector[trait_name1]
#vsel <- ranking(cv_out)[["fulldata"]][1:30]
vsel <- readRDS(stringr :: str_glue("{data_folder}/data/code_output_data/projpred_files/vsel_{trait_name1}_nsel_{nsel}_{date_for_brms_file}.rds"))

prj <- projpred :: project(brms_normal, predictor_terms = vsel, ndraws = 5000)
prj_mat <- as.matrix(prj)
saveRDS(prj_mat, stringr :: str_glue("{data_folder}/data/code_output_data/projpred_files/{trait_name1}_posterior_parameter_matrix.rds"))

prj_linpred <- proj_linpred(prj, newdata = data_test_for_analysis, integrated = FALSE)
prj_linpred_pred <- prj_linpred$pred

for(i in 1:ncol(prj_linpred_pred))
{
  prj_linpred_pred[, i] = prj_linpred_pred[, i] * sd(data_train_for_hierarchical_analysis$trait) + mean(data_train_for_hierarchical_analysis$trait)
}

get_two_quantiles <- t(apply(prj_linpred_pred, 2, function(x) 
{
  quantile(x, probs = c(0.10, 0.90))
}))
mean_linpred_pred <- colMeans(prj_linpred_pred) 

pred_linpred <- data.frame(cbind(mean_linpred_pred, get_two_quantiles))
colnames(pred_linpred) = c("bayesian (mean)" ,
                           "bayesian 10th %tile",
                           "bayesian 90th %tile")

data_test_pred_projpred <- data_test_for_analysis |> 
  select(-starts_with("x")) |> 
  bind_cols(pred_linpred)

data_test_pred_projpred <- data_test_pred_projpred |> 
  mutate(trait = trait * sd(data_train_for_hierarchical_analysis$trait) + 
           mean(data_train_for_hierarchical_analysis$trait))

data_test_pred_projpred_long <- data_test_pred_projpred |> 
  pivot_longer(cols = c(`bayesian (mean)`, 
                        `bayesian 10th %tile`, `bayesian 90th %tile`),
               names_to = "prediction",
               values_to = "value")


data_test_pred_projpred_long <- mutate(data_test_pred_projpred_long,
                                       trait_name = trait_name1,
                                       model = "reduced model")

data_test_pred_complete <- data_test_predictions_long |> 
  bind_rows(data_test_pred_projpred_long)

readr :: write_csv(data_test_pred_complete, 
                   str_glue("{data_folder}/data/code_output_data/predictions/prediction_file_full_model_plus_reduced_model_{trait_name1}.csv"))


RMSE_function(data_test_pred_projpred$trait,
              data_test_pred_projpred$`bayesian (mean)`)

cor_function(data_test_pred_projpred$trait,
             data_test_pred_projpred$`bayesian (mean)`)
# make plots --------------------------------------------------------------

RMSE_function(data_test_predictions$trait,
              data_test_predictions$`bayesian (mean)`)

cor_function(data_test_predictions$trait,
             data_test_predictions$`bayesian (mean)`)

RMSE_function(data_test_predictions$trait,
              data_test_predictions$PLSR)

cor_function(data_test_predictions$trait,
             data_test_predictions$PLSR)

data_test_for_analysis_Bayesian |> 
  ggplot(aes(x = Estimate, y = trait)) +
  geom_point(col = "red", shape = 21, fill = "white") +
  geom_abline()

ggsave(filename = "bayesian_mean.png", 
       width = 3.5, 
       height = 3, 
       units = "in")

data_test_for_analysis_Bayesian |> 
  ggplot(aes(x = Estimate, y = trait)) +
  geom_errorbar(aes(xmin= Q10, xmax= Q90), colour="red", alpha = 0.4,  width=.1) + 
  geom_point(col = "red", shape = 21, fill = "white") +
  geom_abline()

ggsave(filename = "bayesian_intervals.png", 
       width = 3.5, 
       height = 3, 
       units = "in")

# PLSR predictions --------------------------------------------------------

data_frame_with_PLSR_predictions |> 
  ggplot(aes(x = Prediction_PLSR, y = trait)) +
  geom_point(col = "purple", shape = 21, fill = "white") +
  geom_abline()

ggsave(filename = "PLSR_mean.png", 
       width = 3.5, 
       height = 3, 
       units = "in")


RMSE_function(data_frame_with_PLSR_predictions$Prediction_PLSR, 
              data_frame_with_PLSR_predictions$trait)

cor_function(data_frame_with_PLSR_predictions$Prediction_PLSR, 
             data_frame_with_PLSR_predictions$trait)


# Reduced model predictions -----------------------------------------------
cv_out <- readRDS(paste0(data_folder, "/data/code_output_data/proj_pred_object_",
                         trait_name1, 
                         "_",
                         prediction_algorithm,
                         "_",
                         date_for_brms_file,
                         ".rds"))

plot(cv_out, 
     stats =  'rmse', 
     deltas=FALSE)
vsel <- solution_terms(cv_out)[1:8]
prj <- project(brms_normal, solution_terms = vsel)

prj_linpred <- proj_linpred(prj, newdata = data_test_for_analysis, integrated = TRUE)
prediction_bayesian_mean_projpred <- as.numeric(prj_linpred$pred) * sd(data_train_for_hierarchical_analysis$trait) + mean(data_train_for_hierarchical_analysis$trait)

data_test_for_analysis_reduced <- data_test_for_analysis |> mutate(reduced_model = prediction_bayesian_mean_projpred, 
                                                                   trait = trait * sd(data_train_for_hierarchical_analysis$trait) + 
                                                                     mean(data_train_for_hierarchical_analysis$trait))
data_test_for_analysis_reduced |> 
  ggplot(aes(x = reduced_model, y = trait)) +
  geom_point(col = "maroon", shape = 21, fill = "white") +
  geom_abline() + 
  xlab("Estimate")

ggsave(filename = "reduced_mean.png", 
       width = 3.5, 
       height = 3, 
       units = "in")

RMSE_function(data_test_for_analysis_reduced$trait,
              data_test_for_analysis_reduced$reduced_model)

cor_function(data_test_for_analysis_reduced$trait,
             data_test_for_analysis_reduced$reduced_model)


# parameter uncertainty of reduced model ----------------------------------

prj_mat <- as.matrix(prj)

bayesplot_theme_set(ggplot2::theme_bw())
mcmc_areas(prj_mat, prob = 0.8, 
               pars = c("b_Intercept", "b_x713", "b_x610", "sigma" )) +
  ggplot2::coord_cartesian()

bayesplot_theme_set(ggplot2::theme_bw())
mcmc_areas(prj_mat, prob = 0.8, 
           pars = "b_x610") +
  ggplot2::coord_cartesian()

ggsave(filename = "parameter_uncertainty_Carotenoid_Area.png")
