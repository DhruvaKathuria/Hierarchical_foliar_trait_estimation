library(brms)

# Setting global parameters -----------------------------------------------

source("R_codes/input_parameter_file.R")

date_for_brms_object <- "2023-08-14"
brms_normal <- readRDS(paste0("data/code_output_data/brms_object_",
                                trait_name1, 
                                "_",
                                prediction_algorithm,
                                "_",
                                date_for_brms_object,
                                ".rds"))


# Compare the Bayesian results with PLSR results --------------------------

prediction_bayesian <- predict(brms_normal, data_test_for_analysis)
prediction_bayesian_mean <- prediction_bayesian[, 1] * sd(data_train_for_hierarchical_analysis$trait) + mean(data_train_for_hierarchical_analysis$trait)

plot(prediction_bayesian_mean, data_test_for_hierarchical_analysis$trait, pch = 19)
abline(0, 1)
RMSE_function(prediction_bayesian_mean, data_test_for_hierarchical_analysis$trait)
cor_function(prediction_bayesian_mean, data_test_for_hierarchical_analysis$trait)

plot(data_frame_with_PLSR_predictions$Prediction_PLSR, 
     data_frame_with_PLSR_predictions$trait, pch = 19,  xlim = c(0, 50))
abline(0, 1)
RMSE_function(data_frame_with_PLSR_predictions$Prediction_PLSR, 
              data_frame_with_PLSR_predictions$trait)
cor_function(data_frame_with_PLSR_predictions$Prediction_PLSR, 
             data_frame_with_PLSR_predictions$trait)


# Posterior parameters ----------------------------------------------------

parameters1 <- fixef(brms_read)
plot(parameters1[, 1], type = "l")


# get loading for supervised PC -------------------------------------------

if(prediction_algorithm == "supervised_pc")
{
  # get the coefficients back
  param_z <- as.data.frame(brms_read)
  alpha_z <- t(param_z[, 1])
  beta_z <- t(param_z[, 2:21])
  
  # transform the linear model back to original feature space
  param_x <- coef_transform(get_super_pcs, beta_z, alpha_z)
  beta_x <- param_x$beta
  alpha_x <- param_x$alpha
  
  intervals <- apply(beta_x, 1, function(x) {quantile(x, probs = c(0.05, 0.5, 0.95), na.rm = T)})
  
  ggplot() +
    geom_linerange(aes(x = 399:2400, ymin = intervals[1, ], ymax = intervals[3, ])) +
    geom_point(aes(x = 399:2400, y = intervals[2, ])) +
    ylab("Coefficient") +
    xlab("Feature")
}
