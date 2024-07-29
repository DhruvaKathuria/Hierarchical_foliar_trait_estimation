library(microbenchmark)
#library(bayesplot)
#library(ggplot2)
library(brms)
library(projpred)

# Setting global parameters -----------------------------------------------
source("R_codes/input_parameter_file.R")
source("R_codes/Regression_algorithms/data_preprocessing_for_algorithms.R")

rm(list = setdiff(ls(), "data_test_for_analysis"))
source("R_codes/input_parameter_file.R")
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

newdata_rows <- sample(1:nrow(data_test_for_analysis), 50000, replace = T)
data1 <- data_test_for_analysis[newdata_rows, ]
rm(newdata_rows)

m1_brms <- microbenchmark(
  p_predict1  <- predict(brms_normal, data1, ndraws = 5000), 
  times = 20
  )

saveRDS(m1_brms, file = str_glue("data/code_output_data/algorithm_computational_timings/brms_{trait_name1}.rds"))

nsel <- nsel_vector[trait_name1]
vsel <- readRDS(stringr :: str_glue("{data_folder}/data/code_output_data/projpred_files/vsel_{trait_name1}_nsel_{nsel}_{date_for_brms_file}.rds"))

prj <- projpred :: project(brms_normal, predictor_terms = vsel, ndraws = 5000)
#prj_linpred <- proj_linpred(prj, newdata = data_test_for_analysis[1:2, ], integrated = FALSE)
#prj_pred <- prj_linpred$pred

m1_projpred <- microbenchmark(
  p_predict1  <- proj_predict(prj, newdata = data1), 
  times = 20
)

saveRDS(m1_projpred, file = str_glue("data/code_output_data/algorithm_computational_timings/projpred_{trait_name1}.rds"))

system.time(prj_posterior <- proj_predict(prj, newdata = data1))

# prj_out <- prj$outdmin
# 
# beta_short_matrix <- matrix(NA, ncol = length(vsel), nrow = 5000)
# alpha_short_vector <- vector()
# for(i in 1: nrow(beta_short_matrix))
# {
#   alpha_short_vector[i] = prj_out[[i]]$alpha
#   beta_short_matrix[i, ] <- prj_out[[i]]$beta
# }
# 
# data_test <- data_test_for_analysis |> 
#   select(all_of(vsel)) |> 
#   mutate(Intercept = 1,
#          .before = vsel[1]) |> 
#   as.matrix()
# 
# beta_short_comp <- cbind(alpha_short_vector, beta_short_matrix)
# 
# out_pred_short <- data_test[1:2, ] %*%  t(beta_short_comp) 
# 
# 
# ## doing the same for brms
# data_test_for_long <- data_test_for_analysis |> 
#   select(starts_with("x")) 
# data_test_for_long <- data_test_for_long |> 
#   mutate(Intercept = 1,
#          .before = colnames(data_test_for_long)[1]) |> 
#   as.matrix()
# 
# 
# parameters_brms <- fixef(brms_normal, summary = F)
# sample_draws <- sample(1: nrow(parameters_brms),
#                        5000)
# parameters_brms_draws = parameters_brms[sample_draws, ]
# out_pred_long <- data_test_for_long[1:2, ] %*%  t(parameters_brms_draws) 
# 
# predict_brms <- system.time(predict(brms_normal, data_test_for_analysis, ndraws = 5000))
