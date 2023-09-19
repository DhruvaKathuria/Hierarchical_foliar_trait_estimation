library(bayesplot)
library(ggplot2)
library(brms)

# Setting global parameters -----------------------------------------------
source("R_codes/input_parameter_file.R")
date_for_brms_file <- "2023-08-14" #this is the date the brms file was saved
# in folder code data/code_output_data. brms
# files are saved using supervised_pc_and....R

# for personal macbooks
data_folder <- "/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/Github_data/Hierarchical_foliar_trait_estimation"

# Analysis for full model -------------------------------------------------
brms_normal <- readRDS(paste0(data_folder,  "/data/code_output_data/brms_object_",
                              trait_name1, 
                              "_",
                              prediction_algorithm,
                              "_",
                              date_for_brms_file,
                              ".rds"))

data1 <- brms_normal$data

mcmc_plot(brms_normal,
          type = "dens",
          variable = "sigma", 
          prob = 0.8)

pp_check(brms_normal,
         ndraws = 1000)

mcmc_trace(brms_normal, pars = "b_x410")
# read brms file 

mcmc_plot(brms_normal, type = "pairs", variable = c("b_x850", "b_x865"))

# posterior predictive checks plots ---------------------------------------
ppd <- posterior_predict(brms_normal, 
                         ndraws = 10000,
                         newdata = data_test_for_analysis)

ppd1 <- ppd*sd(data_train_for_hierarchical_analysis$trait) + mean(data_train_for_hierarchical_analysis$trait)

ppd1  %>% 
  ppc_intervals(y = as.vector(data_test_for_hierarchical_analysis$trait), 
                yrep = ., as.vector(data_test_for_hierarchical_analysis$trait), 
                prob = 0.90)


# how many observations lie inside intervals ------------------------------

ninety_percent_quantiles <- t(apply(ppd, 2, function(x)quantile(x, c(0.1, 0.9))))
actual
ninety_percent_quantiles <- mclapply(
                          

