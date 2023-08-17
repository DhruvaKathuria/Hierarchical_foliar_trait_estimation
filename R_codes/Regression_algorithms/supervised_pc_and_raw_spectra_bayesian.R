library(dimreduce)

# Setting global parameters -----------------------------------------------

source("R_codes/input_parameter_file.R")

# source the data preprocessing code --------------------------------------

source("R_codes/Regression_algorithms/data_preprocessing_for_algorithms.R")

# formula for brms --------------------------------------------------------
input_x_names <- data_train_for_analysis |> 
  select(starts_with("x")) |> 
  colnames()

non_hierarchical_group = paste(input_x_names, 
                               collapse = "+")
formula_for_brms  = paste("trait ~ 1 + ", 
                   non_hierarchical_group, 
                   sep = "")

# brms implementation -----------------------------------------------------

brms_normal <- brm(as.formula(formula_for_brms), 
                   data= data_train_for_analysis, 
                   family= gaussian(),
                   prior=c(prior(horseshoe(par_ratio = par_ratio1),class="b")
                           #prior(normal(0, 0.05),class="b")
                   ),
                   chains = 1, cores = 1,
                   backend = "cmdstanr", 
                   #threads = threading(4), 
                   init = init_list,
                   warmup = 5000, iter = 15000, 
                   #sample_prior = "only"
                   #save_pars = save_pars(all = TRUE),
                   #control = list(adapt_delta = 0.95, max_treedepth = 15)
)

saveRDS(brms_normal,  
        paste0("data/code_output_data/brms_object_",
               trait_name1, 
               "_",
               prediction_algorithm,
               "_",
               Sys.Date(),
               ".rds"))


