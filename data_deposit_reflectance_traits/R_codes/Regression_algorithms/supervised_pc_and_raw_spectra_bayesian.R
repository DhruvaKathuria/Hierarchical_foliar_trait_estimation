#library(dimreduce)

# Setting global parameters -----------------------------------------------
trait_name1 = "Carotenoid_Area" # options are "LMA", Nitrogen", and "Carotenoid_Area"
source("R_codes/input_parameter_file.R")

# source the data preprocessing code --------------------------------------
## if running the full analysis, uncomment the below. For the example of Carotenoid_Area, keep the below line commented out

#source("R_codes/Regression_algorithms/data_preprocessing_for_algorithms.R")

## For running the Github example: kindly run the below two lined to load the training data for Carotenoid. If doing the full analysis, comment out the below line, and uncomment the line above
data_train_for_analysis = read_csv("data_train_Carotenoid_Area.csv")
par_ratio1 = 0.025

# brms analysis
input_x_names <- data_train_for_analysis |> 
  select(starts_with("x")) |> 
  colnames()

non_hierarchical_group = paste(input_x_names, 
                               collapse = "+")
formula_for_brms  = paste("trait ~ 1 + ", 
                   non_hierarchical_group, 
                   sep = "")
# vsel_group <- paste(vsel, 
#                     collapse = "+")
# formula_for_brms  = paste("trait ~ 1 + ",
#                           vsel_group,
#                           sep = "")

# brms implementation -----------------------------------------------------

brms_normal <- brm(as.formula(formula_for_brms), 
                   data= data_train_for_analysis, 
                   family= gaussian(),
                   prior=c(prior(horseshoe(par_ratio = par_ratio1),class="b")
                           #prior(normal(0, 0.05),class="b")
                   ),
                   chains = 4, cores = 4,
                   #backend = "cmdstanr", 
                   #threads = threading(4), 
                   #init = init_list,
                   warmup = 2000, iter = 10000, # you might have to increase the iter and warmup if the chains dont mix well
                   #sample_prior = "only"
                   #save_pars = save_pars(all = TRUE),
                   #control = list(adapt_delta = 0.95, max_treedepth = 15)
)


dir.create("data/code_output_data", showWarnings = FALSE, recursive = TRUE)

saveRDS(brms_normal,  
        paste0("data/code_output_data/brms_object_",
               trait_name1, 
               "_",
               prediction_algorithm,
               "_",
               Sys.Date(),
               ".rds"))


