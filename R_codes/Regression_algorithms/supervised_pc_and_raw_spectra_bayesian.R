library(dimreduce)

# Setting global parameters -----------------------------------------------

trait_name1 = "Carotenoid_Area"
site_name1 = c("cabo-2018-2019-leaf-level-spectra")
group_variable = "leaf_classification"
hierarchical = F
prediction_algorithm <- "raw_spectra" # options are "raw_spectra" which applies the 
                                        # algorithm to raw spectra, "supervised_pc" which
                                        # chooses the PCs based on the trait as input and
                                        # "naive_PC" which naively finds PCs and applies the
                                        # Bayesian algorithm to that
PLSR_implementation <- F # Set to True if you want to compare the results with PLSR


# source the data preprocessing code --------------------------------------

source("R_codes/Regression_algorithms/data_preprocessing_for_algorithms.R")

if(prediction_algorithm == "supervised_pc")
{
  x_train <- data_train_for_hierarchical_analysis |> 
    select(num_range("x",400:2400)) |> 
    mutate(intercept = 1)
  y_train <- data_train_for_hierarchical_analysis$trait
  
  x_test <- data_test_for_hierarchical_analysis |> 
    select(num_range("x",400:2400))|> 
    mutate(intercept = 1)
  y_test <- data_test_for_hierarchical_analysis$trait
  
  # Comment: I found that the iterative PC of Vehtari group https://arxiv.org/abs/1710.06229 
  # was better than the original https://hastie.su.domains/Papers/spca_JASA.pdf
  get_super_pcs <- ispca(x_train,
                         y_train, 
                         nctot=50) # ispca; nctot is the total PCs
                                              # I arbitrarily set it to 50
                                              # the ispca will first set the 
                                              # supervised PCs and then take the
                                              # rest as naive PCs
  
  x_super_pc_train <- predict(get_super_pcs, 
                              x_train) |> 
    data.frame() |> 
    clean_names()
  x_super_pc_test <- predict(get_super_pcs, 
                             x_test)|> 
    data.frame() |> 
    clean_names()
  
  #remove the input spectra and replace by super+ normal pcs
  data_train_for_analysis <- data_train_for_hierarchical_analysis |> 
    select(-starts_with("x")) |> bind_cols(x_super_pc_train)
  data_test_for_analysis <- data_test_for_hierarchical_analysis |> 
    select(-starts_with("x")) |> bind_cols(x_super_pc_test)
  
}

if(prediction_algorithm == "raw_spectra")
{
  data_train_for_analysis <- data_train_for_hierarchical_analysis
  data_test_for_analysis <- data_test_for_hierarchical_analysis
}
  
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
                   prior=c(prior(normal(20,3),class="Intercept"),
                           prior(horseshoe(df = 1, par_ratio = 0.05),class="b")
                           #prior(normal(0, 0.05),class="b")
                   ),
                   chains = 2, cores = 2,
                   backend = "cmdstanr", 
                   threads = threading(4), 
                   warmup = 10000, iter = 20000, 
                   #sample_prior = "only"
                   #save_pars = save_pars(all = TRUE),
                   control = list(adapt_delta = 0.95, max_treedepth = 15)
)

saveRDS(brms_normal,  
        paste0("data/code_output_data/brms_object_",
               trait_name1, 
               "_",
               prediction_algorithm,
               "_",
               Sys.Date(),
               ".rds"))


