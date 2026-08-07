
{
  library(brms)
  library(caret)
  library(tidyverse)
  library(ggpmisc)
  library(glmnetUtils)
  library(lme4)
  library(mgcv)
  library(modelr)
  library(parallel)
  library(janitor)
  }

# Function List Start -----------------------------------------------------

filter_out_error_groups <-  function(data_frame1)
{
  data_frame1 %>%
    filter(.data[[group_variable]] %in% filter_vector_list[[group_variable]]) %>%
    na.omit() %>%
    distinct()
}


get_PLSR_data_frames = function(data_frame_out, site_name1)
{
  data_frame_out1 = data_frame_out %>% mutate(trait = scale(trait))
  
  data_train_PLSR  = data_frame_out1 %>%
    filter(!(site_name %in% site_name1))
  data_test_PLSR = data_frame_out1 %>%
    filter(site_name %in% site_name1)
  
  data_train_for_hierarchical_analysis_PLSR = data_train_PLSR  %>%
    filter_out_error_groups() %>% select(-c(genus_species1:model, site_name))
  data_test_for_hierarchical_analysis_PLSR = data_test_PLSR  %>%
    filter_out_error_groups()
  
  list(data_train = data_train_for_hierarchical_analysis_PLSR,
       data_test = data_test_for_hierarchical_analysis_PLSR)
}

get_PLSR_data_frames_without_trait_scale = function(data_frame_out, site_name1)
{
  #data_frame_out1 = data_frame_out %>% mutate(trait = scale(trait))
  
  data_train_PLSR  = data_frame_out %>%
    filter(!(site_name %in% site_name1))
  data_test_PLSR = data_frame_out %>%
    filter(site_name %in% site_name1)
  
  data_train_for_hierarchical_analysis_PLSR = data_train_PLSR  %>%
    filter_out_error_groups()# %>% select(-c(genus_species1:model, site_name))
  data_test_for_hierarchical_analysis_PLSR = data_test_PLSR  %>%
    filter_out_error_groups()
  
  list(data_train = data_train_for_hierarchical_analysis_PLSR,
       data_test = data_test_for_hierarchical_analysis_PLSR)
}


mixed_Bayesian_output <-  function(Y_pred)
{
  Y_pred_lower_2.5_quantile <- Y_pred[, 3]
  Y_pred_upper_97.5_quantile <- Y_pred[, 4]
  Y_pred_mean <-  Y_pred[, 1]
  pred_data_frame <-
    data.frame(
      "Pred_mixed_Bayesian_lower_2.5_quantile" = Y_pred_lower_2.5_quantile,
      "Pred_mixed_Bayesian_upper_97.5_quantile" = Y_pred_upper_97.5_quantile,
      "Pred_mixed_Bayesian_mean" = Y_pred_mean
    )
}

PLSR_function = function(data_train, data_test)
{
  data_train = data_train |>
    select(trait, num_range("", 400:2400))
  library(pls)
  set.seed(123)
  ll_1 = plsr(
    trait ~ .,
    data = data_train,
    validation = "CV",
    scale = T,
    center = T,
    segments = 5
  )
  # ll_1 = plsr(
  #   trait ~ .,
  #   data = data_train,
  #   validation = "LOO",
  #   scale = T,
  #   center = T
  # )
  #summary(ll_1)
  RMSE_values_cv = RMSEP(ll_1)
  RMSE_values = RMSE_values_cv$val[1, ,]
  index1_min = which.min(RMSE_values[1:100])
  # validationplot(ll_1, val.type = "MSEP")
  
  data_test1 = data_test |>
    select(trait, num_range("", 400:2400))
  
  pred1 <- predict(ll_1, data_test1, ncomp = index1_min)
  pred1 = pred1[, 1, 1]
  
  data_test_out = data_test |>
    mutate(Prediction_PLSR = pred1,
           .after = trait)
}

PLSR_function_without_trait_scale = function(data_train, data_test)
{
  mean_data_train <- mean(data_train$trait, na.rm = T)
  sd_data_train <- sd(data_train$trait, na.rm = T)
  
  data_train = data_train |>
    select(trait, num_range("", 400:2400))
  
  library(pls)
  set.seed(123)
  ll_1 = plsr(
    trait ~ .,
    data = data_train,
    validation = "CV",
    scale = T,
    center = T,
    segments = 5
  )
  #summary(ll_1)
  RMSE_values_cv = RMSEP(ll_1)
  RMSE_values = RMSE_values_cv$val[1, ,]
  index1_min = which.min(RMSE_values[1:100])
  # validationplot(ll_1, val.type = "MSEP")
  
  data_test1 = data_test |>
    select(trait, num_range("", 400:2400))
  
  pred1 <- predict(ll_1, data_test1, ncomp = index1_min)
  pred1 = pred1[, 1, 1]
  
  data_test_out = data_test |>
    mutate(Prediction_PLSR = pred1,
           .after = trait)
}


scale1  <-  function(data_frame)
{
  matrix1 = as.matrix(data_frame)
  out = data.frame(scale(matrix1))
}

get_scaled_inputs <- function(data_train_for_hierarchical_analysis, 
                              data_test_for_hierarchical_analysis, 
                              scale_x, 
                              scale_y)
{

  x_train <- data_train_for_hierarchical_analysis |> 
    select(num_range("x",400:2400)) 
  y_train <- data_train_for_hierarchical_analysis$trait
  
  x_test <- data_test_for_hierarchical_analysis |> 
    select(num_range("x",400:2400))
  y_test <- data_test_for_hierarchical_analysis$trait
  
  if(scale_x == T)
  {
    x_train_colmeans <- colMeans(x_train, na.rm = T)
    x_train_sd <- apply(x_train, 2, function(x) sd(x, na.rm = T))
    
    x_train <- scale(x_train) |> data.frame()
    
    for(i in 1:ncol(x_train))
    {
      x_test[ ,i] <- (x_test[ ,i] - x_train_colmeans[i])/x_train_sd[i]
    }
    
  }
  
  if(scale_y == T)
  {
    y_colmean <- mean(y_train, na.rm = T)
    y_train_sd <- sd(y_train, na.rm = T)
    
    y_train <- scale(y_train)
    y_test <- (y_test - y_colmean)/y_train_sd
  }
  
  list(x_train = x_train,
       y_train = y_train,
       x_test = x_test, 
       y_test = y_test)
}

# Getting data ready for analysis -----------------------------------------

# PLSR implementation
PLSR_filepath <-  paste0("data/code_output_data/PLSR_object_",
                         trait_name1, 
                         ".rds")
if(file.exists(PLSR_filepath))
{
  data_frame_with_PLSR_predictions <- readRDS(paste0("data/code_output_data/PLSR_object_",
                                                     trait_name1, 
                                                     ".rds"))
  PLSR_implementation = F
}

#Source the file below to get the input data matrices and the output trait
source(
  "R_codes/Regression_algorithms/ECOSIS_Implementation_file_for_Bayesian_ML.R"
)

#The below list is formed to do form groups for the hierarchical type analysis
filter_vector_list <-
  list(
    "Growth_form" = c("tree", "shrub", "herbaceous", "grass", "vine"),
    "Leaf" = c("broad", "needle"),
    "Phenology" = c("deciduous", "evergreen"),
    "genus_species1" = unique(trait_and_metadata_dataframe$genus_species1[!(trait_and_metadata_dataframe$genus_species1 %in% "NA NA")]),
    "family1" = unique(trait_and_metadata_dataframe$family1[!is.na(trait_and_metadata_dataframe$family1)]),
    "leaf_classification" = c("broadleaf",  "needle", "grass", "herbaceous")
  )


#We are first formulating the data frame which will be used for analysis. These files are removed because they have the same observations as the other datasets
sites_remove_vector <-
  c(
    "dried-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests",
    "fresh-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests",
    "ground-leaf-cabo-spectra-from-herbarium-project",
    "fresh-leaf-cabo-spectra-from-herbarium-project",
    "dessain-project-reflectance-spectra",
    "productivity-and-characterization-of-soybean-foliar-traits-under-aphid-pressure"
  )

check_for_distinct_input_data_using_spectra <-
  seq(from = 400, to = 1000) %>%
  as.character()

data_frame_out <-  trait_and_metadata_dataframe %>%
  bind_cols(spectra_df) %>%
  dplyr::filter(!(site_name %in% sites_remove_vector)) %>%
  dplyr::filter(!is.na(trait)) %>%
  dplyr::distinct(trait, site_name, .keep_all = TRUE) %>%
  dplyr::distinct(trait, across(all_of(
    check_for_distinct_input_data_using_spectra
  )), .keep_all = TRUE) %>%
  mutate(trait = as.numeric(trait))

## First I do the subsetting via study sites
site_names <-  data_frame_out %>%
  select(site_name) %>%
  unique() %>%
  pull(site_name)

spectra_names <-  400:2400 %>% as.character()
# PLSR implementation -----------------------------------------------------

#data_frame_out <- data_frame_out |> filter(site_name != "dessain-project-reflectance-spectra")
data_frames_for_PLSR  <-
  get_PLSR_data_frames_without_trait_scale(data_frame_out, site_name1)

if(PLSR_implementation == T)
{
  data_frame_with_PLSR_predictions <-
    PLSR_function(data_frames_for_PLSR[['data_train']],
                  data_frames_for_PLSR[["data_test"]]) %>%
    select(-any_of(num_range("", 400:2400)))
}


if(prediction_algorithm %in% c("supervised_pc", "raw_spectra"))
{
  data_train <- data_frames_for_PLSR[["data_train"]]
  data_test <- data_frames_for_PLSR[["data_test"]]
}

data_train_for_hierarchical_analysis = data_train  %>%
  filter_out_error_groups() |> 
  clean_names()
data_test_for_hierarchical_analysis = data_test  %>%
  filter_out_error_groups() |> 
  clean_names()


if(PLSR_implementation == T)
{
  saveRDS(data_frame_with_PLSR_predictions,  
          paste0("data/code_output_data/PLSR_object_",
                 trait_name1, 
                 ".rds"))
}


# getting data ready for the chosen algorithm -----------------------------

data_scaled <- get_scaled_inputs(data_train_for_hierarchical_analysis, 
                                 data_test_for_hierarchical_analysis,
                                 scale_x,
                                 scale_y)

x_train <- data_scaled$x_train; y_train = data_scaled$y_train
x_test <- data_scaled$x_test; y_test = data_scaled$y_test

if(prediction_algorithm == "raw_spectra")
{
  data_train_for_analysis <- data_train_for_hierarchical_analysis |> 
    select(-starts_with("x")) |> 
    bind_cols(x_train) |> 
    mutate(trait = y_train)
  
  data_test_for_analysis <- data_test_for_hierarchical_analysis |> 
    select(-starts_with("x")) |> 
    bind_cols(x_test) |> 
    mutate(trait = y_test)

  par_ratio1 = 0.025
}