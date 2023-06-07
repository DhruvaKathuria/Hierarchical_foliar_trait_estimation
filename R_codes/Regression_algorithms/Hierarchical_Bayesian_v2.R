#The reason I am writing this code is so that I can choose the Principal
#components to take for regression in my hierarchical model. I first do PCA on
#the entire dataset, split the data into train/test and apply cross-validation to
#determine the components. The advantage of using PCA is that it goes well with
#regression and we can also use Generalized Additive Models for our analysis to
#take into account the non-linear effects of the PCs on the output.

library(brms)
library(caret)
library(tidyverse)
library(ggpmisc)
library(glmnetUtils)
library(lme4)
library(mgcv)
library(modelr)
library(parallel)

# Function List Start -----------------------------------------------------

filter_out_error_groups <-  function(data_frame1)
{
  data_frame1 %>%
    filter(.data[[group_variable]] %in% filter_vector_list[[group_variable]]) %>%
    na.omit() %>%
    distinct()
}

get_optimal_components_for_spline = function(data_train,
                                             number_of_components_to_search,
                                             num_clusters)
{
  set.seed(123)
  cv  <<- crossv_kfold(data_train, k = 5)
  #RMSE_vector = vector(mode = "double", length = 30)
  
  cl <-
    makeCluster(num_clusters)     # set the number of processor cores
  clusterExport(cl, c("cv", "RMSE_function")) # export the objects to all cluster
  clusterEvalQ(cl, {
    library(modelr)
    library(mgcv)
  }) # run the following commands in all the clusters
  
  RMSE_out = parLapply(cl = cl,
                       1:number_of_components_to_search,
                       get_RMSE_for_when_we_have_i_components)
  stopCluster(cl)
  RMSE_out
}

get_PC_data_frame <-  function(data_frame1, spectra_names)
{
  PC_values_scaled <-  data_frame1 %>%
    select(all_of(spectra_names)) %>%
    prcomp(center = T, scale = T) %>%
    pluck("x") %>%
    scale1()
  
  data_frame_PC <-  data_frame1  %>%
    select(-all_of(spectra_names)) %>%
    mutate(trait = scale(trait)) %>%
    bind_cols(PC_values_scaled)
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

get_RMSE_for_when_we_have_i_components <- function(i)
{
  normal_group = paste(paste0("s(" , "PC", 1:i, ")") , collapse = "+")
  fla_mixed  = paste("trait ~ 1 + ", normal_group, sep = "")
  RMSE1 = vector(mode = "double", length = 5)
 
  for (j in 1:5)
  {
    data_train_cv = as.data.frame(cv$train[[j]])
    data_test_cv = as.data.frame(cv$test[[j]])
    model_gamm =   gamm(as.formula(fla_mixed), data = data_train_cv)
    predict1 = predict(model_gamm$gam, data_test_cv)
    RMSE1[j] = RMSE_function(predict1, data_test_cv$trait)
  }
  mean(RMSE1)
}

get_top_n_PC_names_for_a_trait_using_lasso <- 
  function(data_frame1, n_PC, type_of_validation = "normal_cv")
  {
    formula_PC <-
      paste("trait ~", paste(paste0("PC", 1:100), collapse = "+"))
    cv.out <-
      cv.glmnet(formula = as.formula(formula_PC) ,
                data = data_frame1,
                alpha = 0)
    abs_coefficient_values1 <-  cv.out %>%
      coef(s = 'lambda.min') %>%
      as.matrix() %>%
      abs() %>%
      as.data.frame() %>%
      rename("abs_coef" = "s1") %>%
      arrange(desc(abs_coef)) %>%
      rownames_to_column() %>%
      filter(rowname != "(Intercept)")
    PC_names_to_take <-  abs_coefficient_values$rowname[1:n_PC]
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

Prediction_function <- function(data_frame1, method1, Unique_groups)
    # method1 takes arguments "mixed" or "mixed_Bayesian
  {
    data_frame1_known_groups <-
      data_frame1 %>% filter(.data[[group_variable]] %in% Unique_groups)
    data_frame1_unknown_groups <-
      data_frame1 %>% filter(!(.data[[group_variable]] %in% Unique_groups))
    # if we only have known/unknown groups in the test data
    if (nrow(data_frame1) %in% c(nrow(data_frame1_known_groups),
                                 nrow(data_frame1_unknown_groups)) | method1 == "fixed")
    {
      Y_pred <-  prediction_function_list_known[[method1]](data_frame1)
      if (method1 == "mixed_Bayesian")
      {
        data_frame_out <-  data_frame1 %>%
          bind_cols(mixed_Bayesian_output(Y_pred))
      } else
      {
        data_frame_out <-  data_frame1 %>%
          mutate(!!(paste0("Pred_", method1)) := Y_pred)
      }
    } else
    {
      Y_pred <-
        bind_rows(
          prediction_function_list_known[[method1]](data_frame1_known_groups),
          prediction_function_list_unknown[[method1]](data_frame1_unknown_groups)
        )
      data_frame_comp <-  data_frame1_known_groups %>%
        bind_rows(data_frame1_unknown_groups) %>%
        mutate(!!(paste0("Pred_", method1)) := Y_pred)
      data_frame_out <-  data_frame1 %>%
        left_join(
          data_frame_comp,
          by = c("genus_species1", "trait", "site_name"),
          suffix = c("", ".y")
        ) %>%
        select(-ends_with(".y"))
    }
    data_frame_out
  }

scale1  <-  function(data_frame)
{
  matrix1 = as.matrix(data_frame)
  out = data.frame(scale(matrix1))
}

# Setting global parameters -----------------------------------------------

trait_name1 = "Nitrogen"
site_name1 = c("cabo-2018-2019-leaf-level-spectra")
group_variable = "leaf_classification"

# Getting data ready for analysis -----------------------------------------

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


#We are first formulating the data frame which will be used for analysis
sites_remove_vector <-
  c(
    "dried-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests",
    "fresh-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests",
    "ground-leaf-cabo-spectra-from-herbarium-project",
    "fresh-leaf-cabo-spectra-from-herbarium-project"
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

# PLSR implementation -----------------------------------------------------

data_frames_for_PLSR  <-
  get_PLSR_data_frames(data_frame_out, site_name1)
data_frame_with_PLSR_predictions <-
  PLSR_function(data_frames_for_PLSR[['data_train']],
                data_frames_for_PLSR[["data_test"]]) %>%
  select(-any_of(num_range("", 400:2400)))

# PC implementation -------------------------------------------------------

## We are doing the analysis on PC rather than on input covariates to not go 
## into the effects of correlated covariates
spectra_names <-  400:2400 %>% as.character()
data_frame_PC <-  data_frame_out %>%
  get_PC_data_frame(spectra_names = spectra_names)



#RMSE_matrix <-  matrix(NA, ncol = 4, nrow = length(site_names))
#cor_matrix <-  matrix(NA, ncol = 4, nrow = length(site_names))

# What I am trying to do here is that I am comparing the results if I only do the non-Bayesian PCR analysis, Mixed is when we also have a hierarchical model involved, mixed-fixed is when we use the fixed model for non-group data and mixed model for the data for which we have group information in the test data, bayesian mean is the RMSE/cor metrics when we do the Bayesian equivalant of the hierarchical approach and take the mean as the accuracy metric
#colnames(RMSE_matrix) = colnames(cor_matrix) = c("Fixed", "Mixed", "Mixed_Fixed", "Bayesian_mean")

# This for loop is for moving along various sites as I am doing a site transfer analysis
data_train  = data_frame_PC %>%
  filter(!(site_name %in% site_name1))

data_test = data_frame_PC %>%
  filter(site_name %in% site_name1)

# We first get the Principal component data matrix along with the trait value
data_train_for_hierarchical_analysis = data_train  %>%
  filter_out_error_groups()
data_test_for_hierarchical_analysis = data_test  %>%
  filter_out_error_groups()

#Give the cross-validation function here to choose the components to be taken for spline/linear analysis
rmse_values_for_different_PCS_for_spline_analysis <-
  unlist(
    get_optimal_components_for_spline(
      data_train_for_hierarchical_analysis,
      number_of_components_to_search = 40,
      num_clusters = 9
    )
  )
PCs_for_spline_analysis = paste(paste0(
  "s(" ,
  "PC",
  which.min(round(rmse_values_for_different_PCS_for_spline_analysis, 2)),
  ")"
) , collapse = "+")
# equation_formula_for_spline = paste("trait ~ 1 + ",
#                                     PCs_for_spline_analysis,
#                                     ", sigma ~ s(PC1)",
#                                     sep = "")
equation_formula_for_spline = paste("trait ~ 1 + ",
                                    PCs_for_spline_analysis,
                                    sep = "")

# PC_to_take_for_analysis = data_train_for_hierarchical_analysis %>%
#   get_top_n_PC_names_for_a_trait_using_lasso(n_PC = 30, type_of_validation = "normal_cv")
#PC_to_take_for_linear_analysis = paste0("PC", 1:100)
#fla_mixed = paste("trait ~ 1 + ", paste(PC_to_take_for_analysis, collapse = "+"), "+(", "PC2", paste("|", group_variable, ")", sep = ""), sep = "")
#fla_fixed  = paste("trait ~ 1 + ", paste(PC_to_take_for_analysis, collapse = "+"), sep = "")

# Setting up the models for analysis
#Linear Regression
# model_fixed = data_train_for_hierarchical_analysis %>%
#   lm(formula = as.formula(fla_fixed) ,
#            data = .) # Simple lm
#Mixed-effects model
# model_mixed = data_train_for_hierarchical_analysis %>%
#   lmer(formula = as.formula(fla_mixed) ,
#                    data = .) # lmer

# Mixed-effects/Hierarchical bayesian model
model_mixed_Bayesian =
  data_train_for_hierarchical_analysis %>%
  brm(
    formula = as.formula(equation_formula_for_spline),
    data = .,
    family = gaussian(),
    prior = c(
      set_prior("normal(0, 0.05)", class = "b"),
      set_prior("normal(0, 0.05)", class = "Intercept")
      #set_prior("lkj(2)", class = "cor")
    ),
    #prior = set_prior(horseshoe(df = 3, par_ratio = 0.1)),
    
    warmup = 10000,
    iter = 20000,
    chains = 3,
    cores = 3,
    control = list(adapt_delta = 0.99, max_treedepth = 20)
  ) # bayesian hierarchical modeling



# Predicting test data ----------------------------------------------------

Unique_groups = data_train_for_hierarchical_analysis %>%
  select(all_of(group_variable)) %>%
  unique() %>%
  pull(.data[[group_variable]])

prediction_function_list_known <-
  list(
    fixed = function(data_frame1)
      data_frame1 %>% predict(object = model_fixed) %>% data.frame(),
    mixed = function(data_frame1)
      data_frame1 %>% predict(object = model_mixed) %>% data.frame(),
    mixed_Bayesian = function(data_frame1)
      data_frame1 %>% predict(object = model_mixed_Bayesian) %>% data.frame()
  )

prediction_function_list_unknown  <-
  list(
    mixed = function(data_frame1)
      data_frame1 %>% predict(object = model_mixed, re.form = NA) %>% data.frame(),
    mixed_Bayesian = function(data_frame1)
      data_frame1 %>% predict(object = model_mixed_Bayesian, re_formula = NA) %>% data.frame()
  )

# Prediction function for fixed, mixed-effects/Bayesian_mixed effects model
data_frame_with_bayesian_prediction = Prediction_function(
  data_test_for_hierarchical_analysis,
  method1 = "mixed_Bayesian",
  Unique_groups = Unique_groups
) %>%
  select(-any_of(paste0("PC", 1:2001)))

#Combining the PLSR and Bayesian model
data_frame_with_predictions <-
  data_frame_with_bayesian_prediction %>%
  left_join(
    data_frame_with_PLSR_predictions,
    by = c("genus_species1", "trait", "site_name"),
    suffix = c("", ".y")
  ) %>%
  select(-ends_with(".y"))
data_frame_with_predictions <- data_frame_with_predictions %>%
  mutate(trait = trait[, 1])

folder_for_writing_data = "/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/NASA_proposal_3.1.2/GAM_ouput"
readr::write_csv(
  data_frame_with_predictions,
  paste0(
    folder_for_writing_data,
    "/Prediction_data_frame_for_",
    trait_name1,
    ".csv"
  )
)
saveRDS(
  model_mixed_Bayesian,
  paste0(
    folder_for_writing_data,
    "/Bayesian_brms_fit_for_",
    trait_name1,
    ".rds"
  )
)



# Making plots ------------------------------------------------------------
data_frame_with_predictions <- data_frame_with_predictions %>%
  pivot_longer(
    cols = c(Pred_mixed_Bayesian_mean, Prediction_PLSR),
    names_to = "Prediction_Algorithm",
    values_to = "Mean_Prediction"
  )

data_frame_with_predictions %>%
  ggplot(aes(x = Mean_Prediction, y = trait, color = Growth_form)) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = F) +
  facet_grid(Prediction_Algorithm ~ Growth_form) +
  geom_abline()

data_frame_with_predictions %>%
  filter(Prediction_Algorithm == "Pred_mixed_Bayesian_mean") %>%
  ggplot(aes(x = Mean_Prediction, y = trait, color = Growth_form)) +
  geom_point(alpha = 1) +
  geom_errorbar(
    aes(xmin = Pred_mixed_Bayesian_lower_2.5_quantile,
        xmax = Pred_mixed_Bayesian_upper_97.5_quantile),
    width = 0.2,
    alpha = 0.3
  ) +
  facet_wrap( ~ Growth_form) +
  geom_abline()

data_frame_with_predictions %>%
  group_by(Growth_form, Prediction_Algorithm) %>%
  summarize(RMSE1 = sqrt((mean((Mean_Prediction - trait) ^ 2
  ))),
  cor1 = cor(Mean_Prediction, trait))

RMSE_function(hh$trait, hh$Pred_mixed_Bayesian_mean)
cor_function(hh$trait, hh$Pred_mixed_Bayesian_mean)
plot(hh$Pred_mixed_Bayesian_mean, hh$trait)
abline(0, 1)


hh %>%
  filter(Growth_form %in% filter_vector_list[["Growth_form"]]) %>%
  ggplot() +
  geom_point(mapping = aes(
    x = trait,
    y = Pred_mixed_Bayesian$Estimate,
    color = Growth_form
  )) +
  geom_abline()







out_dir = "/Users/dhruvakathuria/Documents/GitHub/Hierarchical_foliar_trait_estimation/output_data_frame_predictions"
data_test_out = readr::write_csv(data_test_out,
                                 file  = paste0(out_dir, "/", site_names[i,], group_variable, ".csv"))

## Now lets compare the models
RMSE_function1 = function(data_frame1) {
  RMSE_function(data_frame1[, 1], data_frame1[, 2])
}
cor_function1 = function(data_frame1) {
  cor_function(data_frame1[, 1], data_frame1[, 2])
}

RMSE_fixed = data_test_out %>% select(trait, Pred_fixed_only) %>% RMSE_function1()
RMSE_mixed = data_test_out %>% select(trait, Pred_mixed_only) %>% RMSE_function1()
RMSE_mixed_fixed = data_test_out %>% select(trait, Pred_mixed_fixed) %>% RMSE_function1()
RMSE_mixed_Bayesian = data_test_out %>% select(trait, Pred_mixed_Bayesian) %>% RMSE_function1()


RMSE_matrix[i,] = c(RMSE_fixed, RMSE_mixed, RMSE_mixed_fixed, RMSE_mixed_Bayesian)

cor_fixed = data_test_out %>% select(trait, Pred_fixed_only) %>% cor_function1()
cor_mixed = data_test_out %>% select(trait, Pred_mixed_only) %>% cor_function1()
cor_mixed_fixed = data_test_out %>% select(trait, Pred_mixed_fixed) %>% cor_function1()
cor_matrix[i,] = c(cor_fixed, cor_mixed, cor_mixed_fixed, cor_mixed_Bayesian)
cor_mixed_Bayesian = data_test_out %>% select(trait, Pred_mixed_Bayesian) %>% cor_function1()

}
