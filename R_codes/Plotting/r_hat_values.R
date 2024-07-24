library(brms)

# Setting global parameters -----------------------------------------------
source("R_codes/input_parameter_file.R")
#source("R_codes/Regression_algorithms/data_preprocessing_for_algorithms.R")



date_vector = c("LMA" = "2023-12-26",
                "Nitrogen" = "2023-12-21",
                "Carotenoid_Area" = "2024-02-12")

rhat_list = list()
for(trait_name1 in c("LMA", "Nitrogen", "Carotenoid_Area"))
{
#trait_name1 = "LMA"
  date_for_brms_file <- date_vector[trait_name1] #this is the date the brms file was saved
  
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
  
  
  rhat1 <- rhat(brms_normal)
  rhat_list[[trait_name1]] = rhat1
}
