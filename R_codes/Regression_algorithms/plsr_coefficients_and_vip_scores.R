#implementing the PLSR regression coefficients plot and the VIP plot

##Regression coefficients

library(pls)
library(ggplot2)
library(tidyverse)

data_folder = "/Users/dkathuri/Downloads/Github_data/Hierarchical_foliar_trait_estimation"

# Data
trait_name1 = "LMA"
color_vector = c("Carotenoid_Area" =  "#F8766D", 
                 "LMA" =   "#00BA38", 
                 "Nitrogen" =  "#619CFF")
data_folder = "/Users/dkathuri/Downloads/Github_data/Hierarchical_foliar_trait_estimation"

trait_names_for_plots  = c("LMA" = "LMA",
                           "Carotenoid_Area" = "Carotenoid",
                           "Nitrogen" = "Nitrogen")
#pls file
get_plsr_regression_coefs_and_vip_scores = function(trait_name1)
  
{
  pls_object = readRDS(file.path(data_folder, "data", "code_output_data", str_glue("{trait_name1}_LOO_PLSR_object_March_2026.rds")))
  RMSE_values_cv = RMSEP(pls_object)
  RMSE_values = RMSE_values_cv$val[1, ,]
  index1_min = which.min(RMSE_values[1:100])
  
  #plotting plsr regression coefficients
  
  coef_vec <- as.vector(coef(pls_object, ncomp = index1_min))
  vip_scores = VIP(ll_1, opt.comp = index1_min)
  wavelengths = 400:2400
  
  plsr_df = data.frame(wavelengths = wavelengths,
                       coefficients = coef_vec,
                       vip = vip_scores,
                       trait = trait_names_for_plots[trait_name1]
                       )

}

plsr_out = lapply(c("Carotenoid_Area", "LMA", "Nitrogen" ),
                  get_plsr_regression_coefs_and_vip_scores)

plsr_out = data.table::rbindlist(plsr_out)

write_csv(plsr_out, file.path(data_folder, "data", "code_output_data", "plsr_files", "plsr_regression_coefs_plus_vip_scores.csv"))
