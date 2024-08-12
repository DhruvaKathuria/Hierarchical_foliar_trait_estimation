library(brms)
library(ggplot2)
library(patchwork)
# posterior predictive check plots ----------------------------------------


#brms_Nitrogen <- readRDS("/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/Github_data/Hierarchical_foliar_trait_estimation/data/code_output_data/brms_object_Nitrogen_raw_spectra_2023-09-20.rds")
#brms_Car <- readRDS("/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/Github_data/Hierarchical_foliar_trait_estimation/data/code_output_data/brms_object_Carotenoid_Area_raw_spectra_2023-08-14.rds")
#brms_LMA <- readRDS("/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/Github_data/Hierarchical_foliar_trait_estimation/data/code_output_data/brms_object_LMA_raw_spectra_2023-09-12.rds")
#trait_name1 <- "Carotenoid_Area"


color_vector = c("Carotenoid_Area" =  "#F8766D80", 
                                 "LMA" =   "#00BA3880", 
                                 "Nitrogen" =  "#619CFF80")
trait_vector <- c("Carotenoid_Area",
                  "LMA",
                  "Nitrogen")

ppc_list = list()
j = 0
for(trait_name1 in trait_vector)
{
  source("R_codes/input_parameter_file.R")
  j = j +1
  brms_normal <- readRDS(paste0(data_folder,  
                                "/data/code_output_data/brms_full_model_files/brms_object_",
                                trait_name1, 
                                "_",
                                prediction_algorithm,
                                "_",
                                date_for_brms_file,
                                ".rds"))
  
  
  
  ppc_list[[j]] <- pp_check(brms_normal,
                      ndraws = 1000) +
    theme(legend.position = "none") + 
    scale_colour_manual(values = c("black", as.character(color_vector[trait_name1]) ))
}
  
(ppc_list[[1]] +  ppc_list[[2]]) / (ppc_list[[3]] + plot_spacer())
ppc_list[[1]] +  ppc_list[[2]] + ppc_list[[3]]

ggsave(filename = "paper_draft/figures/posterior_predictive_checks.png",
       width = 8,
       height = 3.5,
       units = "in")
