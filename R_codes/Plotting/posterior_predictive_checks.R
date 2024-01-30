library(brms)
library(ggplot2)
library(patchwork)
# posterior predictive check plots ----------------------------------------

trait_name1 <- "Carotenoid_Area"
brms_Nitrogen <- readRDS("/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/Github_data/Hierarchical_foliar_trait_estimation/data/code_output_data/brms_object_Nitrogen_raw_spectra_2023-09-20.rds")
brms_Car <- readRDS("/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/Github_data/Hierarchical_foliar_trait_estimation/data/code_output_data/brms_object_Carotenoid_Area_raw_spectra_2023-08-14.rds")
brms_LMA <- readRDS("/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/Github_data/Hierarchical_foliar_trait_estimation/data/code_output_data/brms_object_LMA_raw_spectra_2023-09-12.rds")

ppc_Car <- pp_check(brms_Car,
                    ndraws = 1000) +
  theme(legend.position = "none") + 
  ggtitle("Carotenoid") 

ppc_LMA <- pp_check(brms_LMA,
                    ndraws  = 1000) +
  theme(legend.position = "none") +
  ggtitle("LMA")

ppc_Nitrogen <- pp_check(brms_Nitrogen,
                         ndraws  = 1000)  +
  ggtitle("Nitrogen")
  
(ppc_Car +  ppc_LMA) / (ppc_Nitrogen + plot_spacer())

ggsave(filename = "posterior predictive checks.png",
       width = 8,
       height = 9,
       units = "in")
