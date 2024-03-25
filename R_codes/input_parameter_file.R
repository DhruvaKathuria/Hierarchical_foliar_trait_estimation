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
scale_x = T
scale_y = T

date_vector = c("LMA" = "2023-12-26",
                "Nitrogen" = "2023-12-21",
                "Carotenoid_Area" = "2024-02-12")

nsel_vector <- c("LMA" = "30",
                 "Nitrogen" = "28" ,
                 "Carotenoid_Area" = "14")
