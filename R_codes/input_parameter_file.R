trait_name1 = "Carotenoid_Area"
site_name1 = c("cabo-2018-2019-leaf-level-spectra")
group_variable = "leaf_classification"
hierarchical = F
prediction_algorithm <- "supervised_pc" # options are "raw_spectra" which applies the 
                                        # algorithm to raw spectra, "supervised_pc" which
                                        # chooses the PCs based on the trait as input and
                                        # "naive_PC" which naively finds PCs and applies the
                                        # Bayesian algorithm to that
PLSR_implementation <- T # Set to True if you want to compare the results with PLSR