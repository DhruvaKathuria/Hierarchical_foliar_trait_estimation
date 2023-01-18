#####################################################################################
###For now I am making a database of the commonly used trait names in the datasets used in ECOSIS
LMA_vector = c("lma", "leaf_mass_per_area", "leaf mass per area")
Chl_vector = c("chlorophyll", "chl", "chl_ab", "chlab") # work on how to separate chla and chlb from chltot
Chla_vector = c("chla", "chl_a", "chlb", "chl_b", "chlorophyll_a", "chlorophyll_b", "chlorophylla", "chlorophyllb")
Carbon_vector = c("c", "carbon", "c_")
Calcium_vector = c("Ca", "calcium")
Nitrogen_vector = c("nitrogen", "n", "n%")
Carbon_Nitrogen = "c:n"
Cellulose_vector = c("cellulose")
Lignin_vector = "lignin"
Carotenoid_vector = c("car", "carotenoid")
Usda_symbol = c("usda", "usda symbol")
Genus_species

#######Put all the traits, and dataset names alphabetical wise
trait_vector_list = list("Carotenoid_Area" = Carotenoid_vector, "Carotenoid_Mass" = Carotenoid_vector, 
                         "LMA" = LMA_vector)

exact_vector_list_for_covariate_first_pass_list = list("Carotenoid_Area" =  c("car", "car_area", "car_area (ug/cm2)", "carot_tot_area_l", "carotenoid ( g/cm )", "carotenoid (µg/cm²)", "carotenoid content ( g/cm )", "carotenoids")
                                                       )                         # this is after subsetting by eye the output of subset_trait_data_zeroeth_pass_function
exact_vector_list_for_covariate_second_pass_list = list("Carotenoid_Area" = c("car_area", "carotenoid content ( g/cm )", "carot_tot_area_l", "car_area (ug/cm2)")
                                                        )# this is subsetting by eye the output of above which has more than one matching dataset for the above. The second pass is imp for example for separating trait_area from trait_mass or chl_a from chl_tot

######################################Metadata related to dataset (trait independent)###############################################

# whether the data only has reflectance values or has values of transmittance/absorbance as well. In case, there are transmittance/absorbance values, we need to filter out only reflectance values. Make sure this index is T
is_spectral_measurement_reflectance_only_Boolean_list = list("cabo-2018-2019-leaf-level-spectra" = TRUE,
                                                             "fresh-leaf-spectra-to-estimate-foliar-functional-traits-across-neon-domains" = TRUE,
                                                             "angers-leaf-optical-properties-database--2003-" = TRUE,
                                                             "dessain-project-reflectance-spectra" = TRUE,
                                                             "leaf-optical-properties-experiment-database--lopex93-" = TRUE,
                                                             "leaf-reflectance-plant-functional-gradient-ifgg-kit" = TRUE,
                                                             "productivity-and-characterization-of-soybean-foliar-traits-under-aphid-pressure" = TRUE) 


#################Sample id column name for each dataset, in case the data does not have it, I might have to create one
sample_id_list = list("cabo-2018-2019-leaf-level-spectra" = "sample_id",
                      "fresh-leaf-spectra-to-estimate-foliar-functional-traits-across-neon-domains" = "Sample_ID",
                      "angers-leaf-optical-properties-database--2003-" = "Refl_file",
                      "dessain-project-reflectance-spectra" = "sample_id",
                      "leaf-optical-properties-experiment-database--lopex93-" = "Refl_file",
                      "leaf-reflectance-plant-functional-gradient-ifgg-kit" = NA,
                      "productivity-and-characterization-of-soybean-foliar-traits-under-aphid-pressure" = "SAMP_ID")


############Link to the journal paper##############################
Paper_link_list = list("cabo-2018-2019-leaf-level-spectra" = c("https://ecoevorxiv.org/bfc5t/", "https://www.biorxiv.org/content/10.1101/2022.07.01.498461"),
                       "fresh-leaf-spectra-to-estimate-foliar-functional-traits-across-neon-domains" = NA,
                       "angers-leaf-optical-properties-database--2003-" = "http://www.sciencedirect.com.ezproxy.library.wisc.edu/science/article/pii/S0034425708000813",
                       "dessain-project-reflectance-spectra" = "https://www.biorxiv.org/content/10.1101/2022.07.01.498461v2",
                       "leaf-optical-properties-experiment-database--lopex93-" = c("http://teledetection.ipgp.jussieu.fr/opticleaf/lopex.htm", "http://opticleaf.ipgp.fr/index.php?page=database"),
                       "leaf-reflectance-plant-functional-gradient-ifgg-kit" = c("https://doi.org/10.1002/rse2.86", "https://doi.org/10.1038/s41598-019-43011-1"),
                       "productivity-and-characterization-of-soybean-foliar-traits-under-aphid-pressure" = NA)


###Spectral instrument info#################################
## whether the same spectral instrument was used for all measurements
is_spectral_instrument_used_globally_list = list("cabo-2018-2019-leaf-level-spectra" = T, 
                                                 "fresh-leaf-spectra-to-estimate-foliar-functional-traits-across-neon-domains" = T,
                                                 "angers-leaf-optical-properties-database--2003-" = T,
                                                 "dessain-project-reflectance-spectra" = T,
                                                 "leaf-optical-properties-experiment-database--lopex93-" = T,
                                                 "leaf-reflectance-plant-functional-gradient-ifgg-kit" = T,
                                                 "productivity-and-characterization-of-soybean-foliar-traits-under-aphid-pressure" = T)

###if *is_spectral_instrument_used_globally_list* is T, then fill in below
instrument_manufacturer_info_global_list = list("cabo-2018-2019-leaf-level-spectra" = "spectra vista corporation",
                                                "fresh-leaf-spectra-to-estimate-foliar-functional-traits-across-neon-domains" = "spectral evolution",
                                                "angers-leaf-optical-properties-database--2003-" = "asd",
                                                "dessain-project-reflectance-spectra" = "analytical spectral devices",
                                                "leaf-optical-properties-experiment-database--lopex93-" = "perkin elmer",
                                                "leaf-reflectance-plant-functional-gradient-ifgg-kit" = "malvern panalytical",
                                                "productivity-and-characterization-of-soybean-foliar-traits-under-aphid-pressure" = "analytical spectral devices")

instrument_model_info_global_list = list("cabo-2018-2019-leaf-level-spectra" = "hr-1024i",
                                         "fresh-leaf-spectra-to-estimate-foliar-functional-traits-across-neon-domains" = "psr-3500",
                                         "angers-leaf-optical-properties-database--2003-" = "fieldspec",
                                         "dessain-project-reflectance-spectra" = "field spec 4",
                                         "leaf-optical-properties-experiment-database--lopex93-" = "lambda-19 spectrophotometer",
                                         "leaf-reflectance-plant-functional-gradient-ifgg-kit" = "asd fieldspec 3",
                                         "productivity-and-characterization-of-soybean-foliar-traits-under-aphid-pressure" = "fieldspec4")

###if *is_spectral_instrument_used_globally_list* is F, then fill in below
instrument_manufacturer_info_local_list = list()
instrument_model_info_local_list = list( )
#################################################################




############################################################################################################################