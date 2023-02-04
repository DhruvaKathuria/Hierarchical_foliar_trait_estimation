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

#######Put all the traits, and dataset names alphabetical wise
trait_vector_list = list("Carotenoid_Area" = Carotenoid_vector, "Carotenoid_Mass" = Carotenoid_vector, 
                         "LMA" = LMA_vector, "Carbon_mass" = Carbon_vector, "Nitrogen" = Nitrogen_vector)

exact_vector_list_for_covariate_first_pass_list = list("Carotenoid_Area" =  c("car", "car_area", "car_area (ug/cm2)", "carot_tot_area_l", "carotenoid ( g/cm )", "carotenoid (µg/cm²)", "carotenoid content ( g/cm )", "carotenoids"),
                                                       "LMA" = c("lma_gdw_m2", "lma g m2", "lma", "leaf mass per area", "lma_g_m2", "leaf mass per area (g/cm )", "lma_gm2", "leaf mass per area (g/cm²)",
                                                                 "leaf mass per area (mg/cm2)", "lma_gdw_m2", "leaf mass per area (mg/cm2)", "leaf mass per area (g/cm2)"))                         # this is after subsetting by eye the output of subset_trait_data_zeroeth_pass_function
exact_vector_list_for_covariate_second_pass_list = list("Carotenoid_Area" = c("car_area", "carotenoid content ( g/cm )", "carot_tot_area_l", "car_area (ug/cm2)"),
                                                        "LMA" = exact_vector_list_for_covariate_first_pass_list[["LMA"]])# this is subsetting by eye the output of above which has more than one matching dataset for the above. used choosing one item only for multi-item datasets. The second pass is imp for example for separating trait_area from trait_mass or chl_a from chl_tot

######################################Metadata related to dataset (trait independent)###############################################

# whether the data only has reflectance values or has values of transmittance/absorbance as well. In case, there are transmittance/absorbance values, we need to filter out only reflectance values. Make sure this index is T
is_spectral_measurement_reflectance_only_Boolean_list = list("cabo-2018-2019-leaf-level-spectra" = TRUE,
                                                             "fresh-leaf-spectra-to-estimate-foliar-functional-traits-across-neon-domains" = TRUE,
                                                             "angers-leaf-optical-properties-database--2003-" = TRUE,
                                                             "dessain-project-reflectance-spectra" = TRUE,
                                                             "leaf-optical-properties-experiment-database--lopex93-" = TRUE,
                                                             "leaf-reflectance-plant-functional-gradient-ifgg-kit" = TRUE,
                                                             "productivity-and-characterization-of-soybean-foliar-traits-under-aphid-pressure" = TRUE,
                                                             "2008-university-of-wisconsin-biotron-fresh-leaf-spectra-and-gas-exchange-leaf-traits" = TRUE,
                                                             "2014-cedar-creek-esr-grassland-biodiversity-experiment--leaf-level-contact-data--trait-predictions" = TRUE,
                                                             "2018-talladega-national-forest--leaf-level-reflectance-spectra-and-foliar-traits" = TRUE,
                                                             "common-milkweed-leaf-responses-to-water-stress-and-elevated-temperature" = TRUE,
                                                             "dried-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests" = TRUE,
                                                             "fresh-leaf-cabo-spectra-from-herbarium-project" = TRUE,
                                                             "fresh-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests" = TRUE,
                                                             "fresh-leaf-spectra-to-estimate-leaf-traits-for-california-ecosystems" = TRUE,
                                                             "fresh-leaf-spectra-to-estimate-lma-over-neon-domains-in-eastern-united-states" = TRUE,
                                                             "ground-leaf-cabo-spectra-from-herbarium-project" = TRUE,
                                                             "hyperspectral-leaf-reflectance--biochemistry--and-physiology-of-droughted-and-watered-crops" = TRUE) 


#################Sample id column name for each dataset, in case the data does not have it, I might have to create one
sample_id_list = list("cabo-2018-2019-leaf-level-spectra" = "sample_id",
                      "fresh-leaf-spectra-to-estimate-foliar-functional-traits-across-neon-domains" = "Sample_ID",
                      "angers-leaf-optical-properties-database--2003-" = "Refl_file",
                      "dessain-project-reflectance-spectra" = "sample_id",
                      "leaf-optical-properties-experiment-database--lopex93-" = "Refl_file",
                      "leaf-reflectance-plant-functional-gradient-ifgg-kit" = NA,
                      "productivity-and-characterization-of-soybean-foliar-traits-under-aphid-pressure" = "SAMP_ID",
                      "2008-university-of-wisconsin-biotron-fresh-leaf-spectra-and-gas-exchange-leaf-traits" = "Spectra",
                      "2014-cedar-creek-esr-grassland-biodiversity-experiment--leaf-level-contact-data--trait-predictions" = "guid",
                      "2018-talladega-national-forest--leaf-level-reflectance-spectra-and-foliar-traits" = "Sample_ID",
                      "common-milkweed-leaf-responses-to-water-stress-and-elevated-temperature" = "ID",
                      "dried-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests" = "UniqueID",
                      "fresh-leaf-cabo-spectra-from-herbarium-project" = "sample_name",
                      "fresh-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests" = "UniqueID",
                      "fresh-leaf-spectra-to-estimate-leaf-traits-for-california-ecosystems" = "spectra",
                      "fresh-leaf-spectra-to-estimate-lma-over-neon-domains-in-eastern-united-states" = "Sample_ID",
                      "ground-leaf-cabo-spectra-from-herbarium-project" = "sample_name",
                      "hyperspectral-leaf-reflectance--biochemistry--and-physiology-of-droughted-and-watered-crops" = "uniquefield")


############Link to the journal paper##############################
Paper_link_list = list("cabo-2018-2019-leaf-level-spectra" = c("https://ecoevorxiv.org/bfc5t/", "https://www.biorxiv.org/content/10.1101/2022.07.01.498461"),
                       "fresh-leaf-spectra-to-estimate-foliar-functional-traits-across-neon-domains" = NA,
                       "angers-leaf-optical-properties-database--2003-" = "http://www.sciencedirect.com.ezproxy.library.wisc.edu/science/article/pii/S0034425708000813",
                       "dessain-project-reflectance-spectra" = "https://www.biorxiv.org/content/10.1101/2022.07.01.498461v2",
                       "leaf-optical-properties-experiment-database--lopex93-" = c("http://teledetection.ipgp.jussieu.fr/opticleaf/lopex.htm", "http://opticleaf.ipgp.fr/index.php?page=database"),
                       "leaf-reflectance-plant-functional-gradient-ifgg-kit" = c("https://doi.org/10.1002/rse2.86", "https://doi.org/10.1038/s41598-019-43011-1"),
                       "productivity-and-characterization-of-soybean-foliar-traits-under-aphid-pressure" = NA,
                       "2008-university-of-wisconsin-biotron-fresh-leaf-spectra-and-gas-exchange-leaf-traits" = c("https://doi.org/10.1093/jxb/err294", "https://figshare.com/articles/Spectroscopic_determination_of_leaf_nutritional_morphological_and_metabolic_traits/745311"),
                       "2014-cedar-creek-esr-grassland-biodiversity-experiment--leaf-level-contact-data--trait-predictions" = NA,
                       "2018-talladega-national-forest--leaf-level-reflectance-spectra-and-foliar-traits" = NA,
                       "common-milkweed-leaf-responses-to-water-stress-and-elevated-temperature" = NA,
                       "dried-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests" = NA,
                       "fresh-leaf-cabo-spectra-from-herbarium-project" = c("https://www.biorxiv.org/content/10.1101/2021.04.21.440856v5.full", "https://github.com/ShanKothari/pressed-leaf-models"),
                       "fresh-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests" = "http://onlinelibrary.wiley.com/doi/10.1890/13-2110.1/abstract",
                       "fresh-leaf-spectra-to-estimate-leaf-traits-for-california-ecosystems" = NA,
                       "fresh-leaf-spectra-to-estimate-lma-over-neon-domains-in-eastern-united-states" = NA,
                       "ground-leaf-cabo-spectra-from-herbarium-project" = c("https://www.biorxiv.org/content/10.1101/2021.04.21.440856v5", "https://github.com/ShanKothari/pressed-leaf-models"),
                       "hyperspectral-leaf-reflectance--biochemistry--and-physiology-of-droughted-and-watered-crops" = "https://doi.org/10.1093/jxb/erab255")


###Spectral instrument info#################################
## whether the same spectral instrument was used for all measurements
is_spectral_instrument_used_globally_list = list("cabo-2018-2019-leaf-level-spectra" = T, 
                                                 "fresh-leaf-spectra-to-estimate-foliar-functional-traits-across-neon-domains" = T,
                                                 "angers-leaf-optical-properties-database--2003-" = T,
                                                 "dessain-project-reflectance-spectra" = T,
                                                 "leaf-optical-properties-experiment-database--lopex93-" = T,
                                                 "leaf-reflectance-plant-functional-gradient-ifgg-kit" = T,
                                                 "productivity-and-characterization-of-soybean-foliar-traits-under-aphid-pressure" = T,
                                                 "2008-university-of-wisconsin-biotron-fresh-leaf-spectra-and-gas-exchange-leaf-traits" = T,
                                                 "2014-cedar-creek-esr-grassland-biodiversity-experiment--leaf-level-contact-data--trait-predictions" = T,
                                                 "2018-talladega-national-forest--leaf-level-reflectance-spectra-and-foliar-traits" = T,
                                                 "common-milkweed-leaf-responses-to-water-stress-and-elevated-temperature" = T,
                                                 "dried-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests" = T,
                                                 "fresh-leaf-cabo-spectra-from-herbarium-project" = T,
                                                 "fresh-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests" = T,
                                                 "fresh-leaf-spectra-to-estimate-leaf-traits-for-california-ecosystems" = T,
                                                 "fresh-leaf-spectra-to-estimate-lma-over-neon-domains-in-eastern-united-states" = F,
                                                 "ground-leaf-cabo-spectra-from-herbarium-project" = T,
                                                 "hyperspectral-leaf-reflectance--biochemistry--and-physiology-of-droughted-and-watered-crops" = T)

###if *is_spectral_instrument_used_globally_list* is F, add the manufacturer and model info in the meadata itself, eg look at /Users/dhruvakathuria/Documents/GitHub/Hierarchical_foliar_trait_estimation/R_codes/Getting_and_cleaning_ECOSIS_data/Further_data_cleaning/fresh-leaf-spectra-to-estimate-lma-over-neon-domains-in-eastern-united-states.R
###if *is_spectral_instrument_used_globally_list* is T, then fill in below
instrument_manufacturer_info_global_list = list("cabo-2018-2019-leaf-level-spectra" = "spectra vista corporation",
                                                "fresh-leaf-spectra-to-estimate-foliar-functional-traits-across-neon-domains" = "spectral evolution",
                                                "angers-leaf-optical-properties-database--2003-" = "asd",
                                                "dessain-project-reflectance-spectra" = "analytical spectral devices",
                                                "leaf-optical-properties-experiment-database--lopex93-" = "perkin elmer",
                                                "leaf-reflectance-plant-functional-gradient-ifgg-kit" = "malvern panalytical",
                                                "productivity-and-characterization-of-soybean-foliar-traits-under-aphid-pressure" = "analytical spectral devices",
                                                "2008-university-of-wisconsin-biotron-fresh-leaf-spectra-and-gas-exchange-leaf-traits" = "analytical spectral devices",
                                                "2014-cedar-creek-esr-grassland-biodiversity-experiment--leaf-level-contact-data--trait-predictions" = "analytical spectral devices",
                                                "2018-talladega-national-forest--leaf-level-reflectance-spectra-and-foliar-traits" = "spectra vista corporation",
                                                "common-milkweed-leaf-responses-to-water-stress-and-elevated-temperature" = "analytical spectral devices",
                                                "dried-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests" = "analytical spectral devices",
                                                "fresh-leaf-cabo-spectra-from-herbarium-project" = "spectra vista corporation",
                                                "fresh-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests" = "analytical spectral devices",
                                                "fresh-leaf-spectra-to-estimate-leaf-traits-for-california-ecosystems" = "analytical spectral devices",
                                                "ground-leaf-cabo-spectra-from-herbarium-project" = "spectral evolution",
                                                "hyperspectral-leaf-reflectance--biochemistry--and-physiology-of-droughted-and-watered-crops" = "spectral evolution")

instrument_model_info_global_list = list("cabo-2018-2019-leaf-level-spectra" = "hr-1024i",
                                         "fresh-leaf-spectra-to-estimate-foliar-functional-traits-across-neon-domains" = "psr-3500",
                                         "angers-leaf-optical-properties-database--2003-" = "fieldspec",
                                         "dessain-project-reflectance-spectra" = "field spec 4",
                                         "leaf-optical-properties-experiment-database--lopex93-" = "lambda-19 spectrophotometer",
                                         "leaf-reflectance-plant-functional-gradient-ifgg-kit" = "asd fieldspec 3",
                                         "productivity-and-characterization-of-soybean-foliar-traits-under-aphid-pressure" = "fieldspec4",
                                         "2008-university-of-wisconsin-biotron-fresh-leaf-spectra-and-gas-exchange-leaf-traits" = "asd fieldspec 3",
                                         "2014-cedar-creek-esr-grassland-biodiversity-experiment--leaf-level-contact-data--trait-predictions" = "asd fieldspec 3",
                                         "2018-talladega-national-forest--leaf-level-reflectance-spectra-and-foliar-traits" = "svc hr-1024i spectroradiometer with an attached lc-rp-pro leaf clip foreoptic",
                                         "common-milkweed-leaf-responses-to-water-stress-and-elevated-temperature" = "fieldspec 3",
                                         "dried-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests" = "fieldspec 3",
                                         "fresh-leaf-cabo-spectra-from-herbarium-project" = "hr-1024i",
                                         "fresh-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests" = "fieldspec 3",
                                         "fresh-leaf-spectra-to-estimate-leaf-traits-for-california-ecosystems" = "fieldspec 3",
                                         "ground-leaf-cabo-spectra-from-herbarium-project" = "psr+ 3500",
                                         "hyperspectral-leaf-reflectance--biochemistry--and-physiology-of-droughted-and-watered-crops" = "psr+" )
                                         



#################################################################




############################################################################################################################