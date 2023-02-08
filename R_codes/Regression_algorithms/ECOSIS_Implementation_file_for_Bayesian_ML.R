library(dplyr)
library(arrow)
library(units)
library(ggplot2)

source("/Users/dhruvakathuria/Documents/GitHub/Hierarchical_foliar_trait_estimation/R_codes/Regression_algorithms/Apply_ML_and_prospect_algorithms.R")
#################################################functions used in this code#######################################
#x = datasets_to_take_for_trait[1]

filter_spectra_data = function(x) # this function only works for Carotenoid data for filtering the spectra # need to solve the Github issue
{
  spectra = readr :: read_csv(file = paste0(file.path(mainDir, x), "/", "spectra.csv"), show_col_types = FALSE)
  index_start = which(as.numeric(colnames(spectra)) == 400 )
  index_end = which(as.numeric(colnames(spectra)) == 2400)
  
  if(length(index_start)!= 0 & length(index_end)!=0)
  {
    if((index_end - index_start) == 2000)
    {
    spectra_filtered = spectra[, index_start:index_end]
    }else{spectra_filtered = NA}} else{spectra_filtered = NA}
  
  spectra_filtered |> filter()
  
}

this

filter_trait_data_and_metadata = function(x, trait_name1) # attaches the trait value and also the metadata associated with species as well as the instrument error
{
  metadata_arrow_version = read_parquet(paste0(file.path(mainDir, x), "/", "metadata_updated.parquet"))
  name_for_trait = attributes(metadata_arrow_version)$trait_names[[trait_name1]][["trait_name"]]
  units_for_trait = attributes(metadata_arrow_version)$trait_names[[trait_name1]][["units"]]
  
  names(metadata_arrow_version) = tolower(names(metadata_arrow_version))
  trait_value = as.numeric(unlist(metadata_arrow_version[name_for_trait]))
  
  if(!(trait_name1 %in% c("delta_C13", "delta_N15"))) # only delta_C13 and delta_N15 can have negative names (Source: figure in Phil Townsend paper)
  {
    trait_value[trait_value < 0] = NA
  }
  
  trait_value = trait_value * as_units(units_for_trait)
  
  if(trait_name1 == "Carotenoid_Area") # instead of the for loop, for each trait I need to make a list like the ones I made in "trait_and_sample_id_Database_for_ECOSIS_Data.R" which connects the trait_name and the chosen unit
  {
    trait_value = set_units(trait_value, microgram/cm^2)
  }
  
  if(trait_name1 == "LMA") # instead of the for loop, for each trait I need to make a list like the ones I made in "trait_and_sample_id_Database_for_ECOSIS_Data.R" which connects the trait_name and the chosen unit
  {
    trait_value = set_units(trait_value, gram/m^2)
  }
  
  metadata_merged = merge(metadata_arrow_version, database_for_metadata, by.x = "genus_species1" , by.y = "Scientific_name", all.x = TRUE)
  metadata_merged = metadata_merged %>% select(genus_species1, family1, Growth_form, Phenology, Leaf, manufacturer, model)
  metadata_merged$trait = trait_value
  metadata_merged$site_name = rep(x, nrow(metadata_merged))
  metadata_merged
}

get_indices_subset_function <- function(data_frame1, filtering_type, test_site, fraction_split) # get the indices for the training and testing data according to whether the split is to made globally or site-wise
{
  if(filtering_type == "Global")
  {
    indices_subset = sample(1:nrow(data_frame1), fraction_split * nrow(data_frame1))
  }else if(filtering_type == "Site_specific")
  {
    indices_subset = which(data_frame1$site_name != test_site)
  }
  indices_subset
}

get_test_data_frame_predictions = function(indices_subset, algorithm1) # gives the test dataframe along with the predictions based on algorithm1. The data frame also contains the metadata useful for further exploratory analysis
{
  Y_train = as.numeric(trait_and_metadata_dataframe$trait[indices_subset ])
  Y_test = as.numeric(trait_and_metadata_dataframe$trait[-indices_subset ])
  X_train = spectra_df[indices_subset, ]
  X_test = spectra_df[-indices_subset, ]
  Regression_apply2 = apply_regression_algorithm2 (algorithm1, X_train, Y_train, X_test, Y_test)
  data_mat_out = trait_and_metadata_dataframe[-indices_subset, ]
  data_mat_out$pred = Regression_apply2$predictions
  data_mat_out
}


###############################################Datasets########################################################################
Github_dir = "/Users/dhruvakathuria/Documents/GitHub/Hierarchical_foliar_trait_estimation/"
datasets_already_processed =  list.files(mainDir, recursive = T, pattern = "traits_already_done_for_metadata.txt")
database_for_metadata = readr :: read_csv(paste0(Github_dir, "R_codes/Species_data/Species_attribute_data_Dhruva.csv")) # this is the metadata file that I made from "get_growth_form_phenology_leaf_type_etc_from_Wiki.R"datasets_already_processed =  list.files(mainDir, recursive = T, pattern = "traits_already_done_for_metadata.txt") # we take the datasets for which atleast one of the traits has been processed
trait_name1 = "LMA"
indices_of_datasets_containing_trait_name =  unlist(lapply(datasets_already_processed, function(x)
{
  read_file1 = readr :: read_lines(file.path(mainDir, x))
  trait_name1 %in% read_file1
})
)

datasets_to_take_for_trait = unlist(lapply(datasets_already_processed[indices_of_datasets_containing_trait_name], function(x) strsplit(x, "/traits_already_done_for_metadata.txt" )[[1]])) 

spectra_filtered = lapply(datasets_to_take_for_trait, filter_spectra_data)
indices_spectra_to_take = which(sapply(spectra_filtered, length) == 2001)


trait_and_metadata_dataframe_list = lapply(datasets_to_take_for_trait, function(x){ return(tryCatch(filter_trait_data_and_metadata(x = x, trait_name1 =  "LMA"), error=function(e) NA)) })
indices_to_take_metadata = which(sapply(trait_and_metadata_dataframe_list, length) != 1)
indices_to_take = intersect(indices_spectra_to_take, indices_to_take_metadata)
## IMP: Need to have a check which confirms that the units of each list are the same

spectra_df = do.call(rbind, spectra_filtered[indices_to_take])
trait_and_metadata_dataframe = do.call(rbind, trait_and_metadata_dataframe_list[indices_to_take])
#################################################################################################################

## The below is just a quick fix (Just for Caretonoid) for a first pass at data. Will be replaced by a more formal approach for dealing with spectra
# fixing the spectra range based on seeing what is the extent of spectra variation in the data
# Uncomment the for loop if you want to see what colnames we have for spectra

# for(i in 1:length(datasets_to_take_for_trait))
# {
#   spectra = readr :: read_csv(file = paste0(file.path(mainDir, datasets_to_take_for_trait[i]), "/", "spectra.csv"), show_col_types = FALSE)
#   print(datasets_to_take_for_trait[i])
#   print(colnames(spectra)[1])
#   print(colnames(spectra)[ncol(spectra)])
#   print(ncol(spectra))
#   print(as.numeric(colnames(spectra)[ncol(spectra)]) - as.numeric(colnames(spectra)[1]))
#   print(which(is.na(spectra)))
# }



if(filtering_type == "Global")
{
  indices_subset = get_indices_subset_function(trait_and_metadata_dataframe, filtering_type = "Global", test_site = "None", fraction_split = 0.7)
  data_mat_test =   get_test_data_frame_predictions(indices_subset, algorithm1)
}else if (filtering_type == "Site_specific") 
{
  indices_subset = lapply(datasets_to_take_for_trait, get_indices_subset_function, filtering_type = "Site_specific", data_frame1 = trait_and_metadata_dataframe, fraction_split = 0.7)
  data_mat_test = lapply(indices_subset, get_test_data_frame_predictions, algorithm1 = algorithm1) 
  names(data_mat_test) = datasets_to_take_for_trait
}


