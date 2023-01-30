library(dplyr)
library(arrow)
library(units)

mainDir = "/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/NASA_proposal_3.1.2/ECOSIS_Data_download_Dhruva"  
Github_dir = "/Users/dhruvakathuria/Documents/GitHub/Hierarchical_foliar_trait_estimation/"

trait_name1 = "Carotenoid_Area"
datasets_already_processed =  list.files(mainDir, recursive = T, pattern = "traits_already_done_for_metadata.txt")
indices_of_datasets_containing_trait_name =  unlist(lapply(datasets_already_processed, function(x)
{
  read_file1 = readr :: read_lines(file.path(mainDir, x))
  trait_name1 %in% read_file1
})
)

datasets_to_take_for_trait = unlist(lapply(datasets_already_processed[indices_of_datasets_containing_trait_name], function(x) strsplit(x, "/traits_already_done_for_metadata.txt" )[[1]])) 

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


filter_spectra_data = function(x)
{
  spectra = readr :: read_csv(file = paste0(file.path(mainDir, x), "/", "spectra.csv"), show_col_types = FALSE)
  index_start = which(as.numeric(colnames(spectra)) == 400 )
  index_end = which(as.numeric(colnames(spectra)) == 2400)
  
  spectra_filtered = spectra[, index_start:index_end]

}

spectra_filtered = lapply(datasets_to_take_for_trait, filter_spectra_data)

# Putting all the spectra_filtered list into a matrix form
for(i in 1:length(spectra_filtered))
{
  if(i == 1)
  {
    spectra_matrix = spectra_filtered[[i]]
  }else{spectra_matrix = rbind(spectra_matrix, spectra_filtered[[i]])}
}

#################################################################################################################

##### Getting traits together


filter_trait_data_and_metadata = function(x, trait_name1)
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
  
  metadata_merged = merge(metadata_arrow_version, input_database, by.x = "genus_species1" , by.y = "Scientific_name", all.x = TRUE)
  metadata_merged = metadata_merged %>% select(genus_species1, family1, Growth_form, Phenology, Leaf, manufacturer, model)
  metadata_merged$trait = trait_value
  metadata_merged$site_name = rep(x, nrow(metadata_merged))
  metadata_merged
}

trait_and_metadata_dataframe_list = lapply(datasets_to_take_for_trait, filter_trait_data_and_metadata, trait_name1 =  "Carotenoid_Area")
## IMP: Need to have a check which confirms that the units of each list are the same

trait_and_metadata_dataframe = do.call(rbind, trait_and_metadata_dataframe_list)

# function to remove rows based on a metadata
filter_na_values <- function(data, column) 
{
  data_filtered <- data[!is.na(data[, column]), ] # Filter the data frame based on the presence of NA values in the specified column
  return(data_filtered) # Return the filtered data frame
}

## Dividing into test and train data

get_indices_subset_function <- function(data_frame1, filtering_type, test_site, fraction_split)
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

indices_subset= get_indices_subset_function(trait_and_metadata_dataframe, filtering_type = "Global", test_site = "None", fraction_split = 0.7)
indices_subset_local = get_indices_subset_function(trait_and_metadata_dataframe, filtering_type = "Site_specific", test_site = datasets_to_take_for_trait[1]  , fraction_split = 0.7)

###########################################################################################################
source("/Users/dhruvakathuria/Documents/GitHub/Hierarchical_foliar_trait_estimation/R_codes/Regression_algorithms/Apply_ML_and_prospect_algorithms.R")

# prepping data
Y_train = as.numeric(trait_and_metadata_dataframe$trait[indices_subset ])
Y_test = as.numeric(trait_and_metadata_dataframe$trait[-indices_subset ])
X_train = spectra_matrix[indices_subset, ]
X_test = spectra_matrix[-indices_subset, ]

PLSR_values = apply_regression_algorithm2 ("ridge", X_train, Y_train, X_test, Y_test)
plot(PLSR_values$obs,PLSR_values$predictions)
abline(0, 1, col = "red")

ridge_values = apply_regression_algorithm2 ("ridge", spectra_matrix[indices_subset, ], Carotenoid_vector[indices_subset ], spectra_matrix[-indices_subset, ], Carotenoid_vector[-indices_subset])
plot(ridge_values$obs,ridge_values$predictions)
abline(0, 1, col = "red")

Bayesian_values2 = apply_regression_algorithm2 ("Bayesian_linear_horseshoe", spectra_matrix[indices_subset, ], Carotenoid_vector[indices_subset ], spectra_matrix[-indices_subset, ], Carotenoid_vector[-indices_subset])
plot(Bayesian_values2$obs, Bayesian_values2$predictions)
abline(0, 1, col = "red")
############################################################################################################

library(ggplot2)
metadata_filter = "Growth_form"

data_mat = trait_and_metadata_dataframe[-indices_subset, ]
Regression_apply2 = apply_regression_algorithm2 ("PLSR", X_train, Y_train, X_test, Y_test)
data_mat$pred = Regression_apply2$predictions
data_mat = data_mat[!data_mat[, metadata_filter] %in% c("Data unavailable", "Error in extraction, check"), ]
data_mat$trait = as.numeric(data_mat$trait)
data_mat$error = (data_mat$pred - data_mat$trait)

ggplot(data_mat, aes(x = trait, y = error, color = .data[[metadata_filter]])) +
  geom_point() #+ geom_abline()

