mainDir = "/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/NASA_proposal_3.1.2/ECOSIS_Data_download_Dhruva"  

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
library(arrow)
library(units)

filter_trait_data_only = function(x, trait_name1)
{
  metadata_arrow_version = read_parquet(paste0(file.path(mainDir, x), "/", "metadata_updated.parquet"))
  name_for_trait = attributes(metadata_arrow_version)$trait_names[[trait_name1]][["trait_name"]]
  units_for_trait = attributes(metadata_arrow_version)$trait_names[[trait_name1]][["units"]]
  
  names(metadata_arrow_version) = tolower(names(metadata_arrow_version))
  trait_value = as.numeric(unlist(metadata_arrow_version[name_for_trait]))
  
  if(!(trait_name1 %in% c("delta_C13", "delta_N15")))
  {
    trait_value[trait_value < 0] = NA
  }
  
  trait_value = trait_value * as_units(units_for_trait)
  
  if(trait_name1 == "Carotenoid_Area") # instead of the for loop, for each trait I need to make a list like the ones I made in "trait_and_sample_id_Database_for_ECOSIS_Data.R" which connects the trait_name and the chosen unit
  {
    trait_value = set_units(trait_value, microgram/cm^2)
  }
  trait_value
}

Carotenoid_values_filtered = lapply(datasets_to_take_for_trait, filter_trait_data_only, trait_name1 =  "Carotenoid_Area")
## IMP: Need to have a check which confirms that the units of each list are the same

Carotenoid_vector = unlist(Carotenoid_values_filtered)

## Dividing into test and train data
indices_subset = sample(1:length(Carotenoid_vector), 0.7*length(Carotenoid_vector))

Carotenoid_vector = as.numeric(Carotenoid_vector)

########################################################################################################
source("/Users/dhruvakathuria/Documents/GitHub/Hierarchical_foliar_trait_estimation/R_codes/Regression_algorithms/Apply_ML_and_prospect_algorithms.R")

PLSR_values = apply_regression_algorithm2 ("PLSR", spectra_matrix[indices_subset, ], Carotenoid_vector[indices_subset ], spectra_matrix[-indices_subset, ], Carotenoid_vector[-indices_subset])
plot(PLSR_values$obs,PLSR_values$predictions)
abline(0, 1, col = "red")

ridge_values = apply_regression_algorithm2 ("lasso", spectra_matrix[indices_subset, ], Carotenoid_vector[indices_subset ], spectra_matrix[-indices_subset, ], Carotenoid_vector[-indices_subset])
plot(ridge_values$obs,ridge_values$predictions)
abline(0, 1, col = "red")

Bayesian_values2 = apply_regression_algorithm2 ("Bayesian_linear_horseshoe", spectra_matrix[indices_subset, ], Carotenoid_vector[indices_subset ], spectra_matrix[-indices_subset, ], Carotenoid_vector[-indices_subset])
plot(Bayesian_values2$obs, Bayesian_values2$predictions)
abline(0, 1, col = "red")


