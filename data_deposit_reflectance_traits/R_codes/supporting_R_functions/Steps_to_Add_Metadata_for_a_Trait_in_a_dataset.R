#Steps for updating metadata for a trait in a dataset


# Steps for a particular trait applicable to all datasets -----------------

## for the chosen trait_name, Open *supporting_R_functions/trait_and_sample_id_Database_for_ECOSIS_Data.R* 
## to see if the function *subset_column_names_final_pass* has the trait name 
## values for *exact_vector_list_for_covariate_first_pass_list* and 
## *exact_vector_list_for_covariate_second_pass_list*

## Uncomment below if you do not have values for the above
# out1 = subset_trait_data_zeroeth_pass_function(trait_name1)
# out1

## This will give you all the datasets with the matching names. 
## Now we first fill in *exact_vector_list_for_covariate_first_pass*. 
## For this note down all the names that are corresponding to the trait_name1 
## in each of the datasets. These need to be exact matches
## Once we fill in all the values in *exact_vector_list_for_covariate_first_pass*, run
exact_vector_list_for_covariate_first_pass = exact_vector_list_for_covariate_first_pass_list[[trait_name1]]
subset_list1 = lapply(subset_trait_data_zeroeth_pass_function(trait_name1), subset_function1)
subset_list2 = subset_list1[which(!is.na(subset_list1))]

## If there is difference in the length of subset_list1 and subset_list2 then there are some datasets which are giving NA. Look it up to confirm that you have not accidentaly left out a column name for a trait

## For the datasets, which have more than one matching columns, 
## choose the ones which match exactly and assign them to the  
## *exact_vector_list_for_covariate_second_pass_list* 
## in *supporting_R_functions/trait_and_sample_id_Database_for_ECOSIS_Data.R*
exact_vector_list_for_covariate_second_pass <-  
  exact_vector_list_for_covariate_second_pass_list[[trait_name1]] # this is subsetting by eye 
                                                                  # the output of above which has more 
                                                                  # than one matching dataset for the above. 
                                                                  # The second pass is imp for example for 
                                                                  #separating trait_area from trait_mass 
                                                                  # or chl_a from chl_tot
subset_list3 = lapply(subset_list1, trait_name_subset)
subset_list3
## Make sure each dataset in subset_list3 has only one element, 
## if not you will have to tweak the code (based on a particular trait) in 
## *supporting_R_functions/getting_traits_data.R* under 
## function "subset_column_names_final_pass"


# Trait indepenent metadata related to specific dataset ------------------------------------

## Open the metadata file
dataset_name1 = dataset_name1
metadata_file_from_ECOSIS = readr :: read_csv(file = paste0(file.path(mainDir, dataset_name1), "/", "metadata.csv"), show_col_types = FALSE)
spectra_file_from_ECOSIS = readr :: read_csv(file = paste0(file.path(mainDir, dataset_name1), "/", "spectra.csv"), show_col_types = FALSE)

## Check the spectra file to see if you have relavant spectra or not

## If trait units are available in the metadata file, go to 
## *Getting_and_Cleaning_Ecosis_data/02_create_parquet_metadata.R* 
## and add the unit under *trait_name1_unit*.

## Fill in the below lists in *trait_and_sample_id_Database_for_ECOSIS_Data.R* 
## if the below are FALSE which means that the 
## dataset has not been processed for trait independent metadata
dataset_name1 %in% names(is_spectral_measurement_reflectance_only_Boolean_list)
dataset_name1 %in% names(sample_id_list)
dataset_name1 %in% names(Paper_link_list)
dataset_name1 %in% names(is_spectral_instrument_used_globally_list)
dataset_name1 %in% names(instrument_manufacturer_info_global_list)
dataset_name1 %in% names(instrument_model_info_global_list)

## Always check that the data are leaf only. 
## can be seen from going to paste0("https://ecosis.org/package/", dataset_name1) 
## and also check the metadata file

## Add the **is_spectral_measurement_reflectance_only_Boolean_list** in 
## **supporting_R_functions/trait_and_sample_id_Database_for_ECOSIS_Data.R** (if available from metadata). 
## Open paste0("https://ecosis.org/package/", dataset_name1) and confirm

## If the Boolean_list is F or if the spectral_information is not global,
## go to the original dataset folder in raw_data and create a new R-code which 
## first does 
## source(source(paste0(Github_dir,  "Creating_folder_for_further_data_cleaning_and moving_data_there.R"))). 
## This source makes a new folder and transfers the metadata and spectra files there, 
## and then write new metadata/spectra file filtered only with reflectance data 
## (or with the added manufacturer/model columns in dataframe if spectral instrument is not global ) 
## and save in the dataset folder
## For future reference, save the R filename in 
## *R_codes/Getting_and_cleaning_ECOSIS_data/Further_data_cleaning* 

## Add the **sample_id** variable name in 
## **trait_and_sample_id_Database_for_ECOSIS_Data.R** (if available)

## Open paste0("https://ecosis.org/package/", dataset_name1)
## From the ECOSIS site, look at the Paper under *Linked Resources* 
## If trait_units not found in step 4, Check the paper for units of the trait 
## and add under *02_create_parquet_metadata.R*

## If **is_spectral_measurement_reflectance_only_Boolean_list** not found in step 4, 
## look under measurement quantity on the ECOSIS site

## Add the paper_link and spectral instrument information from ECOSIS website 
## in **supporting_R_functions/trait_and_sample_id_Database_for_ECOSIS_Data.R**
