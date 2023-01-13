###########Steps for updating metadata for a trait in a dataset

##########################TRAIT related global steps valid for all datasets################################################

# Step:1 for the chosen trait_name, Open *getting_traits_data.R* to see if the function *subset_column_names_final_pass* has the trait name values for *exact_vector_list_for_covariate_first_pass* and *exact_vector_list_for_covariate_second_pass*
## If you do not have **fill this later**


############################################################################################################################

##################################################Metadata related to specific dataset####################################
# Step:3 Open the metadata file


metadata_file_from_ECOSIS = readr :: read_csv(file = paste0(file.path(mainDir, dataset_name1), "/", "metadata.csv"), show_col_types = FALSE)

# Step:4 Get data from Metadata file
#### If trait units are available in the metadata file, go to *adding_more_metadata_information_to_metadata_files.R* and add under *trait_name1_unit* (Line 10).

#### Add the **is_spectral_measurement_reflectance_only_Boolean_list** in **trait_and_sample_id_Database_for_ECOSIS_Data.R** (if available)
#########If the Boolean_list is F,in the original data folder, create a folder "data_downloaded_using_api" and transfer both the metadata and spectra files there 
#########and create A new R-code which new metadata/spectra file filtered only with reflectance data and save in the dataset folder
#########For future reference, save the R filename in *GitHub_folder/Hierarchical_foliar_trait_estimation/R_codes/Getting_and_cleaning_ECOSIS_data/Further_data_cleaning* 

#### Add the **sample_id** variable name in **trait_and_sample_id_Database_for_ECOSIS_Data.R** (if available)


# Step 5 Open paste0("https://ecosis.org/package/", dataset_name1)
#### From the ECOSIS site, look at the Paper under *Linked Resources* 
#### If trait_units not found in step 4, Check the paper for units of the trait and add under *adding_more_metadata_information_to_metadata_files.R*
#### If **is_spectral_measurement_reflectance_only_Boolean_list** not found in step 4, look under measurement quantity on the ECOSIS site
#### Add the paper_link and spectral instrument information from ECOSIS website in **trait_and_sample_id_Database_for_ECOSIS_Data.R**
