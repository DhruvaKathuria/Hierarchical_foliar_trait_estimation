# Hierarchical_foliar_trait_estimation
 Estimating  foliar plant function traits using hyperspectral data.
## Getting and Cleaning ECOSIS data
All codes in R_codes/Getting_and_cleaning_ECOSIS_data
### Getting ECOSIS data
1) The code [[Getting_ECOSIS_data_from_API_and_storing_metadata.R]] reads the ECOSIS website and downloads data which has not been already downloaded and separates the output into metadata and spectra folders
2) [[getting_traits_data.R]]: Use this code to extract the dataset names (downloaded using the above code) for a particular covariate. Code is mostly automatic but needs a visual inspection for new trait names and build the database in [[trait_and_sample_id_Database_for_ECOSIS_Data.R]].

### Cleaning ECOSIS data
1) "[[adding_more_metadata_information_to_metadata_files.R]]": This is the main code that writes the parquet file for the metadata. Each data folder that is analyzed has its own parquet file which contains the metadata dataframe plus the metadata that I add. The code also writes a "[[trait_already_done_for_metadata.txt]] to the data folder which contains the traits that I have already added the metadata for.

2) 4) The code [[adding_Genus_Species_data_in_metadata_files.R]] crunches the metadata and adds a column for genus and species. We also add Family data using taxize package. Other data related to taxize can be added as well if the user wants.

#### Instruction file for [[adding_more_metadata_information_to_metadata_files.R]] and metadata information for each dataset

2) [[Steps_to_Add_Metadata_for_a_Trait_in_a_dataset.R]]: This file needs to be read before running beyond Line 20 of the"[[adding_more_metadata_information_to_metadata_files.R]]". This file contains steps on how and where to add the metadata information which is then read by the various R-codes.

3) [[trait_and_sample_id_Database_for_ECOSIS_Data.R]]: This is where most of the metadata specific to each dataset is saved.

#### Extra data cleaning for some datasets
4) Folder: Getting_and_cleaning_ECOSIS_Data/Further_data_cleaning: In some of the datasets, I write further R-codes which clean the metadata so that they can be used in the analysis. The R-codes should be updated as I process more metadata.
