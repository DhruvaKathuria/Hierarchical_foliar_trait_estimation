---

---
# Hierarchical_foliar_trait_estimation
Estimating  foliar plant function traits using hyperspectral data.
 
## Getting and Cleaning ECOSIS data
All codes in R_codes/Getting_and_cleaning_ECOSIS_data

### Getting ECOSIS data
1) The code [[01_get_data_ecosis.R]] reads the ECOSIS website and downloads data which has not been already downloaded and separates the output into metadata and spectra folders

### Cleaning ECOSIS data
1) [[02_create_parquet_metadata.R]]: This is the main code that writes the parquet file for the metadata. Each data folder that is analyzed has its own parquet file which contains the downloaded metadata dataframe plus the metadata parquet file that I create. The code also writes a [[trait_already_done_for_metadata.txt]] to the data folder which contains the traits that I have already added the metadata for.

2) The code [[03_adding_genus_species_info.R]] crunches the metadata and adds a column for genus and species. We also add Family data using taxize package. Other data related to taxize can be added as well if the user wants.

### Supporting_R_functions
These are .R files in R_codes/supporting_R_functions used by main R files

1) [[getting_traits_data.R]]: This code has functions used by [[adding_more_metadata_information_to_metadata_files.R]]. Code is mostly automatic but needs a visual inspection for new trait names and to build the database in [[trait_and_sample_id_Database_for_ECOSIS_Data.R]].

2) [[Steps_to_Add_Metadata_for_a_Trait_in_a_dataset.R]]: This is an instruction file for [[adding_more_metadata_information_to_metadata_files.R]] and  needs to be read before running beyond Line 20 of the [[adding_more_metadata_information_to_metadata_files.R]]. This file contains steps on how and where to add the metadata information which is then read by the various R-codes. 

3) [[trait_and_sample_id_Database_for_ECOSIS_Data.R]]: This is where most of the metadata specific to each dataset is saved.

4) [[scrape_wikipedia_for_species_metadata_table.R]]: This is the code which scrapes wikipedia and adds metadata group information. The data arising from this is stored in data/Species_data. Update: I found that using ChatGPT, it is much more accurate, so the updated species_attribute_data is the one that is currently being used in the code (data/species_data/species_attribute_data_Dhruva_GPT.csv)

#### Extra data cleaning for some datasets
1) Folder: Getting_and_cleaning_ECOSIS_Data/Further_data_cleaning: In some of the datasets, I write further R-codes which clean the metadata so that they can be used in the analysis. For such datasets, the original downloaded datasets are in the sub-folder "data_downloaded_using_api" and the modified versions are in the main dataset folder. 

#### Where data are kept
Both the downloaded data and the parquet versions are in the raw_data folder inside their respective dataset folders.
