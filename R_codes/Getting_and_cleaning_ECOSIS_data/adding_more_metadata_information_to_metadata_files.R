rm(list = ls())

library(arrow)
library(stringr)

mainDir = "/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/NASA_proposal_3.1.2/ECOSIS_Data_download_Dhruva"  


trait_name1 = "Carotenoid_Area"
Github_dir = "/Users/dhruvakathuria/Documents/GitHub/Hierarchical_foliar_trait_estimation/R_codes/Getting_and_cleaning_ECOSIS_data/"
source(paste0(Github_dir, "getting_traits_data.R"))

# Check all datasets relavant to that trait_name
print(subset_column_names_final_pass(trait_name1))

## Pick a dataset and change the dataset_name1
dataset_name1 = "productivity-and-characterization-of-soybean-foliar-traits-under-aphid-pressure"

## Datasets which have issues, for specific issues why a dataset wasnt selected, laod the metadata_arrow_version.parquet using read_parquet and type comment(metadta_arrow_version) 
datasets_not_to_take = c("leaf-reflectance-plant-functional-gradient-ifgg-kit")

if(!(dataset_name1 %in% datasets_not_to_take))
{

  ##IMP NOTE Go through the *Steps_to_Add_Metadata_for_a_Trait_in_a_dataset.R* for this trait and dataset combination
  ##IMP NOTE Only move further after completing all the steps
  
  trait_name1_unit = "microgram/cm^2"
  source(paste0(Github_dir, "getting_traits_data.R"))
  
  
  
  ##############metadata_function#########
  # dataset_name = "cabo-2018-2019-leaf-level-spectra"
  # sample_id_variable = "sample_id"
  # 
  # variable_name1 = "Carotenoid_Area"
  # units_for_variable_name1 = "microgram/cm^2"
  
    
  add_metadata_function = function(dataset_name, variable_name1, units_for_variable_name1)
  {
    metadata_file_from_ECOSIS = readr :: read_csv(file = paste0(file.path(mainDir, dataset_name), "/", "metadata.csv"), show_col_types = FALSE)
    
    ## See if you already have added metadata to the dataset for any other trait_name
    traits_already_done_path = paste0(file.path(mainDir, dataset_name), "/", "traits_already_done_for_metadata.txt")
    if(file.exists(traits_already_done_path))
    {
    metadata_traits_already_added = readr :: read_lines(traits_already_done_path)
    }else{metadata_traits_already_added = NA}
    
    # read in the previous version of metadata_arrow_version
    if(!is.na(metadata_traits_already_added))
    {
      metadata_arrow_version = read_parquet(paste0(file.path(mainDir, dataset_name), "/", "metadata_updated.parquet"))
    }else
    {
      metadata_arrow_version = metadata_file_from_ECOSIS
      ##################set global attributes########################
      ## sample_id
      sample_id_vector = metadata_arrow_version[sample_id_list[[dataset_name]] ]
      if(length(sample_id_vector)!=0)
      {
        metadata_arrow_version = metadata_arrow_version[!names(metadata_arrow_version) %in% sample_id_list[[dataset_name]] ]
        metadata_arrow_version$sample_id = as.character(sample_id_vector)
      }else
      {
        metadata_arrow_version$sample_id = rep(NA, nrow(metadata_arrow_version))
      }
      
      ## only reflectance values
      attributes(metadata_arrow_version)$reflectance_values_only = is_spectral_measurement_reflectance_only_Boolean_list[[dataset_name]] # if this is FALSE, then need to filter out the reflectance values
      if(is_spectral_measurement_reflectance_only_Boolean_list[[dataset_name]] == F)
      {
        
      }
      
      ## paper_link
      attributes(metadata_arrow_version)$Paper_links = Paper_link_list[[dataset_name]]
      
      ## spectral instrument
      attributes(metadata_arrow_version)$is_spectral_instrument_used_globally = is_spectral_instrument_used_globally_list[[dataset_name]]
      if(attributes(metadata_arrow_version)$is_spectral_instrument_used_globally == T)
      {
        metadata_arrow_version$manufacturer = rep(instrument_manufacturer_info_global_list[[dataset_name]], nrow(metadata_arrow_version))
        metadata_arrow_version$model = rep(instrument_model_info_global_list[[dataset_name]], nrow(metadata_arrow_version))
      }else
      {
        
      }
      
    }
    
    if(variable_name1 %in% metadata_traits_already_added == F) # local trait_name_metadata
    {
      subset_list = subset_column_names_final_pass(variable_name1)
      column_name = unlist(subset_list[names(subset_list) == dataset_name])
      attributes(metadata_arrow_version)$trait_names[[variable_name1]][["trait_name"]] = as.character(column_name)
      attributes(metadata_arrow_version)$trait_names[[variable_name1]][["units"]] = units_for_variable_name1
      
      metadata_traits_already_added = c(metadata_traits_already_added, variable_name1)
      metadata_traits_already_added = as.character(na.omit(metadata_traits_already_added))
      readr::write_lines(metadata_traits_already_added, traits_already_done_path)
    }
    metadata_arrow_version
  }
  
  metadata_arrow_version =  add_metadata_function(dataset_name1, trait_name1, trait_name1_unit)
  
  #####################Uncomment below if you want to check the datasets subsetted using a variable_name##########################
  #print(subset_trait_data_zeroeth_pass_function("Carotenoid_Area"))
  #print(subset_column_names_final_pass("Carotenoid_Area"))
  ###########################################################################################################################
  
  
  
  #####################Add a comment below if you want to add a comment to the dataset; otherwise keep it to NULL##########################
  # always add the date when adding comments
}else{metadata_arrow_version  = data.frame()}
comment_to_add = "January 11, 2023: The Carotenoid data does not have any units, and I did not find any papers on ECOSIS and could not search as well. Plotting the data and comparing with TRY data does seem to suggest that the data is in ug/cm2.So assuming that unit for now."
#comment_to_add = NULL

if(!is.null(comment_to_add))
{
  if(is.null(comment(metadata_arrow_version)))
  {
    comment(metadata_arrow_version) = comment_to_add
  }else{
    comment2 = paste0(comment_to_add, "\n", comment(metadata_arrow_version))
    comment(metadata_arrow_version) = c(comment_to_add, comment(metadata_arrow_version))
  }
}
#################################################################################################################################
write_parquet(metadata_arrow_version,  paste0(file.path(mainDir, dataset_name1), "/", "metadata_updated.parquet"))


##########################################Adding any other metadata later in the data############################################

add_extra_metadata = function(dataset_name1, metadata_name1, metadata_value1) # change function as necessary like adding new sub-lists etc
{
  metadata_arrow_version = read_parquet(paste0(file.path(mainDir, dataset_name1), "/", "metadata_updated.parquet"))
  attributes(metadata_arrow_version)[[metadata_name1]] = metadata_value1
  write_parquet(metadata_arrow_version,  paste0(file.path(mainDir, dataset_name1), "/", "metadata_updated.parquet"))
}

###################################################################################################################################

