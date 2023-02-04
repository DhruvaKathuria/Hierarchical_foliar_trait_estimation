source(paste0(Github_dir,  "Creating_folder_for_further_data_cleaning_and moving_data_there.R"))

create_folder_and_move_files(dataset_name1)
metadata_file_from_ECOSIS = readr :: read_csv(file = paste0(file.path(mainDir, dataset_name1, "/data_downloaded_using_api"), "/", "metadata.csv"), show_col_types = FALSE)
spectra_file_from_ECOSIS = readr :: read_csv(file = paste0(file.path(mainDir, dataset_name1, "/data_downloaded_using_api"), "/", "spectra.csv"), show_col_types = FALSE)

##### add manufacturer and model names
metadata_file = metadata_file_from_ECOSIS
metadata_file$manufacturer = metadata_file$model = sapply(metadata_file_from_ECOSIS$Sample_ID, function(x) strsplit(x, "")[[1]][1])
metadata_file$manufacturer[metadata_file$manufacturer == "L"] = "analytical spectral devices"
metadata_file$manufacturer[metadata_file$manufacturer == "P"] = "spectral evolution"
metadata_file$model[metadata_file$model == "L"] = "fieldspec 3"
metadata_file$model[metadata_file$model == "P"] = "psr+"  
  
readr :: write_csv(spectra_file_from_ECOSIS, file = paste0(file.path(mainDir, dataset_name1), "/", "spectra.csv"))
readr :: write_csv(metadata_file, file = paste0(file.path(mainDir, dataset_name1), "/", "metadata.csv"))
