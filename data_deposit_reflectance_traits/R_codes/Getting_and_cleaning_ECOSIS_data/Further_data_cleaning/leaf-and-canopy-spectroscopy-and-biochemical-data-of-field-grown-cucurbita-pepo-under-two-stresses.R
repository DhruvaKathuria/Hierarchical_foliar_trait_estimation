source(paste0(Github_dir,  "Creating_folder_for_further_data_cleaning_and moving_data_there.R"))

create_folder_and_move_files(dataset_name1)
metadata_file_from_ECOSIS = readr :: read_csv(file = paste0(file.path(mainDir, dataset_name1, "/data_downloaded_using_api"), "/", "metadata.csv"), show_col_types = FALSE)
spectra_file_from_ECOSIS = readr :: read_csv(file = paste0(file.path(mainDir, dataset_name1, "/data_downloaded_using_api"), "/", "spectra.csv"), show_col_types = FALSE)

##### add manufacturer and model names
metadata_file = metadata_file_from_ECOSIS
indices_to_take = which(metadata_file$Paired_Spectra == "leaf")

metadata_file = metadata_file[indices_to_take, ]
spectra_file = spectra_file_from_ECOSIS[indices_to_take, ]

readr :: write_csv(spectra_file, file = paste0(file.path(mainDir, dataset_name1), "/", "spectra.csv"))
readr :: write_csv(metadata_file, file = paste0(file.path(mainDir, dataset_name1), "/", "metadata.csv"))
