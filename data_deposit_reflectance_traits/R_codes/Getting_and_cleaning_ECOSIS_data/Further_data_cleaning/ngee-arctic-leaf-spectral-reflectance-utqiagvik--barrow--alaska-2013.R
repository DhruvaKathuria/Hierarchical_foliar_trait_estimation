rm(list = ls())
mainDir = "/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/NASA_proposal_3.1.2/ECOSIS_Data_download_Dhruva"  
dataset_name1 = "ngee-arctic-leaf-spectral-reflectance-utqiagvik--barrow--alaska-2013"

metadata_file_from_ECOSIS = readr :: read_csv(file = paste0(file.path(mainDir, dataset_name1, "data_downloaded_using_api"), "/", "metadata.csv"), show_col_types = FALSE)
spectra_file_from_ECOSIS = readr :: read_csv(file = paste0(file.path(mainDir, dataset_name1, "data_downloaded_using_api"), "/", "spectra.csv"), show_col_types = FALSE)

##### remove Carotenoid columns as there is not data only -9999 in values
metadata_file = metadata_file_from_ECOSIS[,!(names(metadata_file_from_ECOSIS) %in% c("Carot_tot_L", "Carot_tot_area_L"))]
spectra_file = spectra_file_from_ECOSIS

readr :: write_csv(spectra_file, file = paste0(file.path(mainDir, dataset_name1), "/", "spectra.csv"))
readr :: write_csv(metadata_file, file = paste0(file.path(mainDir, dataset_name1), "/", "metadata.csv"))
