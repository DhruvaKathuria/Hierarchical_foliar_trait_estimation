rm(list = ls())
mainDir = "/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/NASA_proposal_3.1.2/ECOSIS_Data_download_Dhruva"  

metadata_file_from_ECOSIS = readr :: read_csv(file = paste0(file.path(mainDir, "fresh-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests/data_downloaded_using_api"), "/", "metadata.csv"), show_col_types = FALSE)
spectra_file_from_ECOSIS = readr :: read_csv(file = paste0(file.path(mainDir, "fresh-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests/data_downloaded_using_api"), "/", "spectra.csv"), show_col_types = FALSE)

##### filter only reflectance data
indices_to_keep = which(metadata_file_from_ECOSIS$measurement == "reflectance")

spectra_file = spectra_file_from_ECOSIS[indices_to_keep, ]
metadata_file = metadata_file_from_ECOSIS[indices_to_keep, ]

readr :: write_csv(spectra_file, file = paste0(file.path(mainDir, "fresh-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests"), "/", "spectra.csv"))
readr :: write_csv(metadata_file, file = paste0(file.path(mainDir, "fresh-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests"), "/", "metadata.csv"))
