mainDir = "/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/NASA_proposal_3.1.2/ECOSIS_Data_download_Dhruva"
subDir = "cabo-2018-2019-leaf-level-spectra"

data_input = readr:: read_csv("/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/NASA_proposal_3.1.2/ECOSIS_Data_download_Dhruva/cabo-2018-2019-leaf-level-spectra/ref_spec.csv")

spectra_data = data_input[, which(colnames(data_input) == "400"):ncol(data_input)]
metadata = data_input[, !(colnames(data_input) %in% colnames(spectra_data))]

write.csv(metadata, file = paste0(file.path(mainDir, subDir), "/", "metadata.csv"), row.names = F)
write.csv(spectra_data, file = paste0(file.path(mainDir, subDir), "/", "spectra.csv"), row.names = F)
