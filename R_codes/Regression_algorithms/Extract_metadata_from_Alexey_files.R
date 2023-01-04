library(dplyr)
library(csvy)
############Write code to analyze all the files########
setwd("/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/NASA_proposal_3.1.2/spectra_db-master")

filter_by_covariate = function(covariate_to_take)
{
  metadata_files = list.files(pattern = "metadata.csvy", recursive = T)
  metadata_files_to_take_covariate = vector()
  for(i in metadata_files) 
  {
    metadata_1 = read_csvy(i)
    folder_to_take_covariate = unlist(strsplit(i, "/"))[1]
    spectra_files_1 = list.files(path = paste0(folder_to_take_covariate, "/spectra"))
    spectra_file_1 = read_csvy(paste0(folder_to_take_covariate, "/spectra/", spectra_files_1[1]))
    spectra_file_1 = na.omit(spectra_file_1)
    
    condition1 = covariate_to_take %in% colnames(metadata_1)
    condition2 = min(spectra_file_1$wavelengths) <= 400 & max(spectra_file_1$wavelengths) >= 2500 
    
    if(condition1 == T & condition2 == T){metadata_files_to_take_covariate = c(i, metadata_files_to_take_covariate)}
  }
  metadata_files_to_take_covariate
}

