rm(list = ls())
mainDir = "/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/NASA_proposal_3.1.2/ECOSIS_Data_download_Dhruva"  
dataset_name = "dessain-project-reflectance-spectra"

metadata_file_from_ECOSIS = readr :: read_csv(file = paste0(file.path(mainDir, dataset_name, "data_downloaded_using_api"), "/", "metadata.csv"), show_col_types = FALSE)
spectra_file_from_ECOSIS = readr :: read_csv(file = paste0(file.path(mainDir, dataset_name, "data_downloaded_using_api"), "/", "spectra.csv"), show_col_types = FALSE)

##### Get Carotenoid_Area
library(units)
LMA = metadata_file_from_ECOSIS$LMA 
LMA = set_units(LMA, kg/m2)

car_mass = metadata_file_from_ECOSIS$car
car_mass = set_units(car_mass, mg/g)

car_area = LMA * car_mass
car_area = set_units(car_area, microgram/cm^2)

metadata_file_from_ECOSIS$car_area = car_area

metadata_file = metadata_file_from_ECOSIS
spectra_file = spectra_file_from_ECOSIS
readr :: write_csv(spectra_file, file = paste0(file.path(mainDir, dataset_name), "/", "spectra.csv"))
readr :: write_csv(metadata_file, file = paste0(file.path(mainDir, dataset_name), "/", "metadata.csv"))
