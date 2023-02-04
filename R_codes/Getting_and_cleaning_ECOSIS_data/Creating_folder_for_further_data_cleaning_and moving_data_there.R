library(filesstrings)
create_folder_and_move_files = function(dataset_name1)
{
  dir.create(file.path(mainDir, dataset_name1, "data_downloaded_using_api"))
  file.move(paste0(file.path(mainDir, dataset_name1), "/", "spectra.csv"), file.path(mainDir, dataset_name1, "/data_downloaded_using_api"))
  file.move(paste0(file.path(mainDir, dataset_name1), "/", "metadata.csv"), file.path(mainDir, dataset_name1, "/data_downloaded_using_api"))
}