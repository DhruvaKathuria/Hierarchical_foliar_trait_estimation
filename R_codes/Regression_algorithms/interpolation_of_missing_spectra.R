
spectra_file_list = list()
metadata_list = list()

for(i in 1:length(metadata_files_to_take_covariate))
{
  metadata_1 = read_csvy(metadata_files_to_take_covariate[i])
  folder_to_take_covariate = unlist(strsplit(metadata_files_to_take_covariate[i], "/"))[1]
  #spectra_files_1 = list.files(path = paste0(folder_to_take_covariate, "/spectra"))
  
  if(take_sunlit_leaves_only == T)
  {
    metadata_covariate = dplyr::filter(metadata_1, sun_shade == "sun")
  }else {metadata_covariate = metadata_1}
  
  if(take_only_field_data == T)
  {
    metadata_covariate = dplyr::filter(metadata_1, is_experiment == FALSE)
  }
  
  observation_ids = metadata_covariate$observation_id
  
  for(jj in 1: length(observation_ids))
  {
    spectra_file_path = paste0(folder_to_take_covariate, "/spectra/", observation_ids[jj], ".csvy")
    if (file.exists(spectra_file_path))
    {
      spectra_file_1 = read_csvy(spectra_file_path)
      
      spectra_file_1 = filter(spectra_file_1, between(wavelengths, 400, 2500))
      indices_to_fill =  which(is.na(spectra_file_1[, 2]))
      if(length(indices_to_fill) > 0)
      {
        if(interpolation_method == "spline")
        {
          sp1 = spline(spectra_file_1[,1], spectra_file_1[,2], xout = spectra_file_1[indices_to_fill, 1] )
          sp1$y[sp1$y < 0] = 0
          sp1$y[sp1$y > 1] = 1
          spectra_file_1[indices_to_fill, 2] = sp1$y
        }
        
        if(interpolation_method == "linear")
        {
          sp2 = approx(spectra_file_1[,1], spectra_file_1[,2], xout = spectra_file_1[indices_to_fill, 1] )
          sp2$y[sp2$y < 0] = 0
          sp2$y[sp2$y > 1] = 1
          spectra_file_1[indices_to_fill, 2] = sp2$y
        }
      }
      spectra_file_list[[folder_to_take_covariate]][[observation_ids[jj]]] = spectra_file_1[, 2]
    }else{metadata_covariate = dplyr :: filter(metadata_covariate, observation_id != observation_ids[jj])}
  }
  metadata_list[[folder_to_take_covariate]] = metadata_covariate
}
