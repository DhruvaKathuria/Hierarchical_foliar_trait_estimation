rm(list = ls())
pls_pred_list = list()
#PROSPECT parameters = leaf_chla_per_area, leaf_chlb_per_area,leaf_chltot_per_area, leaf_cartot_per_area, leaf_mass_per_area, leaf_prospect_N, leaf_anth_per_area

#for(covariate_to_analyze in c("leaf_mass_per_area",  "leaf_chla_per_area", "leaf_chlb_per_area", "leaf_chltot_per_area", "leaf_cartot_per_area", "leaf_prospect_N", "leaf_anth_per_area"))
for(covariate_to_analyze in c("leaf_chltot_per_area", "leaf_cartot_per_area")) # this represents the traits that the user wants to analyze 
{
  print(covariate_to_analyze)
  
  take_sunlit_leaves_only = F
  interpolation_method =  "spline"  #"linear", "spline", "None" #For None, it will not do any interpolation, do not choose this option for ML analysis
  take_only_field_data = T
  stratified_sampling = T # When stratified_sampling is T, this means that we are taking an entire experiment as a subset
  ###################################################
  
  R_codes_folder = "/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/NASA_proposal_3.1.2/R_codes/"
  source(paste0(R_codes_folder, "Extract_metadata_from_Alexey_files.R")) # This selects the sites which has observations for the particular covariate
  metadata_files_to_take_covariate = filter_by_covariate(covariate_to_analyze) 
  
  source(paste0(R_codes_folder,"interpolation_of_missing_spectra.R")) # this code fills in any missing spectra using linear/cubic interpolation
  source(paste0(R_codes_folder,"Apply_ML_and_prospect_algorithms.R")) # options are PLSR, Random_Forest, PROSPECT, ANN, cluster_plus_PLSR, land_class_plus_PLSR
  
  
  ind_rm_metadata = which(names(metadata_list) == "ecosis_cedarcreek_biodiversity" ) # we remove this site, because it has errors (source: Alexey Shiklomanov, Sean Serbin)
  ind_rm_spectra = which(names(spectra_file_list) == "ecosis_cedarcreek_biodiversity")
  if(length(ind_rm_metadata) != 0)
  {
    metadata_list = metadata_list[-ind_rm_metadata]
    
  }
  
  if(length(ind_rm_spectra) != 0)
  {
    spectra_file_list = spectra_file_list[-ind_rm_spectra]
    
  }
  ###Prepare test and train data (X = spectra, Y = trait)
  
  # For now, we take only one site as the test site and take the rest for training
  set.seed(123)
  
  if(length(spectra_file_list) > 1)
  {
    for(subset1 in 1:length(spectra_file_list))
    {
      #test_site_index = sample(1:length(spectra_file_list), 1)
      test_site_index = subset1
      train_sites = names(spectra_file_list)[-test_site_index]
      test_sites = names(spectra_file_list)[test_site_index]
      print(test_sites)
      
      spectra_data_train_matrix = matrix(unlist(spectra_file_list[train_sites]), ncol = 2500 - 400 + 1, byrow = T) 
      covariate_train = as.numeric(unlist(lapply(metadata_list[train_sites], function(x){x[covariate_to_analyze]})))
      
      spectra_data_test_matrix = matrix(unlist(spectra_file_list[test_sites]), ncol = 2500 - 400 + 1, byrow = T) 
      covariate_test = unlist(lapply(metadata_list[test_sites], function(x){x[covariate_to_analyze]}))
      
      ##Apply the algorithms
      #for(method1 in c("ridge", "Random_Forest", "elastic_net", "PLSR", "PCR", "lasso"))
      for(method1 in c("PLSR", "Bayesian_linear_horseshoe")) # for a list of methods you can use here, refer to "Apply_ML_and_prospect_algorithms.R"
      {
        print(method1)
        x1 = Sys.time()
        pls_pred = apply_regression_algorithm(method1, spectra_data_train_matrix, covariate_train, spectra_data_test_matrix, covariate_test)
        x2 = Sys.time()
        print (x2 - x1)
        pls_pred[['time_diff']] = x2 - x1
        pls_pred_list[[covariate_to_analyze]] [[ names(spectra_file_list)[test_site_index] ]] [[method1]] = pls_pred # this list gives the accuracy stats, time taken, predictions, observations
      }
      saveRDS(pls_pred_list, file = "pls_pred_file_Dec06.rds")
    }
  }else{pls_pred_list[[covariate_to_analyze]][[method1]] = NA}
}
#plot(pls_pred$predictions, covariate_test)
#abline(0,1)

#hist(covariate_test)
#hist(covariate_train)
