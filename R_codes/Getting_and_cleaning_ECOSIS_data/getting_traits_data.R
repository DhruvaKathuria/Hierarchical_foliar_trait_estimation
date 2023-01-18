
datasets_downloaded = list.files(mainDir)
source(paste0(Github_dir, "trait_and_sample_id_Database_for_ECOSIS_Data.R")) 

#####################################################################################
get_column_name = function(index, trait_vector)
{
  subDir = datasets_downloaded[index]
  # the below datasets have been downloaded for "leaf" from ECOSIS using "Getting_ECOSIS_data_from_API_and_storing_metadata.R"
  r3_trait_only = readr:: read_csv(paste0(file.path(mainDir, subDir), "/", "metadata.csv"), show_col_types = FALSE) # getting the metadata files
  
  if(nrow(r3_trait_only) != 0)
  {
    names1 = tolower(names(r3_trait_only)) # we do everything lower case so that there are no discrepancies
    trait_vector = tolower(trait_vector)
    
    match<- paste(trait_vector[order(-nchar(trait_vector))], collapse = '|^') 
    test<- str_extract_all(names1, match, simplify= T) # this shows the exact name in the trait vector which matches with the column name. "" indicates no match.
    
    covariate_test = unique(test[test!= ""]) # these are the names in trait_vector that match (approximately) with the column names
    if(length(table(test)) >= 2) # if there are more than one column names that match elements in the trait_vector; we take two because there is always "" which denotes no matching
    {
      if(any(nchar(covariate_test) == 1)) # we do this for elements that are written as "C", "K", etc. as a lot of names match with single letters
      {
        trait_to_take = covariate_test[nchar(covariate_test) == 1] # we first see if we have any column name satisfying the criterion for single letter column names
        column_to_take = names1[names1 == trait_to_take]
        if(length(column_to_take) == 0)
        {
          column_to_take = names1[test != "" & test != trait_to_take] # if the single letter criterion is not fulfilled, then we take all of the rest of the columns, we will have to manually look at the column names
          if(length(column_to_take) == 0)
          {
            column_to_take = NA
          }
        }
      }else
      {
        column_to_take = names1[test != ""]
      }

    }else{column_to_take = NA}
  }else{column_to_take = NA}
  column_to_take
    ## This will fail if we only have chla and chlb
  #   if(length(column_to_take) > 1)
  #   {
  #     target_column_names = names1[column_to_take]
  #     indices_for_target_column_names = which(target_column_names %in% trait_vector)
  #     if(length(indices_for_target_column_names) > 0)
  #     {
  #       target_column_name_to_take = target_column_names[indices_for_target_column_names]
  #     }else{target_column_name_to_take = NA}
  #     
  #   }else if(length(column_to_take) == 1)
  #   {
  #     target_column_name_to_take = names1[column_to_take]
  #   }else if(length(column_to_take) == 0)
  #   {
  #     target_column_name_to_take = NA
  #   }
  #   }else{target_column_name_to_take = NA}
  # target_column_name_to_take
}


####################################################################################
# Apply function to all the ECOSIS datasets
# can write an lapply loop for this as well (just easier to debug using for loop)

subset_trait_data_zeroeth_pass_function = function(trait_name)
{
  out1 = list()
  for(i in 1: length(datasets_downloaded))
  {
    out1[[i]] = get_column_name(i, trait_vector_list[[trait_name]])
  }
  names(out1) = datasets_downloaded
  ####################################################################################
  
  out2 = out1[!is.na(out1)] # these are the relavant datasets for the particular trait
  out2
}

subset_column_names_final_pass = function(trait_name)
{
  
  exact_vector_list_for_covariate_first_pass = exact_vector_list_for_covariate_first_pass_list[[trait_name]] # this is after subsetting by eye the output of subset_trait_data_zeroeth_pass_function
  exact_vector_list_for_covariate_second_pass = exact_vector_list_for_covariate_second_pass_list[[trait_name]] # this is subsetting by eye the output of above which has more than one matching dataset for the above. The second pass is imp for example for separating trait_area from trait_mass or chl_a from chl_tot

  subset_function1 = function(x) 
  {
    boolean_1 = any(x %in% exact_vector_list_for_covariate_first_pass) == T
    if(boolean_1 == T)
    {
      y = x
    }else
    {
      y = NA
    }
    y
  }
  
  subset_list = lapply(subset_trait_data_zeroeth_pass_function(trait_name), subset_function1)
  subset_list = subset_list[which(!is.na(subset_list))]
  
  ##choosing one item only for multi-item lists
  
  trait_name_subset <- function(x) # this is for the datasets which has more than one matching dataset
  {
    if(length(x) > 1)
    {
      indices_to_keep = which(x %in% exact_vector_list_for_covariate_second_pass)
    }else{indices_to_keep = 1}
    x2 = x[indices_to_keep]
    x2
  }
  
  subset_list = lapply(subset_list, trait_name_subset)
}


