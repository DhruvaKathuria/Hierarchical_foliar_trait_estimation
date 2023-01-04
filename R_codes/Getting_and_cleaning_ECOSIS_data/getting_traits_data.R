library(stringr)

mainDir = "/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/NASA_proposal_3.1.2/ECOSIS_Data_download_Dhruva"
datasets_downloaded = list.files(mainDir)

#####################################################################################
###For now I am making a database of the commonly used trait names in the datasets used in ECOSIS
LMA_vector = c("lma", "leaf_mass_per_area", "leaf mass per area")
Chl_vector = c("chlorophyll", "chl", "chl_ab", "chlab") # work on how to separate chla and chlb from chltot
Chla_vector = c("chla", "chl_a", "chlb", "chl_b", "chlorophyll_a", "chlorophyll_b", "chlorophylla", "chlorophyllb")
Carbon_vector = c("c", "carbon", "c_")
Calcium_vector = c("Ca", "calcium")
Nitrogen_vector = c("nitrogen", "n", "n%")
Carbon_Nitrogen = "c:n"
Cellulose_vector = c("cellulose")
Lignin_vector = "lignin"
Usda_symbol = c("usda", "usda symbol")
#####################################################################################


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
out1 = list()
for(i in 1: length(datasets_downloaded))
{
  out1[[i]] = check_column_name(i, LMA_vector)
}
names(out1) = datasets_downloaded
####################################################################################

out1[!is.na(out1)] # these are the relavant datasets for the particular trait


