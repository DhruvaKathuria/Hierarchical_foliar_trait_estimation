library(taxize)
library(arrow)
library(stringr)

mainDir = "/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/NASA_proposal_3.1.2/ECOSIS_Data_download_Dhruva"  

trait_name1 = "LMA"
datasets_already_processed =  list.files(mainDir, recursive = T, pattern = "traits_already_done_for_metadata.txt")
indices_of_datasets_containing_trait_name =  unlist(lapply(datasets_already_processed, function(x)
{
  read_file1 = readr :: read_lines(file.path(mainDir, x))
  trait_name1 %in% read_file1
})
)

datasets_to_take_for_trait = unlist(lapply(datasets_already_processed[indices_of_datasets_containing_trait_name], function(x) strsplit(x, "/traits_already_done_for_metadata.txt" )[[1]])) 

x = datasets_to_take_for_trait[1]

USDA_symbol_vector = c("usda", "usda symbol")

genus_only_vector_first_list = c("genus")
genus_only_vector_second_list = c("latin genus", "latin.genus")

species_only_vector_first_list = c("species")
species_only_vector_second_list = c("latin species", "latin.species")

no_genus_or_species_first_list = c("latin name", "latin")
no_genus_or_species_second_list = c("latin name", "latin.name")

USDA_attribute_data =  readr ::read_csv("/Users/dhruvakathuria/Downloads/USDA_Plant_database.txt")
trait_name_subset <- function(x, exact_vector_list_for_covariate_second_pass) # this is for the datasets which has more than one matching dataset
{
  if(length(x) > 1)
  {
    indices_to_keep = which(x %in% exact_vector_list_for_covariate_second_pass)
  }else{indices_to_keep = 1}
  x2 = x[indices_to_keep]
  x2
}
index = 6
get_latin_genus = function(index)
{
  dataset_name = datasets_to_take_for_trait[index]
  traits_already_done_path = paste0(file.path(mainDir, dataset_name), "/", "traits_already_done_for_metadata.txt")
  metadata_traits_already_added = readr :: read_lines(traits_already_done_path)
  if( !("Genus_Species" %in% metadata_traits_already_added & "Family" %in% metadata_traits_already_added))
  {
    # the below datasets have been downloaded for "leaf" from ECOSIS using "Getting_ECOSIS_data_from_API_and_storing_metadata.R"
    metadata_arrow_version = read_parquet(paste0(file.path(mainDir, dataset_name), "/", "metadata_updated.parquet"))
    names1 = tolower(names(metadata_arrow_version)) # we do everything lower case so that there are no discrepancies
    names(metadata_arrow_version) = names1
   
    
    match_genus<- paste(genus_only_vector_first_list[order(-nchar(genus_only_vector_first_list))], collapse = '|^') 
    test_genus <- str_extract_all(names1, match_genus, simplify= T) # this shows the exact name in the trait vector which matches with the column name. "" indicates no match.
    covariate_test_genus = unique(test_genus[test_genus!= ""]) # these are the names in trait_vector that match (approximately) with the column names
    column_to_take_genus = names1[test_genus != ""]
    
    if(length(column_to_take_genus) > 1)
    {
      column_to_take_genus =  trait_name_subset(column_to_take_genus, genus_only_vector_second_list)
    }
    
    match_species<- paste(species_only_vector_first_list[order(-nchar(species_only_vector_first_list))], collapse = '|^') 
    test_species <- str_extract_all(names1, match_species, simplify= T) # this shows the exact name in the trait vector which matches with the column name. "" indicates no match.
    covariate_test_species = unique(test_species[test_species!= ""]) # these are the names in trait_vector that match (approximately) with the column names
    column_to_take_species = names1[test_species != ""]
    
    if(length(column_to_take_species) > 1)
    {
      column_to_take_species =  trait_name_subset(column_to_take_species, species_only_vector_second_list)
    }
    
    
    if(length(column_to_take_genus) != 0 & length(column_to_take_species) != 0) # we have separate genus and species names
    {
      Genus_values = unlist(metadata_arrow_version[column_to_take_genus])
      check_Genus_names = strsplit(Genus_values, " ")
      if(all(unlist(lapply(check_Genus_names, length)) > 1)) print("Error in Genus name, troubleshooting necessary") 
      
      Species_values = unlist(metadata_arrow_version[column_to_take_species])
      check_Species_names = strsplit(Species_values, " ")
      if(all(unlist(lapply(check_Species_names, function(x) x[1]  )) == Genus_values)) 
      {
        Species_values  = unlist(lapply(check_Species_names, function(x) x[2]  ))
      }
      
      Genus_Species_Name = paste(unlist(metadata_arrow_version[column_to_take_genus]), unlist(metadata_arrow_version[column_to_take_species]), sep = " ")
      check_Genus_Species_names = strsplit(Genus_Species_Name,  " ")
      Genus_Species_Name = unlist(lapply(check_Genus_Species_names, function(x) paste(x[1], x[2], sep = " "))) # This ensures that we only have the Genus and species and not the third word which is usually a one letter word denoting the person that named the plant
      
    }
    
    if(length(column_to_take_genus) == 0 & length(column_to_take_species) != 0) # we have only a species column; this potentially means that 
    {
      Genus_Species_Name = unlist(metadata_arrow_version[column_to_take_species])
      check_Genus_Species_names = strsplit(Genus_Species_Name,  " ")
      if(all(unlist(lapply(check_Genus_Species_names, length)) > 1))
      {
      Genus_Species_Name = unlist(lapply(check_Genus_Species_names, function(x) paste(x[1], x[2], sep = " ")))
      }
    }
    
    if(length(column_to_take_genus) == 0 & length(column_to_take_species) == 0) # if we do not have species and genus columns
    {
      match_names<- paste(no_genus_or_species_first_list[order(-nchar(no_genus_or_species_first_list))], collapse = '|^') 
      test_names <- str_extract_all(names1, match_names, simplify= T) # this shows the exact name in the trait vector which matches with the column name. "" indicates no match.
      covariate_test_names = unique(test_names[test_names!= ""]) # these are the names in trait_vector that match (approximately) with the column names
      column_to_take_names = names1[test_names != ""]
      
      if(length(column_to_take_names) != 0)
      {
        if(length(column_to_take_names) > 1)
        {
          column_to_take_names =  trait_name_subset(column_to_take_names, no_genus_or_species_second_list)
        }
        
        Genus_Species_Name = unlist(metadata_arrow_version[column_to_take_names])
        check_Genus_Species_names = strsplit(Genus_Species_Name,  " ")
        Genus_Species_Name = unlist(lapply(check_Genus_Species_names, function(x) paste(x[1], x[2], sep = " ")))
      }else # If there is no name as well, as a last resort, check the USDA names
      {
        usda_trait_vector = tolower(USDA_symbol_vector)
        match_USDA<- paste(USDA_symbol_vector[order(-nchar(USDA_symbol_vector))], collapse = '|^') 
        test_USDA <- str_extract_all(names1, match_USDA, simplify= T) # this shows the exact name in the trait vector which matches with the column name. "" indicates no match.
        covariate_test_USDA = unique(test_USDA[test_USDA!= ""]) # these are the names in usda_trait_vector that match (approximately) with the column names
        
        if(length(covariate_test_USDA) != 0)
        {
          column_to_take = names1[test_USDA != ""]
          USDA_codes = unlist(metadata_arrow_version[column_to_take])
          Genus_Species_Name = unlist(lapply(USDA_codes, function(x)
          {
            ind1 = which(tolower(USDA_attribute_data$Symbol) == tolower(x))
            if(length(ind1) == 0)
            {
              out1 = NA
              }else if(length(ind1) == 1)
              {
                out1 = USDA_attribute_data$`Scientific Name with Author`[ind1]
              }else if (length(ind1) > 1)
              {
                out1 = USDA_attribute_data$`Scientific Name with Author`[ind1[1]] # arbitratily right now taking as first index where there are multiple matches with the USDA plant base
              }
            out1
          }))
          check_Genus_Species_names = sapply(Genus_Species_Name, function(x) {if(is.na(x)) {out = NA} else {out = strsplit(x,  " ")}}) 
          Genus_Species_Name =  unlist(lapply(check_Genus_Species_names, function(x) paste(x[1], x[2], sep = " ")))
        }else{Genus_Species_Name = rep(NA, nrow(metadata_arrow_version))}
      }
    }
    #Genus_Species_Name
    table_Genus_Species_Name = table(Genus_Species_Name)
    table_family = lapply(names(table_Genus_Species_Name), function(x){ return(tryCatch(tax_name(query = x, get = "family", db = "ncbi")$family, error=function(e) NA)) })
    table_family1 = unlist(table_family)
    family_names = unlist(lapply(Genus_Species_Name, function(x) { table_family1[which(x == names(table_Genus_Species_Name))]} ) )
    
    metadata_arrow_version$genus_species1 = Genus_Species_Name
    metadata_arrow_version$family1 = family_names
    write_parquet(metadata_arrow_version,  paste0(file.path(mainDir, dataset_name), "/", "metadata_updated.parquet"))
    
    metadata_traits_already_added = c(metadata_traits_already_added, "Genus_Species", "Family")
    metadata_traits_already_added = as.character(na.omit(metadata_traits_already_added))
    readr::write_lines(metadata_traits_already_added, traits_already_done_path)
    }
}

for(i in 1:length(datasets_to_take_for_trait)) {get_latin_genus(i)}
