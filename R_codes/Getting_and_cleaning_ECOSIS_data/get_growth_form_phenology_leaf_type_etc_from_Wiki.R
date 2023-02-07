library(XML)
library(arrow)
library(stringr)


###################################################Functions used in the code####################################################
get_wiki_data = function(scientific_name, wiki_section_number)
{
  wiki_url = paste0("https://en.wikipedia.org/wiki/" , sub(" ", "_", scientific_name))
  wiki_read = readLines(wiki_url, encoding = "UTF-8")
  parsed_wiki = htmlParse(wiki_read, encoding = "UTF-8")
  wiki_intro_text = parsed_wiki["//p"]
  xx = xmlValue(wiki_intro_text) # This is a specific section of Wikipedia page (the section numbers usually starts from 3 for our purposes)
}


function_for_classification <- function(scientific_name, wiki_section_text, classification_form) # For a particular Wikipedia section as denoted by wiki_text, the function matches the competing variable names (such as "tree", "shrub", etc) and see whose occurrences are the most, so as to classify the data like that
{
  # Get the desired section number from the Wikipedia page of a species (using scientific_name)
  string_count = sapply(classification_list[[classification_form]], function(x)  str_count(wiki_section_text, tolower(x))) # getting string counts of all the choices in the classification_form (classfication form can be "growth_form", "leaf_type", etc.)
  str_count = string_count[which(string_count == max(string_count))] # choosing the one that occurs the most
  if(length(str_count) == 0 | all(str_count == 0))
  {
    out1 = "Data unavailable"
  }else if(length(str_count) == 1)
  {
    out1 = names(string_count[which(string_count == max(string_count))])
  }else if(length(string_count) > 1 & all(str_count != 0))
  {
    out1 = "More than one matching set in Wiki, check manually"
  }
  out1
}

get_final_classification_for_species <- function(scientific_name, classification_form)
{
  wiki_text_whole = get_wiki_data(scientific_name)
  for(wiki_section_number in 1:length(wiki_text_whole))
  {
    out1 = function_for_classification(scientific_name, wiki_text_whole[wiki_section_number], classification_form)
    if(!(length(out1) == 0 | out1 == "Data unavailable" | out1 == "More than one matching set in Wiki, check manually")) break
  }
  out1
}

get_extra_metadata = function(index1, classification_form)
{
  x = datasets_to_take_for_trait[index1]
  metadata_arrow_version = read_parquet(paste0(file.path(mainDir, x), "/", "metadata_updated.parquet"))
  
  
  out = sapply(names(table(metadata_arrow_version$genus_species1)), function(scientific_name, classification_form1 = classification_form)
  {return(tryCatch(get_final_classification_for_species(scientific_name, classification_form1), error=function(e) "Error in extraction, check"))}
  )
  closeAllConnections()
  out
}
#########################################################################################################################



########################################################Implementation#########################################################
mainDir = "/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/NASA_proposal_3.1.2/ECOSIS_Data_download_Dhruva"  

classification_list = list("Growth_form" = c("shrub", "tree", "herbaceous", "grass", "vine"),
                           "Phenology" = c("deciduous", "evergreen"),
                           "Leaf" = c("broad", "needle")) # These are the various classifications that I am using right now. More classifications can be easily added here

trait_name1 = "LMA"
datasets_already_processed =  list.files(mainDir, recursive = T, pattern = "traits_already_done_for_metadata.txt")
indices_of_datasets_containing_trait_name =  unlist(lapply(datasets_already_processed, function(x)
{
  read_file1 = readr :: read_lines(file.path(mainDir, x))
  trait_name1 %in% read_file1
})
)

datasets_to_take_for_trait = unlist(lapply(datasets_already_processed[indices_of_datasets_containing_trait_name], function(x) strsplit(x, "/traits_already_done_for_metadata.txt" )[[1]])) 

#out = get_extra_metadata(1, "Growth_form")

classification_names = c("Growth_form", "Phenology", "Leaf")

for(ii in 1:length(classification_names))
{
  out1 = unlist(sapply(1:length(datasets_to_take_for_trait), function(i) get_extra_metadata(i, classification_names[ii] )))
  if (ii == 1) 
  {
    out_mat = cbind(names(out1), out1)
  }else if(ii !=1 & all(out_mat[,1] == names(out1))) 
    {
    out_mat = cbind(out_mat, out1)
    }else
      {
        print("Error, names dont match, cannot cbind") 
        break
      }
}

colnames(out_mat) = c("Scientific_name", classification_names)
out_mat1 = unique(out_mat)

out_mat2 = readr :: read_csv(paste0("/Users/dhruvakathuria/Documents/GitHub/Hierarchical_foliar_trait_estimation/R_codes/Species_data/", "Species_attribute_data_Dhruva.csv"))
out_mat3 = rbind(data.frame(out_mat1), out_mat2)
out_mat3 = unique(out_mat3)
readr:: write_csv(data.frame(out_mat3), paste0("/Users/dhruvakathuria/Documents/GitHub/Hierarchical_foliar_trait_estimation/R_codes/Species_data/", "Species_attribute_data_Dhruva.csv"))

