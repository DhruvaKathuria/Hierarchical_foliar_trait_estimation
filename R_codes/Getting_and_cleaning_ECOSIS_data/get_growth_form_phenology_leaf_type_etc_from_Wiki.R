library(XML)
library(arrow)
library(stringr)

classification_list = list("Growth_form" = c("shrub", "tree", "herbaceous", "grass", "vine"),
                           "Phenology" = c("deciduous", "evergreen"),
                            "Leaf" = c("broad", "needle"))

function_for_classification <- function(wiki_text, classification_form)
{
  
  string_count = sapply(classification_list[[classification_form]], function(x)  str_count(wiki_text, tolower(x)))
  str_count = string_count[which(string_count == max(string_count))]
  if(length(str_count) == 0 | all(str_count == 0))
  {
    out1 = "Data unavailable"
  }else if(length(str_count) == 1)
  {
    out1 = names(string_count[which(string_count == max(string_count))])
  }else if(length(string_count) > 1)
  {
    out1 = "More than one matching set in Wiki, check manually"
  }
  out1
}

get_wiki_data = function(scientific_name)
{
  wiki_url = paste0("https://en.wikipedia.org/wiki/" , sub(" ", "_", scientific_name))
  wiki_read = readLines(wiki_url, encoding = "UTF-8")
  parsed_wiki = htmlParse(wiki_read, encoding = "UTF-8")
  wiki_intro_text = parsed_wiki["//p"]
  xx = xmlValue(wiki_intro_text[4]) # This is the description section of Wikipedia page
}

mainDir = "/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/NASA_proposal_3.1.2/ECOSIS_Data_download_Dhruva"  

trait_name1 = "Carotenoid_Area"
datasets_already_processed =  list.files(mainDir, recursive = T, pattern = "traits_already_done_for_metadata.txt")
indices_of_datasets_containing_trait_name =  unlist(lapply(datasets_already_processed, function(x)
{
  read_file1 = readr :: read_lines(file.path(mainDir, x))
  trait_name1 %in% read_file1
})
)

datasets_to_take_for_trait = unlist(lapply(datasets_already_processed[indices_of_datasets_containing_trait_name], function(x) strsplit(x, "/traits_already_done_for_metadata.txt" )[[1]])) 

get_extra_metadata = function(index1)
{
x = datasets_to_take_for_trait[index1]
metadata_arrow_version = read_parquet(paste0(file.path(mainDir, x), "/", "metadata_updated.parquet"))

extract_metadata = function(scientific_name)
{
  wiki_data = get_wiki_data(scientific_name)
  classification = function_for_classification(wiki_data, "Growth_form")
}

out = sapply(names(table(metadata_arrow_version$genus_species1)), function(scientific_name)
  {return(tryCatch(extract_metadata(scientific_name), error=function(e) "Error in extraction, check"))}
  )

}

out1 = get_extra_metadata(1)




