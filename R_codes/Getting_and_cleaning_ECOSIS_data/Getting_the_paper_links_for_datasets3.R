Github_dir = "/Users/dhruvakathuria/Documents/GitHub/Hierarchical_foliar_trait_estimation/"
datasets_already_processed =  list.files(mainDir, recursive = T, pattern = "traits_already_done_for_metadata.txt")
datasets_to_take_for_trait = unlist(lapply(datasets_already_processed, function(x) strsplit(x, "/traits_already_done_for_metadata.txt" )[[1]])) 
xx = vector()

for(i in 1:length(datasets_to_take_for_trait))
{
  x = datasets_to_take_for_trait[i]
  metadata_arrow_version = read_parquet(paste0(file.path(mainDir, x), "/", "metadata_updated.parquet"))
  xx = c(xx, attributes(metadata_arrow_version)$Paper_links)
}

xx = xx[!is.na(xx)]

