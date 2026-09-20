{
  library(httr)
  library(jsonlite)
  library(stringr)
  library(parallel)
}


# Function list start -----------------------------------------------------

data_download_function = function(dataset_name)
{
  mainDir <-  "data/raw_data"
  data1 <-  dataset_name
  
  ## though not very optimal, we do two downloads, one with spectra only, the other with spectra + metadata. this helps us to separate out the metadata and spectra dataframes
  path_only_spectra <-  paste0("https://ecosis.org/api/package/", 
                               data1, 
                               "/export?metadata=false")
  path_trait_plus_spectra = paste0("https://ecosis.org/api/package/", 
                                   data1, 
                                   "/export?metadata=true")
  
  r2_only_spectra <- GET(url = path_only_spectra)
  r2_trait_plus_spectra = GET(url = path_trait_plus_spectra)
  if(status_code(r2_trait_plus_spectra) == 200) # 200 denotes success download
  {
    r3_spectra_only <-  readr:: read_csv(content(r2_only_spectra), show_col_types = FALSE)
    r3_trait_plus_spectra <-  readr:: read_csv(content(r2_trait_plus_spectra), show_col_types = FALSE)
    r3_trait_only <-  r3_trait_plus_spectra[, -which(names(r3_trait_plus_spectra) %in% names(r3_spectra_only))]
    
    subDir <-  dataset_name
    ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)
    readr::write_csv(r3_trait_only, file = paste0(file.path(mainDir, subDir), "/", "metadata.csv"))
    readr::write_csv(r3_spectra_only, file = paste0(file.path(mainDir, subDir), "/", "spectra.csv"))
    out <-  colnames(r3_trait_only)
    
  }else{out = NA}
  out
}

path <-  "https://ecosis.org/api/package/search?text=&filters=[]&start=0&stop=200" # arbitratily set stop to a high value so that it takes in all the spectra data, start and stop refer to the dataset number
r <- GET(url = path)
if(status_code(r) == 200) # 200 denotes success download
  {r <- content(r, as = "text", encoding = "UTF-8")
  df <- fromJSON(r,flatten = TRUE)} # df is the dataframe that contains the metadata of the measurements

##filtering datasets using leaf measurements
indices_leaf_measurement <-  sapply(df[['items']][['Target Type']], 
                                    function(x)any(x == "leaf")) ## we can also do x == "canopy"
datasets_to_take <-  df$items$ecosis.package_name[indices_leaf_measurement] # getting the name of the datasets we need to focus on, here we are only subsetting the leaf measurements



mainDir = "data/raw_data"
datasets_downloaded = list.files(mainDir)
datasets_left_to_download = setdiff(datasets_to_take, datasets_downloaded)

#####################################################################################
#####Uncomment below to implement the parallel implementation of the code

#detectCores()
max_clusters <- 9
cl <- makeCluster(min(length(datasets_left_to_download), 
                      max_clusters))     # set the number of processor cores
#clusterExport(cl, c()) # export the objects to all cluster
clusterEvalQ(cl, {library(httr)
  library(jsonlite)
  library(readr)
}) # run the following commands in all the clusters

get_colnames = parLapply(cl, datasets_left_to_download, data_download_function)
stopCluster(cl)

#####################################################################################
