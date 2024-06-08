# Carotenoid metadata
## fresh-leaf-spectra-to-estimate-foliar-functional-traits-across-neon-domains

metadata_file <- readr :: read_csv("/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/NASA_proposal_3.1.2/ECOSIS_Data_download_Dhruva/fresh-leaf-spectra-to-estimate-foliar-functional-traits-across-neon-domains/metadata.csv")

car_df <- metadata_file |> 
  filter(!is.na(`Car_mass (mg/g)`))

study_sites <- unique(car_df$Site_Name)

## Nitrogen metadata

metadata_file_nitrogen_1 <- readr :: read_csv("/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/NASA_proposal_3.1.2/ECOSIS_Data_download_Dhruva/dried-leaf-spectra-to-estimate-foliar-functional-traits-over-neon-domains-in-eastern-united-states/metadata.csv")

nitrogen_df <- metadata_file_nitrogen_1 |> 
  filter(!is.na(Nitrogen))

study_sites_df1 <- unique(nitrogen_df$Site_Name)


metadata_file_nitrogen_2 <- readr :: read_csv("/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/Documents/GitHub/Hierarchical_foliar_trait_estimation/data/raw_data/dry-leaf-spectra-to-estimate-foliar-functional-traits-across-neon-domains/metadata.csv")

nitrogen_df_2 <- metadata_file_nitrogen_2 |> 
  filter(!is.na(`Nitrogen (mg/g)`))

study_sites_df2 <- unique(nitrogen_df_2$Site_Name)


# LMA metadata ------------------------------------------------------------
metadata_file_lma_1 <- readr :: read_csv("/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/Documents/GitHub/Hierarchical_foliar_trait_estimation/data/raw_data/fresh-leaf-spectra-to-estimate-leaf-traits-for-california-ecosystems/metadata.csv")
lma_df <- metadata_file_lma_1 |> 
  filter(!is.na(`Leaf mass per area (mg/cm2)`))

study_sites_df1 <- lma_df |> 
  select(`Location Name`) |> 
  unique() |> 
  unlist()

metadata_file_lma_2 <- readr :: read_csv("/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/Documents/GitHub/Hierarchical_foliar_trait_estimation/data/raw_data/leaf-spectra-of-4-plant-species-from-belgian-dune-grasslands---rosa-rugosa-from-the-northern-japan/metadata.csv")
lma_df2 <- metadata_file_lma_2 |> 
  filter(!is.na(`Leaf mass per area (mg/cm2)`))
locations2 <- lma_df2 |> 
  select(Latitude, Longitude, country) |> 
  mutate(Latitude = round(Latitude, 1), 
         Longitude = round(Longitude, 1)) |> 
  unique() 
#write_csv(locations2, "locations2.csv")

metadata_file_lma_3 <- readr :: read_csv("/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/Documents/GitHub/Hierarchical_foliar_trait_estimation/data/raw_data/nasa-fft-project-leaf-reflectance-morphology-and-biochemistry-for-northern-temperate-forests/metadata.csv")
lma_df3 <- metadata_file_lma_3 |> 
  filter(!is.na(`LMA_gDW_m2`))
locations3 <- lma_df3 |> 
  select(State, Latitude, Longitude, `Location Name`) |> 
  mutate(Latitude = round(Latitude, 1), 
         Longitude = round(Longitude, 1)) |> 
  group_by(`Location Name`)|> 
  slice(1) |> 
  ungroup()

locations4 <- locations3 |> 
  mutate(location_name = str_glue("{`Location Name`}, {State}"))

write_csv(locations4, "locations4.csv")


# getting the test and the composite files --------------------------------

cabo_dataset <- readr :: read_csv("/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/NASA_proposal_3.1.2/ECOSIS_Data_download_Dhruva/cabo-2018-2019-leaf-level-spectra/metadata.csv")

#for LMA

cabo_dataset_lma_locations <- cabo_dataset |> 
  filter(!is.na(LMA)) |> 
  select(latitude, longitude) |> 
  mutate(longitude = round(longitude, 1),
         latitude = round(latitude, 1)) |> 
  unique()

cabo_dataset_car_locations <- cabo_dataset |> 
  filter(!is.na(car_area)) |> 
  select(latitude, longitude) |> 
  mutate(longitude = round(longitude, 1),
         latitude = round(latitude, 1)) |> 
  unique()


cabo_dataset_nitrogen_locations <- cabo_dataset |> 
  filter(!is.na(Nmass)) |> 
  select(latitude, longitude) |> 
  mutate(longitude = round(longitude, 1),
         latitude = round(latitude, 1)) |> 
  unique()

test_dataset <- cabo_dataset_car_locations |> 
  bind_rows(cabo_dataset_lma_locations, cabo_dataset_nitrogen_locations) |> 
  mutate(location_name = NA,
         .before = latitude) |> 
  mutate(trait = c(rep("Carotenoid_Area", nrow(cabo_dataset_car_locations)),
                   rep("LMA", nrow(cabo_dataset_lma_locations)),
                   rep("Nitrogen", nrow(cabo_dataset_nitrogen_locations))
  ),
  .after = longitude) |>
  mutate(site_name = "cabo-2018-2019-leaf-level-spectra",
         data_split_type  = "test")

train_dataset <- readr :: read_csv("data/locations_of_study_sites_train.csv")
# Uncomment the below only if you export the .csv file from the .xlsx file
# train_dataset <- train_dataset[1:98,] |> 
#   select(location_name : site_name) |> 
#   mutate(data_split_type = "train")


total_dataset <- bind_rows(train_dataset,
                           test_dataset)

# readr :: write_csv(train_dataset,
#                    "data/locations_of_study_sites_train.csv")

readr :: write_csv(test_dataset,
                   "data/locations_of_study_sites_test.csv")

readr :: write_csv(total_dataset, 
                   "data/locations_of_study_sites.csv") 

cabo_dataset_lma<- cabo_dataset |> 
  filter(!is.na(car_area))
#cabo_dataset_lma |> select(growth.form) |> table()
functional_groups <- cabo_dataset_lma |> 
  #filter(growth.form == "tree") |> 
  select(functional.group) |> 
  table()

percent_functional_groups <- sort((functional_groups/sum(functional_groups)) *100,
                                  decreasing = T)
round(percent_functional_groups, 1)
