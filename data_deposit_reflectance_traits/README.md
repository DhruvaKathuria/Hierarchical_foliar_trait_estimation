---
mainfont: "Arial Unicode MS"
header-includes:
  - \usepackage{xurl}
---

# Data and Code Deposit — "A Bayesian Approach to Hyperspectral Leaf Trait Prediction with Uncertainty Quantification"

This deposit contains the reflectance and trait data, and the R code used to obtain, clean, and
model it, for the leaf-trait–spectra analyses reported in the manuscript. Data files retain the
genus/species, family, growth form, phenology, and leaf-type columns produced during data cleaning
(see `R_​codes/​Getting_​and_​cleaning_​ECOSIS_​data/​03_​adding_​genus_​species_​info.R` and
`get_​growth_​form_​phenology_​leaf_​type_​etc_​from_​Wiki.R` in the project GitHub repository <https://github.com/DhruvaKathuria/Hierarchical_foliar_trait_estimation>) and used
by the outlier-filtering step in the regression pipeline (`filter_​out_​error_​groups()`); these
columns are not the subject of the manuscript's analyses but are documented below for completeness.

## Data Access

All spectral reflectance and leaf trait data were downloaded from the Ecological Spectral
Information System (EcoSIS, https://ecosis.org) via its public API using the R script
`R_​codes/​Getting_​and_​cleaning_​ECOSIS_​data/​01_​get_​data_​ecosis.R`, on the download dates recorded in
the ECOSIS package metadata for each dataset. See `datasets_​used.csv` for
the exact EcoSIS package (dataset) URLs and DOIs used in this study, organized by trait.

The Canadian Airborne Biodiversity Observatory (CABO) test dataset is cited as:
Kothari, S., et al. (2023) — see manuscript reference list and `datasets_​used.csv` entry
`cabo-2018-2019-leaf-level-spectra` for the EcoSIS record.

## File-Level Metadata

| File / Folder | Description |
|---|---|
| `data/​datasets_​used.csv` | List of the final EcoSIS datasets used in the analysis, one row per (trait, dataset) pair, with a link to the EcoSIS record for that dataset. |
| `data/​data_​train_​Carotenoid_​Area.csv` | Example training data (spectra + trait value + species/growth-form metadata) used to fit the Carotenoid Area (Car_A) models. |
| `data/​data_​test_​Carotenoid_​Area.csv` | Held-out CABO test data (spectra + trait value + species/growth-form metadata) used to evaluate the Carotenoid Area (Car_A) models. |
| `data/​Species_​data/​Species_​attribute_​data.csv` | Lookup table mapping each species' scientific name to a growth form, phenology, and leaf-type classification (compiled with the assistance of a large language model, cross-checked against Wikipedia). |
| `R_​codes/​Regression_​algorithms/​supervised_​pc_​and_​raw_​spectra_​bayesian.R` | **Main model-fitting script.** Fits the full Bayesian prior regression model of a leaf trait on the reflectance spectrum. Sources `input_​parameter_​file.R` for global settings; for the packaged example it reads `data_​train_​Carotenoid_​Area.csv` directly (the line sourcing `data_​preprocessing_​for_​algorithms.R`, which rebuilds the training data from all raw EcoSIS datasets, is commented out and only needed if re-running the full multi-dataset pipeline). |
| `R_​codes/​Regression_​algorithms/​covariate_​reduction_​of_​full_​Baysian_​model.R` | **Main wavelength-selection script.** Performs projection predictive variable selection on a previously fitted `brms` model (loaded from an `.rds` file — either produced locally by `supervised_​pc_​and_​raw_​spectra_​bayesian.R`, or the corresponding example object hosted at the Zenodo repository linked in the Open Research Statement above). Runs a fast approximate `varsel()` pass (for exploratory speed, not used in the paper) followed by the full `cv_​varsel()` k-fold (K = 5) forward-search cross-validation used in the paper, identifying the reduced set of wavelengths that best predict the trait, and plots the cross-validated RMSE as a function of the number of wavelengths retained. |
| `R_​codes/​Getting_​and_​cleaning_​ECOSIS_​data/​01_​get_​data_​ecosis.R` | Queries the EcoSIS API for the first 200 datasets(the number 200 is arbitrary, and was chosen since at the time of download, none of the analyzed trait had more than 200 datasets) registered in EcoSIS, filters to datasets whose `Target Type` metadata field includes `"leaf"`(`leaf` denotes leaf spectra as opposed to `canopy` spectra), and downloads the spectra and trait metadata for each retained dataset. |
| `R_​codes/Getting_​and_​cleaning_​ECOSIS_​data/02_​create_​parquet_​metadata.R` | Standardizes each dataset's raw trait metadata: assigns a `sample_​id`, records whether the spectral values are reflectance-only, and maps each trait name to its corresponding raw column name and units, based on `trait_​and_​sample_​id_​Database_​for_​ECOSIS_​Data.R`. |
| `R_​codes/Getting_​and_​cleaning_​ECOSIS_​data/Creating_​folder_​for_​further_​data_​cleaning_​and moving_​data_​there.R` | Helper function that creates a subfolder and moves the raw downloaded `spectra.csv`/​`metadata.csv` files into it prior to per-dataset cleaning. |
| `R_​codes/Getting_​and_​cleaning_​ECOSIS_​data/Further_​data_​cleaning/*.R` | Dataset-specific cleaning scripts. Each script splits a dataset's raw combined file into `spectra.csv`/​`metadata.csv` and/​or filters records to reflectance-only measurements (400–2400 nm, 1 nm sampling, 2001 continuous bands), as required for that dataset. |
| `R_​codes/supporting_​R_​functions/getting_​traits_​data.R` | Functions used by `02_​create_​parquet_​metadata.R` to match a trait name (e.g., "Nitrogen") to the corresponding raw column name in each dataset's metadata file. |
| `R_​codes/supporting_​R_​functions/trait_​and_​sample_​id_​Database_​for_​ECOSIS_​Data.R` | Lookup tables of trait name synonyms, sample ID column names, units, EcoSIS paper links, and spectral-instrument metadata, used across all cleaning scripts. |
| `R_​codes/supporting_​R_​functions/Steps_​to_​Add_​Metadata_​for_​a_​Trait_​in_​a_​dataset.R` | Documentation script (not executed as part of the pipeline) describing the manual workflow followed to add a new trait/​dataset combination to the standardized metadata. |
| `R_​codes/input_​parameter_​file.R` | Global parameter file sourced by the regression scripts below. Sets the trait-independent analysis options: the held-out test site (`site_​name1`, the CABO dataset), the grouping variable for outlier filtering (`group_​variable`), whether to fit a hierarchical model (`hierarchical`), which prediction algorithm/​representation to use (`prediction_​algorithm`: `"raw_​spectra"`, `"supervised_​pc"`, or `"naive_​PC"`), whether to also fit PLSR for comparison (`PLSR_​implementation`), whether inputs/​outputs are standardized (`scale_​x`, `scale_​y`), and per-trait bookkeeping (`date_​vector`, `nsel_​vector`) used to locate previously saved model objects. |
| `R_​codes/Regression_​algorithms/data_​preprocessing_​for_​algorithms.R` | Supporting script that sources `ECOSIS_​Implementation_​file_​for_​Bayesian_​ML.R` to assemble a combined trait + spectra data frame across all raw EcoSIS datasets for a trait, removes known duplicate/​overlapping datasets and duplicate observations, defines `filter_​vector_​list` (the set of allowed values used by `filter_​out_​error_​groups()` to drop observations with unrecognized/​erroneous group labels, including the `genus_​species1`, `family1`, `Growth_​form`, `Phenology`, `Leaf`, and `leaf_​classification` groupings), splits the data into training and CABO test sets, and standardizes (scales/​centers) the spectral predictors and trait response before handing them to the Bayesian model script. |
| `R_​codes/Regression_​algorithms/ECOSIS_​Implementation_​file_​for_​Bayesian_​ML.R` | Supporting script (sourced by `data_​preprocessing_​for_​algorithms.R`) that, for a given trait, reads and filters each raw dataset's `spectra.csv` to the common 400–2400 nm range, corrects any reflectance values mistakenly left on a 0–100 (rather than 0–1) scale, and reads each dataset's `metadata_​updated.parquet` to extract the trait values (converted to standardized units) and instrument metadata. It merges in `data/Species_​data/Species_​attribute_​data.csv` to attach `Growth_​form`, `Phenology`, `Leaf`, and `leaf_​classification` labels (by scientific name) to each observation, producing a single combined data frame. Like `data_​preprocessing_​for_​algorithms.R`, this script requires the full `data/raw_​data/` folder to run and is included for reference/​completeness rather than as part of the packaged single-trait example. |

## Column-Level Metadata

### `datasets_​used.csv`
| Column | Type | Description |
|---|---|---|
| `trait` | categorical | Leaf trait: `Carotenoid_​Area`, `Nitrogen`, or `LMA`. |
| `site_​name` | categorical | EcoSIS package (dataset) identifier/​slug. |
| `url` | text | URL to the EcoSIS record for that dataset (`https://ecosis.org/package/<site_​name>`). |


### `data_​train_​Carotenoid_​Area.csv`, `data_​test_​Carotenoid_​Area.csv`
| Column | Type | Description |
|---|---|---|
| `genus_​species1` | categorical | Genus and species (scientific name) of the sampled plant, extracted from each dataset's metadata during cleaning; `NA` where not resolvable. |
| `family1` | categorical | Taxonomic family, looked up from `genus_​species1`; `NA` where not resolvable. |
| `growth_​form` | categorical | Growth form of the species (e.g., tree, shrub, herbaceous, grass, vine), from `data/Species_​data/Species_​attribute_​data.csv`; `NA` where not available. |
| `phenology` | categorical | Leaf phenology of the species (deciduous or evergreen), from `data/Species_​data/Species_​attribute_​data.csv`; `NA` where not available. |
| `leaf` | categorical | Leaf morphology of the species (broad or needle), from `data/Species_​data/Species_​attribute_​data.csv`; `NA` where not available. |
| `leaf_​classification` | categorical | Combined leaf/​growth-form classification (`broadleaf`, `needle`, `grass`, `herbaceous`); `NA` where not available. |
| `manufacturer` | categorical | Manufacturer of the spectroradiometer used to collect the reflectance spectrum. |
| `model` | categorical | Instrument model of the spectroradiometer used. |
| `trait` | numeric | Measured Carotenoid Area (Car_A) value, in µg cm⁻². |
| `site_​name` | categorical | EcoSIS package (dataset) identifier/​slug the observation was drawn from. |
| `x400` … `x2400` | numeric | Percent leaf reflectance at each 1 nm wavelength band from 400 nm to 2400 nm (2001 columns total). Column name `x<wavelength>` gives the band center in nanometers. |

### `data/Species_​data/Species_​attribute_​data.csv`
| Column | Type | Description |
|---|---|---|
| `Scientific_​name` | categorical | Genus and species (scientific name) |
| `Growth_​form` | categorical | Growth form of the species (tree, shrub, herbaceous, grass, or vine). |
| `Phenology` | categorical | Leaf phenology of the species (deciduous or evergreen). |
| `Leaf` | categorical | Leaf morphology of the species (broad or needle). |
| `leaf_​classification` | categorical | Combined leaf/​growth-form classification (broadleaf, needle, grass, or herbaceous). |

Missing/​null values in all files are encoded as `NA`.
The urls and the download dates for each dataset is given in 'data/datasets_used.csv'. 
