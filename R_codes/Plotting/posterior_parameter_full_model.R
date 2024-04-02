# uncomment to plot fig-posterior_parameters_full_model

source("R_codes/input_parameter_file.R")

# in folder code data/code_output_data. brms
# files are saved using supervised_pc_and....R
# for personal macbooks

# Analysis for full model -------------------------------------------------

brms_normal <- readRDS(paste0(data_folder,  
                              "/data/code_output_data/brms_full_model_files/brms_object_",
                              trait_name1, 
                              "_",
                              prediction_algorithm,
                              "_",
                              date_for_brms_file,
                              ".rds"))

# fixed effects -----------------------------------------------------------

posterior_pars <- fixef(brms_normal, summary = F)
spectra_regression_means <- colMeans(posterior_pars)
indices_top_five <- order(abs(spectra_regression_means), decreasing = T)[1:4]
wavelength_names <- names(spectra_regression_means)[indices_top_five]

wavelength_names <- unlist(lapply(wavelength_names, function(x) {str_split_1(x, "x")[2]}))

wavelength_names <- paste(wavelength_names, "nm", sep = " ")

posterior_pars_max <- posterior_pars[, indices_top_five] |>
  as_tibble()

colnames(posterior_pars_max) = wavelength_names

posterior_pars_max |>
  pivot_longer(cols = everything(),
               names_to = "Wavelength",
               values_to = "Regression Coefficient") |>
 ggplot2:: ggplot(aes(x = `Regression Coefficient`)) +
  geom_histogram(color = "red") +
  facet_wrap(~Wavelength) +
  coord_cartesian(ylim = c(0, 5000))

ggsave(filename = "paper_draft/figures/parameter_posterior_histogram.png",
       height = 5,
       width = 5,
       units = "in")
