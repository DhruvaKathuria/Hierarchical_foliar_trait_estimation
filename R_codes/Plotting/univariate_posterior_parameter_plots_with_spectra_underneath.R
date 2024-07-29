library(bayesplot)
library(ggplot2)
library(patchwork)
library(tidyverse)

source("R_codes/input_parameter_file.R")

color_vector = c("Carotenoid_Area" =  "#F8766D", 
                 "LMA" =   "#00BA38", 
                 "Nitrogen" =  "#619CFF")
vec_list <- posterior_parameters_full <- posterior_parameters_reduced <-   list()

for(trait_name1 in c("Carotenoid_Area", "LMA", "Nitrogen"))
{
  prj_mat <- readRDS(stringr :: str_glue("{data_folder}/data/code_output_data/projpred_files/{trait_name1}_posterior_parameter_matrix.rds"))
  prj_mat2 <- prj_mat[, -c(1, ncol(prj_mat))]
  wavelengths <- colnames(prj_mat2) |> 
    str_split_i("b_x", 2) |> 
    as.numeric()
  
  mean_error = colMeans(prj_mat2)
  q10_q90_error = apply(prj_mat2, 
                        MARGIN = 2, 
                         function(x) quantile(x, probs = c(0.1, 0.9)) ) 
  q10_q90_error <- t(q10_q90_error)
  colnames(q10_q90_error) <- c("q10", "q90")
  
  df_selected_posterior_parameters <- data.frame(wavelength = wavelengths,
                                                 mean = mean_error) |> 
    bind_cols(q10_q90_error)
  
  df_posterior_parameters <- data.frame(wavelength = 400:2400)
                                        #mean = NA,
                                        #q10 = NA,
                                        #q90 = NA)
  
  df_posterior_parameters <- dplyr :: left_join(df_posterior_parameters,
                                                df_selected_posterior_parameters,
                                                by = "wavelength")
  
  df_posterior_parameters |> ggplot(aes(x = wavelength, y = mean )) +
    geom_point() +
    geom_errorbar(aes(ymin = q10, 
                      ymax = q90)) 
  
  #create labels for plotting on x-axis
  vec <- sort(wavelengths)
  # Calculate the successive differences
  diffs <- diff(vec)
  # Find positions where the difference is less than 20
  remove_positions <- which(diffs < 20) + 1
  # Remove the second elements where the difference is less than 20
  vec <- vec[-remove_positions]
  vec_list[[trait_name1]] <- c(400, vec, 2400)
  
  
  posterior_parameters_full[[trait_name1]] <-  df_posterior_parameters |> ggplot(aes(x = wavelength, y = mean)) +
    geom_point(color = color_vector[trait_name1]) +
    geom_errorbar(aes(ymin = q10, 
                      ymax = q90),
                  color = color_vector[trait_name1]) +
    scale_x_continuous(breaks = vec_list[[trait_name1]]) +
    #coord_cartesian(ylim =  c(-10, 10)) +
    theme(axis.title.x = element_blank(),  # Remove x-axis title
          axis.title.y = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))
    
  
  posterior_parameters_reduced[[trait_name1]] <- posterior_parameters_full[[trait_name1]] +
    coord_cartesian(ylim =  c(-5, 5))
}

posterior_full_comp <- posterior_parameters_full[["Carotenoid_Area"]] / posterior_parameters_full[["LMA"]]/ posterior_parameters_full[["Nitrogen"]]
posterior_reduced_comp <- posterior_parameters_reduced[["Carotenoid_Area"]] / posterior_parameters_reduced[["LMA"]]/ posterior_parameters_reduced[["Nitrogen"]]

ggsave(filename = "paper_draft/figures/univariate_posterior_plots_without_underlying_spectra.png",
       posterior_reduced_comp,
       width = 7.5,
       height = 10,
       units = "in")
