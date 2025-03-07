library(bayesplot)
library(ggplot2)
library(patchwork)
library(tidyverse)
trait_name1 = "Carotenoid_Area"
source("R_codes/input_parameter_file.R")
color_scheme_set("pink")

prj_mat <- readRDS(stringr :: str_glue("{data_folder}/data/code_output_data/projpred_files/{trait_name1}_posterior_parameter_matrix.rds"))
new_colnames <- colnames(prj_mat)[-ncol(prj_mat)] |> 
  str_split_i("b_", 2)
  
colnames(prj_mat)[-ncol(prj_mat)] <- new_colnames


# bivariate plots
bayesplot_theme_set(ggplot2::theme_bw())
mcmc_pairs1 <- mcmc_pairs(prj_mat, pars = c(colnames(prj_mat)[2:6], "sigma"),
           off_diag_args = list(size = 1.5))

plot(mcmc_pairs1)
ggsave(filename = str_glue("paper_draft/figures/bivariate_plots_for_{trait_name1}.png"),
       plot = mcmc_pairs1,
       width = 8.5,
       height = 8.5)

# marginal plots
mcmc_intervals1 <- mcmc_intervals(prj_mat,
               point_size = 3) +
  ggplot2::coord_cartesian(xlim = c(-3, 3))

ggsave(filename = str_glue("paper_draft/figures/marginal_plots_for_{trait_name1}.png"),
       plot = mcmc_intervals1)

mcmc_pairs1 + mcmc_intervals1
