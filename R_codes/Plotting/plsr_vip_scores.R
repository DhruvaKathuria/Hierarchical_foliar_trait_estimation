#plotting plsr coefficients and vip scores
library(ggplot2)
library(patchwork)
# Data
trait_name1 = "LMA"
color_vector = c("Carotenoid_Area" =  "#F8766D", 
                 "LMA" =   "#00BA38", 
                 "Nitrogen" =  "#619CFF")
data_folder = "/Users/dkathuri/Downloads/Github_data/Hierarchical_foliar_trait_estimation"

trait_names_for_plots  = c("LMA" = "LMA",
                           "Carotenoid_Area" = "Carotenoid",
                           "Nitrogen" = "Nitrogen")
plsr_df = read_csv(file.path(data_folder, "data", "code_output_data", "plsr_files", "plsr_regression_coefs_plus_vip_scores.csv"))

plsr_vip_scores = list()

for(trait_name1 in c("Carotenoid_Area", "LMA", "Nitrogen"))
{

  plsr_vip_scores[[trait_name1]] <- plsr_df |> 
    filter(trait == trait_names_for_plots[trait_name1]) |> 
    ggplot(aes(x = wavelengths, y = vip)) +
    geom_line(color = color_vector[trait_name1]) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    scale_x_continuous(
      limits = c(400, 2400),
      breaks = seq(400, 2400, by = 200)  # optional, cleaner ticks
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text = element_blank(),
          legend.position = "none") +
    xlab("")
  
}
plsr_vip_scores_comp_plot <- plsr_vip_scores[[1]] / plsr_vip_scores[[2]] /plsr_vip_scores[[3]]
plsr_vip_scores_comp_plot

ggsave(filename = "paper_draft/figures/plsr_vip_scores.png",
       plsr_vip_scores_comp_plot,
       width = 7.5,
       height = 10,
       units = "in")


# regression coefficients
plsr_regression_coefs = list()

for(trait_name1 in c("Carotenoid_Area", "LMA", "Nitrogen"))
{
  
  plsr_regression_coefs[[trait_name1]] <- plsr_df |> 
    filter(trait == trait_names_for_plots[trait_name1]) |> 
    ggplot(aes(x = wavelengths, y = coefficients)) +
    geom_line(color = color_vector[trait_name1]) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    scale_x_continuous(
      limits = c(400, 2400),
      breaks = seq(400, 2400, by = 200)  # optional, cleaner ticks
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text = element_blank(),
          legend.position = "none") +
    xlab("")
  
}

plsr_regression_coefs_comp_plot <- plsr_regression_coefs[[1]] / plsr_regression_coefs[[2]] /plsr_regression_coefs[[3]]
plsr_regression_coefs_comp_plot

ggsave(filename = "paper_draft/figures/plsr_regression_coefs.png",
       plsr_regression_coefs_comp_plot,
       width = 7.5,
       height = 10,
       units = "in")
