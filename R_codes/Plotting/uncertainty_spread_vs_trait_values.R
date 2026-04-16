library(tidyverse)
data_folder = "/Users/dkathuri/Downloads/Github_data/Hierarchical_foliar_trait_estimation"
# Load data
df <- read_csv(list.files(pattern = "prediction_file_full_model_plus_reduced_model_", 
                          path = file.path(data_folder, "data", "code_output_data", "predictions"),
                          full.names = T))

df_bayes <- df %>%
  filter(prediction %in% c("bayesian (mean)",
                          "bayesian 10th %tile",
                          "bayesian 90th %tile")) |>
  filter(model == "reduced model")

# 🔑 FIX: include `trait` in grouping
df_wide <- df_bayes %>%
  group_by(genus_species1, trait_name, trait, model, prediction) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = prediction, values_from = value)

# Compute spread
df_summary <- df_wide %>%
  mutate(
    spread = `bayesian 90th %tile` - `bayesian 10th %tile`
  )

uncertainty_plot = ggplot(df_summary,
       aes(x = trait, y = spread, color = trait_name)) +
  
  geom_point(alpha = 0.6) +
  
  # Optional smooth trend
  #geom_smooth(se = FALSE, method = "loess") +
  
  #facet_grid(trait_name ~ model, scales = "free") +
  #facet_wrap(. ~ model, scales = "free_y") +
  facet_wrap( .~ trait_name, scales = "free") +
  
  labs(
    x = "Observed Trait Value",
    y = "Uncertainty (90th - 10th percentile)",
    title = "",
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_blank()
  )

uncertainty_plot

ggsave(filename = "paper_draft/figures/prediction_uncertainty_vs_observed_traits.png",
       uncertainty_plot,
       height = 4,
       width = 7,
       units = "in")
