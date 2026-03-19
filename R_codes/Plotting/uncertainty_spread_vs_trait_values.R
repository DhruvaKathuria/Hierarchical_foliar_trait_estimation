library(tidyverse)

# Load data
df <- read_csv("/Users/dkathuri/Downloads/prediction_file_full_model_plus_reduced_model_Carotenoid_Area.csv")

df_bayes <- df %>%
  filter(prediction %in% c("bayesian (mean)",
                          "bayesian 10th %tile",
                          "bayesian 90th %tile"))

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

ggplot(df_summary,
       aes(x = trait, y = spread)) +
  
  geom_point(alpha = 0.6,
             color = "grey") +
  
  # Optional smooth trend
  geom_smooth(se = FALSE, method = "loess") +
  facet_wrap(. ~ model) +
  
  labs(
    x = "Observed Trait Value",
    y = "Uncertainty (90th - 10th percentile)",
    title = "Prediction Uncertainty vs Observed Trait"
  ) +
  
  theme_minimal()


ggplot(df_summary,
       aes(x = trait)) +
  
  geom_linerange(
    aes(
      ymin = `bayesian 10th %tile`,
      ymax = `bayesian 90th %tile`
    ),
    color = "grey",
    alpha = 0.3
  ) +
  
  geom_point(
    aes(y = `bayesian (mean)`),
    color = "grey",
    size = 1.5
    
  ) +
  facet_wrap(. ~ model) +
  labs(
    x = "Observed Trait",
    y = "Prediction",
    title = "Bayesian Predictions with 10–90% Interval"
  ) +
  
  theme_minimal()
