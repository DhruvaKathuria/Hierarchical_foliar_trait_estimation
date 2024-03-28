library(ggplot2)
library(dplyr)
library(tidyr)
prediction_files <- readr::read_csv(list.files(pattern = "full_model_plus_reduced_model",
                                               path = "/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/Github_data/Hierarchical_foliar_trait_estimation/data/code_output_data/predictions",
                                               full.names = T))


# full model comparison ---------------------------------------------------

prediction_files_full <- prediction_files |> 
 filter(model == "full model") |> 
  pivot_wider(names_from = prediction,
              values_from = value,
              ) 

prediction_files_reduced <- prediction_files |> 
  filter(model == "reduced model") |> 
  pivot_wider(names_from = prediction,
              values_from = value,
  ) 

ind_remove <- which(is.na(prediction_files_full$PLSR))

prediction_files_full_edit <- prediction_files_full[-ind_remove, ] |> 
  pivot_longer(cols = c(`bayesian (mean)`, PLSR, 
                        `bayesian 10th %tile`, `bayesian 90th %tile`),
               names_to = "prediction",
               values_to = "value")

prediction_files_reduced_edit <- prediction_files_reduced[-ind_remove, ] |> 
  pivot_longer(cols = c(`bayesian (mean)`, 
                        `bayesian 10th %tile`, `bayesian 90th %tile`),
               names_to = "prediction",
               values_to = "value")

prediction_files_edit <- prediction_files_full_edit |> bind_rows(prediction_files_reduced_edit)


summary_tibble_full <- prediction_files_edit |> 
  filter(model == "full model",
         prediction %in% c("bayesian (mean)", "PLSR")) |> 
  group_by(trait_name, prediction) |> 
  summarize(RMSE  = sqrt(mean((trait - value)^2)),
            R = cor(trait, value))

summary_tibble_full_unique <- summary_tibble_full  |> 
  distinct(trait_name, prediction, .keep_all = TRUE) |> 
  mutate (RMSE = round(RMSE, 2),
          R = round(R, 2))

ggplot(prediction_files_edit %>%
         filter(model == "full model", prediction %in% c("bayesian (mean)", "PLSR")), aes(x = value, y = trait, col = trait_name)) +
  geom_point(shape = 21, fill = "white") +
  geom_abline() +
  geom_text(data = summary_tibble_full_unique, aes(x = -Inf, y = Inf, label = paste("RMSE:", RMSE, "\nR:", R)), 
            hjust = 0, vjust = 1, check_overlap = TRUE) +
  facet_wrap(prediction ~ trait_name, scales = "free")

ggsave(filename = "paper_draft/figures/prediction_bayesian_plsr_comparison.png"
       #width  = 8,
       #height = 6,
       #units = "in"
       )



summary_tibble_reduced <- prediction_files_edit |> 
         filter(prediction %in% "bayesian (mean)") |> 
  group_by(trait_name, model) |> 
  summarize(RMSE  = sqrt(mean((trait - value)^2)),
            R = cor(trait, value))

summary_tibble_reduced_unique <- summary_tibble_reduced  |> 
  distinct(trait_name, model, .keep_all = TRUE) |> 
  mutate (RMSE = round(RMSE, 2),
          R = round(R, 2))

prediction_files_edit |> 
  filter(prediction %in% c("bayesian (mean)",
                           "bayesian 10th %tile",
                           "bayesian 90th %tile"
                           )) |> 
  pivot_wider(names_from = prediction,
              values_from = value) |> 
  ggplot(aes(x = `bayesian (mean)`, 
             y = trait,
             col = trait_name)) +
  geom_errorbar(aes(xmin = `bayesian 10th %tile`, 
                    xmax = `bayesian 90th %tile`)) +
  geom_point(shape = 21, fill = "white", alpha = 0.6) +
  geom_text(data = summary_tibble_reduced_unique, aes(x = -Inf, y = Inf, label = paste("RMSE:", RMSE, "\nR:", R)), 
            hjust = 0, vjust = 1, check_overlap = TRUE) +
  geom_abline() +
  facet_wrap(model ~ trait_name,
             scales = "free")

ggsave(filename = "paper_draft/figures/prediction_bayesian_full_model_reduced_model.png",
       #width  = 8,
       #height = 6,
       #units = "in"
       )

prediction_files_edit |> 
  filter(trait_name == "LMA",
         model == "reduced model") |> 
  pivot_wider(names_from = prediction,
              values_from = value) |> 
  ggplot(aes(x = `bayesian (mean)`, 
             y = trait)) +
  geom_errorbar(aes(xmin = `bayesian 10th %tile`, 
                    xmax = `bayesian 90th %tile`)) +
  geom_point( shape = 21, fill = "white") +
  geom_abline()
  
