library(brms)
library(caret)
library(tidyverse)
library(ggpmisc)
library(glmnetUtils)
library(lme4)

###########################Function List##################
#Scaling
scale1  = function(data_frame){matrix1 = as.matrix(data_frame); out = data.frame(scale(matrix1)) }

#The reason I am writing this code is so that I can choose the Principal components to take for regression in my hierarchical model. I first do PCA on the entire dataset, split the data into train/test and apply cross-validated lasso on the train dataset to get the PCs important for regression. This is important because we can have low varaince PCs which can be important for the response variable.
spectra_names = 400:2400 |> as.character()

#The below list is formed to do form groups for the hierarchical type analysis
#Getting data ready for analysis
#Source the file below to get the input data matrices and the output trait
source("/Users/dhruvakathuria/Documents/GitHub/Hierarchical_foliar_trait_estimation/R_codes/Regression_algorithms/ECOSIS_Implementation_file_for_Bayesian_ML.R")

filter_vector_list = list("Growth_form" = c("tree", "shrub", "herbaceous", "grass", "vine"), 
                          "Leaf" = c("broad", "needle"), 
                          "Phenology" = c("deciduous", "evergreen"),
                          "genus_species1" = unique(trait_and_metadata_dataframe$genus_species1[!(trait_and_metadata_dataframe$genus_species1 %in% "NA NA")]),
                          "family1" = unique(trait_and_metadata_dataframe$family1[!is.na(trait_and_metadata_dataframe$family1)]))


#We are first formulating the data frame which will be used for analysis
data_frame_out = trait_and_metadata_dataframe |> bind_cols(spectra_df) |> 
  distinct(trait, .data[["400"]], .data[["500"]], .data[["600"]], .data[["700"]], .data[["800"]], .data[["900"]],  .keep_all = TRUE) |> 
  filter(!is.na(trait)) |> 
  mutate(trait = as.numeric(trait)) |>
  distinct(trait, site_name, .keep_all = TRUE) |>
  filter(!(site_name %in% c("dried-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests",
                            "fresh-leaf-spectra-to-estimate-leaf-morphology-and-biochemistry-for-northern-temperate-forests",
                            "ground-leaf-cabo-spectra-from-herbarium-project",
                            "fresh-leaf-cabo-spectra-from-herbarium-project"
                            )))

## getting the principal_components of the data_frame

PC_values_scaled <-  data_frame_out |> select(all_of(spectra_names)) |> prcomp(center = T, scale = T) |> pluck("x") |> scale1()
data_frame_PC = data_frame_out |> select(-all_of(spectra_names)) |> mutate(trait = (trait - mean(trait)) / sd(trait)) |> bind_cols(PC_values_scaled)


# Now we split the data into train and test so that we can apply lasso on the train and get the PCs
# Below function is from ECOSIS_Implementation_file_for_Bayesian_ML.R
indices_subset = data_frame_PC |>  get_indices_subset_function(filtering_type = "Global", test_site = "None", fraction_split = 0.7)
data_train = data_frame_PC |>  slice(indices_subset)
data_test = data_frame_PC |>  slice(-indices_subset)

# Now we apply the lasso on the train dataset and get the PCs which are most important according to lasso
#fla = paste("trait ~", paste(spectra_names, collapse = "+"))
formula_PC =  paste("trait ~", paste(paste0("PC", 1:2001), collapse = "+"))
cv.out <-  cv.glmnet(formula = as.formula(formula_PC) , data = data_train, alpha =1)
abs_coefficient_values = cv.out |> coef(s = 'lambda.min') |> as.matrix() |> abs() |> as.data.frame() |> rename("abs_coef" = "s1") |>  arrange(desc(abs_coef))

## I do the following steps to see how many PCs approximately I should take for this. Turns out that 100 is a good number. Only uncomment if I want to do the analysis again
# RMSE1 = Cor1 = vector()
# for(i in seq(from = 5, to = 400, by = 10)  )
# {
#   data_frame_choose = data_train |> select(all_of(c("trait", names(cc[1:i])))) 
#   Y_predict = data_frame_choose |> lm(formula = (trait~.)) |> predict(data_test)
#   
#   
#   RMSE1[i] = Y_predict |> RMSE_function(data_test$trait)
#   Cor1[i] = Y_predict |> cor_function(data_test$trait)
# }


#I have decided to keep 100 PCs and I will now apply the linear regresssion model to see how well the predictions are doing on the train data 
n_PC = 100
abs_coefficient_values = abs_coefficient_values |> filter(rownames(abs_coefficient_values) != "(Intercept)")
data_train_PC = data_train |>  select(c("trait", any_of(rownames(abs_coefficient_values)[1:n_PC]) ))
Y_predict_train = data_train_PC |> lm(formula = (trait~.)) |> predict()
data_train_prediction  =  data_train_PC |> 
  mutate(predict1 = Y_predict_train) |> 
  mutate(difference = predict1 - trait) |> 
  bind_cols(select(data_train,  -c("trait", paste0("PC", 1:2001)))) 
  
#I then plot both the predictions and also the outlieres to see where the differences are happening from the global model  
data_train_prediction |> 
  ggplot(aes(predict1, trait, color = site_name)) +
  geom_point() + 
  geom_abline()
# data_train_outliers |> ggplot(aes(trait, difference, color = Growth_form)) +
#   geom_point() + geom_hline(yintercept = 0)

#Now I want to analyze the residuals to see if we have any group specific changes in beta coefficients of the principal components. This will help us to look at which PCs to take in hierarchical approach
group_variable = "family1"

PC_to_take = rownames(abs_coefficient_values)[1:n_PC]
data_train_for_hierarchical_analysis = data_train_prediction  |> filter(.data[[group_variable]] %in% filter_vector_list[[group_variable]] ) |> na.omit() |> distinct()
for(i in 1:20)
{
  p1 = data_train_for_hierarchical_analysis |> 
    ggplot(aes(x = get(PC_to_take[i]), y = difference)) + 
    geom_point() + 
    xlab(PC_to_take[i]) + 
    ylab("Residual") + 
    geom_vline(xintercept = 0) +
    facet_wrap(~ Growth_form, nrow = 2) 
  print(p1)
}
data_train_outliers = data_train_for_hierarchical_analysis |> filter(abs(difference) > 1) |> select(-any_of(rownames(abs_coefficient_values)[1:n_PC]))   #filter(site_name == "cabo-2018-2019-leaf-level-spectra")

# I find that only "PC1, PC2 and PC5 to shows some kind of change with residuals which have very high values
#Now lets apply the lme4 package to see if mixed effects actually help in improving predictions 
fla_mixed = paste("trait ~ 1 + ", paste(PC_to_take[1:10], collapse = "+"), paste("+(", "1 + PC1 + PC2 + PC3", collapse = "+"), paste("|", group_variable, ")", sep = ""), sep = "")
#tt = "trait ~ 1 + PC1+PC5+PC4+PC11+PC7+PC2+PC13+PC12+PC15+PC6+PC26+PC30+PC23+PC22+PC31+PC50+PC17+PC62+PC54+PC60+PC35+PC34+PC52+PC20+PC46+PC3+PC137+PC14+PC8+PC568+PC627+PC835+PC33+PC415+PC848+PC109+PC44+PC155+PC106+PC752+PC9+PC135+PC516+PC499+PC1207+PC284+PC36+PC661+PC651+PC719+PC946+PC582+PC638+PC283+PC427+PC146+PC306+PC515+PC212+PC553+PC112+PC180+PC463+PC1131+PC89+PC393+PC10+PC145+PC117+PC1627+PC532+PC1387+PC175+PC558+PC1151+PC566+PC201+PC880+PC1421+PC1471+PC1105+PC56+PC101+PC179+PC962+PC1396+PC600+PC1488+PC565+PC510+PC287+PC525+PC1810+PC219+PC803+PC474+PC425+PC21+PC743"
#fla_mixed = paste(tt, paste("+(", "1 + PC1 + PC11 + PC2", collapse = "+"), paste("|", group_variable, ")", sep = ""), sep = "")


#Nitrogen: paste("+(", "1 + PC19 + PC2 + PC9 +  PC4 + PC11", collapse = "+")
#LMA: paste("+(", "1 + PC5 + PC10 + PC4 +  PC1 + PC11", collapse = "+")
#Carotenoid_Area: paste("+(", "1 + PC1 + PC11 + PC7 + PC13 + PC2", collapse = "+")

fla_fixed  = paste("trait ~ 1 + ", paste(PC_to_take[1:10], collapse = "+"), sep = "")
model = lm(formula = as.formula(fla_fixed) , data = data_train_for_hierarchical_analysis)
#fla_fixed  = tt
model_mixed = lmer(formula = as.formula(fla_mixed) , data = data_train_for_hierarchical_analysis)
model_mixed_Bayesian = brm(formula = as.formula(fla_mixed) , data = data_train_for_hierarchical_analysis, family = gaussian(), prior = c(set_prior("normal(0, 2)", class = "b"),
                                                                                                       set_prior("normal(0, 2)", class = "Intercept")
),

warmup = 4000, iter = 10000, chains = 3, cores = 3,

control = list(adapt_delta = 0.99, max_treedepth = 20))

data_test_for_hierarchical_analysis = data_test  |> filter(.data[[group_variable]] %in% filter_vector_list[[group_variable]] ) |> na.omit() |> distinct()

# Y_predict_test = data_train_for_hierarchical_analysis |> lm(formula = as.formula(fla_fixed)) |> predict(data_test_for_hierarchical_analysis)
# plot(Y_predict_test, data_test_for_hierarchical_analysis$trait)
# abline(0, 1)

#I make plots based on the mixed model. I only use the fixed effects for observations which do not have species information in the training dataset
data_test_for_hierarchical_analysis_filtered = data_test_for_hierarchical_analysis |> filter(genus_species1 %in% unique(data_train_for_hierarchical_analysis$genus_species1))
Y_pred_mixed_filtered = predict(model_mixed, data_test_for_hierarchical_analysis_filtered)
data_test_for_hierarchical_analysis_filtered1 = data_test_for_hierarchical_analysis_filtered |> mutate(Prediction = Y_pred_mixed_filtered)
Y_pred_global_filtered = predict(model, data_test_for_hierarchical_analysis_filtered)
data_test_for_hierarchical_analysis_global_filtered1 = data_test_for_hierarchical_analysis_filtered |> mutate(Prediction = Y_pred_global_filtered)
plot(Y_pred_global_filtered, Y_pred_mixed_filtered)
abline(0, 1)
RMSE_function(Y_pred_global_filtered, data_test_for_hierarchical_analysis_filtered$trait)
RMSE_function(Y_pred_mixed_filtered, data_test_for_hierarchical_analysis_filtered$trait)

cor_function(Y_pred_global_filtered, data_test_for_hierarchical_analysis_filtered$trait)
cor_function(Y_pred_mixed_filtered, data_test_for_hierarchical_analysis_filtered$trait)


data_test_for_hierarchical_analysis_no_species  = data_test_for_hierarchical_analysis |> filter(! (genus_species1 %in% unique(data_train_for_hierarchical_analysis$genus_species1)) )
Y_pred_mixed_no_species = predict(model_mixed, data_test_for_hierarchical_analysis_no_species, re.form = NA) # species data not available in training datasets
data_test_for_hierarchical_analysis_no_species1 = data_test_for_hierarchical_analysis_no_species |> mutate(Prediction = Y_pred_mixed_no_species)


data_test_comp = data_test_for_hierarchical_analysis_filtered1 |> bind_rows(data_test_for_hierarchical_analysis_no_species1) |> mutate(Difference = Prediction - trait) 
data_test_comp |> filter(abs(Difference) > 1) |>  ggplot(aes(trait, Difference, color = genus_species1)) +
  geom_point() + geom_hline(yintercept = 0) + ylab("Difference") +
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
           label = paste0("Count = ", length(which(abs(data_test_comp$Difference) >1))))


Y_pred_global  = data_train_for_hierarchical_analysis |> lm(formula = as.formula(fla_fixed)) |> predict(data_test_comp)

data_test_comp = data_test_comp |> mutate(Global_pred = Y_pred_global)  |> mutate(Difference_global = Global_pred - trait)
RMSE1 = round(RMSE_function(data_test_comp$trait, data_test_comp$Prediction),3)
cor1 = round(cor_function(data_test_comp$trait, data_test_comp$Prediction), 3)

RMSE_global = round(RMSE_function(data_test_comp$trait, data_test_comp$Global_pred),3)
cor_global = round(cor_function(data_test_comp$trait, data_test_comp$Global_pred), 3)

data_test_comp |> ggplot(aes(Global_pred, trait)) + geom_point() +
  geom_abline(colour = "red") + 
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
           label = paste0("RMSE = ", RMSE_global, "; Cor = ", cor_global), size = 8)

data_test_comp |> ggplot(aes(Prediction, trait)) + geom_point() +
  geom_abline(colour = "red") + 
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1,
           label = paste0("RMSE = ", RMSE1, "; Cor = ", cor1), size = 8 )


data_test_comp |> filter(abs(Difference_global) > 1) |>  ggplot(aes(trait, Difference_global, color = genus_species1)) +
  geom_point(size = 3) + geom_hline(yintercept = 0) + ylab("Difference") +theme(legend.position="bottom") + ylim(-5,3)
data_test_comp |> filter(abs(Difference) > 1) |>  ggplot(aes(trait, Difference, color = genus_species1)) +
  geom_point(size = 3) + geom_hline(yintercept = 0) + ylab("Difference") +theme(legend.position="bottom")+ ylim(-5,3)

saveRDS(data_test_comp, file = paste0("/Users/dhruvakathuria/Downloads/", trait_name1, ".rds"))

# plot(Y_pred_mixed_filtered, data_test_for_hierarchical_analysis_filtered$trait, pch = 19, cex = 0.5)
# abline(0, 1)
# plot(Y_pred_mixed_no_species,  data_test_for_hierarchical_analysis_no_species$trait, pch = 19, cex = 0.5)
# abline(0, 1)
# plot(c(Y_pred_mixed_filtered, Y_pred_mixed_no_species), c(data_test_for_hierarchical_analysis_filtered$trait, data_test_for_hierarchical_analysis_no_species$trait), pch = 19, cex = 0.75)
# abline(0, 1, col = "red", lwd = 2)

########

Y_pred_mixed = predict(model_mixed, data_test_comp, re.form = NA)
plot(Y_pred_mixed, data_test_comp$trait)
abline(0, 1)

#I now compare these results with fixed PCR algorithm
Y_predict_test_filtered = 
plot(Y_predict_test_filtered, data_test_for_hierarchical_analysis_filtered$trait, pch = 19, cex = 0.75)
abline(0, 1, col = "red", lwd = 2)
RMSE_function(Y_predict_test_filtered, data_test_for_hierarchical_analysis_filtered$trait)
cor_function(Y_predict_test_filtered, data_test_for_hierarchical_analysis_filtered$trait)




