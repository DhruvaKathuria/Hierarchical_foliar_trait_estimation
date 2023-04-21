library(brms)
library(caret)
library(tidyverse)
library(ggpmisc)

take_Principal_Components = T
ncomp = 20
group_variable = "genus_species1"
# The below list is formed to do form groups for the hierarchical type analysis
# Getting data ready for analysis
#Source the file below to get the input data matrices and the output trait
source("/Users/dhruvakathuria/Documents/GitHub/Hierarchical_foliar_trait_estimation/R_codes/Regression_algorithms/ECOSIS_Implementation_file_for_Bayesian_ML.R")

filter_vector_list = list("Growth_form" = c("tree", "shrub", "herbaceous", "grass", "vine"), 
                          "Leaf" = c("broad", "needle"), 
                          "Phenology" = c("deciduous", "evergreen"),
                          "genus_species1" = unique(trait_and_metadata_dataframe$genus_species1[!(trait_and_metadata_dataframe$genus_species1 %in% "NA NA")]))

###########################################Functions################################################
get_PCs <- function(X_train, X_test, ncomp)
{
  results <- prcomp(as_tibble(X_train), center = TRUE, scale = F)
  out1_load = results$rotation
  PC_values_for_spectra = as.matrix(scale(X_train)) %*% out1_load[, 1:ncomp]
  #variance_explained = results$sdev^2 / sum(results$sdev^2)
  data_test = t((t(X_test) - results$center))
  data_test_PCA = as.matrix(data_test) %*% out1_load
  data_test_PCA_filtered = data_test_PCA[, 1:ncomp]
  
  list(PC_train = PC_values_for_spectra, PC_test = data_test_PCA_filtered)
}

#######################################################################################################



spectra_df_scale = scale(spectra_df)
Y_train = as.numeric(trait_and_metadata_dataframe$trait[indices_subset ])
Y_test = as.numeric(trait_and_metadata_dataframe$trait[-indices_subset ])
X_train = spectra_df_scale[indices_subset, ]
X_test = spectra_df_scale[-indices_subset, ]

colnames(X_train) = colnames(X_test) = paste("X", colnames(X_train), sep = "")

###############
if(take_Principal_Components == T)
{
  
  PCs = get_PCs(X_train, X_test, ncomp)
  X_train = PCs[[1]]; X_test = PCs[[2]]
}

data_matrix_for_analysis =  trait_and_metadata_dataframe[indices_subset, ] |> bind_cols(X_train) |> filter(.data[[group_variable]] %in% filter_vector_list[[group_variable]] ) |> na.omit() |> distinct()
data_matrix_for_analysis$trait = as.numeric(data_matrix_for_analysis$trait)
data_matrix_for_analysis$trait1 = scale(data_matrix_for_analysis$trait)
###############################Applying the Bayesian Hierarchical algorithms#####################
if(take_Principal_Components == T)
{
  PC_names = paste0("PC", 1:ncomp)
  nhierarchical = 10
  fla = paste("trait1 ~ 1 + ", paste(PC_names, collapse = "+"), paste("+(1+", paste(PC_names[1:nhierarchical], collapse = "+"), paste("|", group_variable, ")", sep = ""), sep = ""))
}else {
  fla = paste("trait1 ~1 + ", paste(colnames(X_train), collapse = "+"), paste("+(1+", paste(colnames(X_train)[1:100], collapse = "+"), paste("|", group_variable, ")", sep = ""), sep = ""))
}

out3 = brm(formula = as.formula(fla) , data = data_matrix_for_analysis, family = gaussian(), prior = c(set_prior("horseshoe(3, par_ratio = 3)", class = "b"),
                                                                                                               set_prior("normal(0, 2)", class = "Intercept")
                                                                                                        ),
    
    warmup = 1000, iter = 4000, chains = 3, cores = 3,
    
    control = list(adapt_delta = 0.99, max_treedepth = 20))

saveRDS(out2, "first_success")


############################################Predicting on test data########################################

data_test2 = cbind(trait_and_metadata_dataframe[-indices_subset, ], data_test_PCA_filtered)
data_test2 = data_test2 |> filter(Growth_form %in% filter_vector_list[[Growth_form]])
pp = predict(out3, data_test2, robust = T)
plot(pp[,1], data_test2$trait)
abline(0, 1)
RMSE_function(pp[,1], as.numeric(data_test2$trait))
cor_function(pp[,1], as.numeric(data_test2$trait))

t3 = na.omit(t2)
pp2 = predict(out2, t3)
plot(pp2[,1], t3$trait, xlim = c(0, 400))
abline(0, 1)

f1 = coef(out3, summary = T)

ind_vine = which(data_test2$Growth_form == "vine")
composite_vine = data.frame(cbind(pp[,1], as.numeric(data_test2$trait), data_test2$Growth_form))
composite_vine = composite_vine |> filter(X3 == "vine")
plot(composite_vine[,1], composite_vine[, 2])
RMSE_function(as.numeric(composite_vine[,1]), as.numeric(composite_vine[, 2]))
cor_function(as.numeric(composite_vine[,1]), as.numeric(composite_vine[, 2]))
abline(0, 1)
plot(pred1[ind_vine], as.numeric(data_frame_1_test$Y_train)[ind_vine])
RMSE_function(pred1[ind_vine], as.numeric(data_frame_1_test$Y_train)[ind_vine])
cor_function(pred1[ind_vine], as.numeric(data_frame_1_test$Y_train)[ind_vine])
abline(0, 1)
composite_vine_PCR = composite_vine_PCR |> filter(X3 == "vine")

data_test_composite = data.frame (Pred = c(Bayesian = pp[,1], PCR_pred = pred1), Method = rep(c( "Bayesian", "PCR"), each = length(pred1)), Obs = rep(data_test2$trait, 2), Growth_form = rep(data_test2$Growth_form, 2))
data_test_composite = na.omit(data_test_composite)
data_test_composite$Obs = as.numeric(data_test_composite$Obs)
g1 = ggplot(data_test_composite, aes(Pred, Obs, color = Growth_form)) +
  geom_point() + geom_abline() 
  facet_grid(Method ~ Growth_form)

ggsave(filename = "/Users/dhruvakathuria/Downloads/predictions.tiff", plot = g1, width = 8, height = 5, units = "in",
         dpi = 300)

summ <- data_test_composite %>% 
  group_by(Method, Growth_form) %>% 
  summarise(Rsq = R2(Pred, Obs),
            RMSE = RMSE(Pred, Obs)) %>% 
  mutate_if(is.numeric, round, digits=2) 

