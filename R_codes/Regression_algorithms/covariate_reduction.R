library(projpred)
library(stringr)
library(brms)
# Global parameters -------------------------------------------------------

trait_name1 = "LMA"
prediction_algorithm <- "raw_spectra"
date_for_brms_file <- "2023-09-13" #this is the date the brms file was saved
                                   # in folder code data/code_output_data. brms
                                   # files are saved using supervised_pc_and....R

macstudio_folder <- "/Users/dhruvakathuria/Library/Mobile Documents/com~apple~CloudDocs/NASA_work/Github_data/Hierarchical_foliar_trait_estimation"
brms_normal <- readRDS(paste0(str_glue("{macstudio_folder}/data/code_output_data/brms_object_"),
               trait_name1, 
               "_",
               prediction_algorithm,
               "_",
               date_for_brms_file,
               ".rds"))


# fast covariate reduction to determine the approximate number of  --------

cvvs_fast <- cv_varsel(
  brms_normal,
  validate_search = FALSE,
  ### Only for the sake of speed (not recommended in general):
  nclusters_pred = 20,
  ###
  nterms_max = 30,
  ### In interactive use, we recommend not to deactivate the verbose mode:
  verbose = T
  ### 
)

plot(cvvs_fast, stats = "mlpd", ranking_nterms_max = NA)


# actual implementation ---------------------------------------------------
par_ratio1 <- 0.025

# library(doParallel)
# cl <- makeCluster(3)
# registerDoParallel(cl)

nterms_max1 = 50 # to be determined from cvvs_fast
cv_out <- cv_varsel(brms_normal,
                    cv_method = "kfold",
                    method = "forward",
                    K = 5,
                    nterms_max = nterms_max1,
                    parallel = FALSE,
                    verbose = T,
                    nclusters_pred = 15,
                    nclusters = 15, 
                    search_terms = coln1)


#steps for varsel
# Use this only if you have fit the data to a subset of data_train_predictors
# in brms_normal and then you use the other subset for the varsel
# right now I am just using the same data for both fitting and for 
# varsel. This might admittedly lead to overfitting, but it saves a ton of
# memory.
# set.seed(100)
# search_indices <- sample(1:nrow(data_train_for_analysis), 0.6*nrow(data_train_for_analysis))
# data_train_search <- data_train_for_analysis[search_indices, ]
# data_train_pred <- data_train_for_analysis[-search_indices, ]
# 
# data_train_pred_predictors <- data_train_pred %>% select(starts_with("x"))


# creating a reference model ----------------------------------------------


# list_for_varsel <- list(data <- data_train_pred_predictors,
#                         offset <- rep(0, nrow(data_train_pred)), 
#                         weights <- rep(1, nrow(data_train_pred)), 
#                         y <-  as.vector(data_train_pred$trait))
# 
# reference_model <-  get_refmodel(brms_normal, newdata = data_train_search)
# 


# using same data for both brms and also for varsel -----------------------

# code to take a subset of spectra using correlation ----------------------
# uncomment if file not saved
# data1 <- brms_normal$data
# cor1 <- apply(data1[, -1], 2, function(x)abs(cor(x, data1[, 1])))
# plot(cor1)
# 
# l1 <- which(cor1 > 0.15)
# data_input <- data1[ , -1]
# coln1 <- colnames(data_input)[l1]
# 
# s1 <- solution_terms(cv_out)[1:20]
# which(s1 %in% coln1)
# 
# saveRDS(coln1, str_glue("{macstudio_folder}/data/code_output_data/terms_for_{trait_name1}"))

coln1 <- readRDS(paste0(macstudio_folder, "/data/code_output_data/terms_for_LMA"))

nterms_max1 = 40 # to be determined from cvvs_fast
cv_out <- varsel(brms_normal,
                   # cv_method = "kfold",
                    method = "forward",
                    #d_test <- data_train_pred,
                    nterms_max = nterms_max1,
                    parallel = FALSE,
                    verbose = T,
                    #ndraws = 1000,
                    #nclusters = NULL,
                    #ndraws_pred = 100,
                    nclusters_pred = 15,
                    nclusters = 15, 
                    search_terms = coln1)

saveRDS(cv_out,
        paste0(macstudio_folder, "/data/code_output_data/varsel_proj_pred_object_search_terms_with_correlation_greater_than_0.15_nterms_max_",
               nterms_max1,
               trait_name1, 
               "_",
               prediction_algorithm,
               "_",
               date_for_brms_file,
               ".rds"))

plot(cv_out, 
     stats ='rmse', 
     deltas=FALSE)

nsel <- suggest_size(cv_out, stat = "rmse", alpha=0.1) # Dont rely on this
                                                            # look more on the plot
vsel <- solution_terms(cv_out)[1:50]

