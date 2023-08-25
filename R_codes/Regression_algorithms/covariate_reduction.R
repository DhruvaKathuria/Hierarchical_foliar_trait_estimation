library(projpred)
# Global parameters -------------------------------------------------------

trait_name1 = "Carotenoid_Area"
prediction_algorithm <- "raw_spectra"
date_for_brms_file <- "2023-08-17" #this is the date the brms file was saved
                                   # in folder code data/code_output_data. brms
                                   # files are saved using supervised_pc_and....R

brms_normal <- readRDS(paste0("data/code_output_data/brms_object_",
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
par_ratio1 <- 0.05

library(doParallel)
cl <- makeCluster(3)
registerDoParallel(cl)

nterms_max1 = 30 # to be determined from cvvs_fast
cv_out <- cv_varsel(brms_normal,
                    cv_method = "kfold",
                    method = "forward",
                    K = 5,
                    nterms_max = nterms_max1,
                    parallel = FALSE,
                    verbose = T)
saveRDS(cv_out,
        paste0("data/code_output_data/proj_pred_object_",
               trait_name1, 
               "_",
               prediction_algorithm,
               "_",
               date_for_brms_file,
               ".rds"))

plot(cv_out, 
     stats = c('mlpd', 'rmse'), 
     deltas=FALSE)

nsel <- suggest_size(cv_out, stat = "rmse", alpha=0.1) # Dont rely on this
                                                            # look more on the plot
vsel <- solution_terms(cv_out)[1:nsel]

