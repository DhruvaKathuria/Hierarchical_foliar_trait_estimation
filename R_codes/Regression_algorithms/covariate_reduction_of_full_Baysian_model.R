library(projpred)
library(stringr)
library(brms)
# Global parameters -------------------------------------------------------

trait_name1 = "Carotenoid_Area"
prediction_algorithm <- "raw_spectra"


#uncomment the below if you have your brms_file run using "supervised_pc_and_raw_spectra_bayesian.R"

#brms_normal <- readRDS(paste0(str_glue("/data/code_output_data/brms_object_"),
#                              trait_name1, 
#                              "_",
#                              prediction_algorithm,
#                              "_",
#                              date_for_brms_file,
#                              ".rds"))



# comment out the below if you are using your own brms file. Run the below line if you are using the zenodo brms_file
brms_normal <- readRDS("brms_object_Carotenoid_Area_raw_spectra_2024-02-12.rds")

# fast covariate reduction to determine the approximate number of components (not used in this paper)  --------

cvvs_fast <- varsel(
  brms_normal,
  validate_search = TRUE,
  ### Only for the sake of speed (not recommended in general):
  nclusters_pred = 5,
  #cv_method = "kfold",
  #K = 5,
  ###
  nterms_max = 20,
  verbose = T
  ### 
)

plot(cvvs_fast, stats = "mlpd", ranking_nterms_max = NA)


# actual implementation used in the paper ---------------------------------------------------
par_ratio1 <- 0.025

# library(doParallel)
# cl <- makeCluster(3)
# registerDoParallel(cl)

# This step will be very slow
nterms_max1 = 50 #  we fix it to 50 in this paper
cv_out <- cv_varsel(brms_normal,
                    cv_method = "kfold",
                    method = "forward",
                    K = 5,
                    nterms_max = nterms_max1,
                    parallel = FALSE,
                    verbose = T,
                    nclusters_pred = 15,
                    nclusters = 15)

plot(cv_out, 
     stats ='rmse', 
     deltas=FALSE,
     text_angle = 45,
     ranking_abbreviate = T,
     ranking_repel = "text") +
  theme(axis.text.x = element_text(angle = 45, 
                                   vjust = 0.5, 
                                   hjust = 1,
                                   size = 11),
        axis.text.y = element_text(size = 11),
        text = element_text(size = 10)) 