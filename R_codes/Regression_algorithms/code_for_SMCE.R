library(projpred)

nterms_max1 = 40 
nclusters1 = 20
trait_name1 = "Nitrogen"
brms_normal <- readRDS("brms_object_Nitrogen_raw_spectra_2023-12-21.rds")
#nclusters_pred1 = 2
cv_out <- varsel(brms_normal,
                 #cv_method = "kfold",
                 method = "forward",
                 nterms_max = nterms_max1,
                 parallel = FALSE,
                 verbose = T
                 #ndraws = 100,
                 #K= 5,
                 #nclusters = NULL,
                 #ndraws_pred = 100
                 #nclusters_pred = nclusters_pred1,
                 #nclusters = nclusters1
)
#search_terms = spectra_to_take)

saveRDS(cv_out,
        paste0(trait_name1,
               ".rds"))