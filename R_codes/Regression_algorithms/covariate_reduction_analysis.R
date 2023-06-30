
# Global parameters -------------------------------------------------------

trait_name1 = "Carotenoid_Area"
prediction_algorithm <- "raw_spectra"
date_for_brms_file <- "2023-06-29" #this is the date the brms file was saved
# in folder code data/code_output_data. brms
# files are saved using supervised_pc_and....R

cv_out <- readRDS(paste0("data/code_output_data/proj_pred_object_",
                         trait_name1, 
                         "_",
                         prediction_algorithm,
                         "_",
                         date_for_brms_file,
                         ".rds"))



