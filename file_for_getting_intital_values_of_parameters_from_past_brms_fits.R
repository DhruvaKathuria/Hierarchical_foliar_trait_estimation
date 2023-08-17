library(brms)
# Global parameters -------------------------------------------------------

trait_name1 = "Nitrogen"
prediction_algorithm <- "raw_spectra"
date_for_brms_file <- "2023-07-24" #this is the date the brms file was saved
# in folder code data/code_output_data. brms
# files are saved using supervised_pc_and....R

brms_normal <- readRDS("/Users/dkathuri/Downloads/brms_object_Nitrogen_raw_spectra_2023-07-03 11:15:30.466665.rds")

par1 <- fixef(brms_normal)
par2 <- par1[, 1]

sigma1 <- posterior_summary(as.data.frame(brms_normal)$sigma)

plot(par2)
data_frame_input <- brms_normal$data
cmeans <- colMeans(data_frame_input)
plot(cmeans)

csd_1 <- apply(data_frame_input, 2, sd)
plot(csd_1)

saveRDS(par2, file = "Nitrogen_population_level_parameters.rds")
saveRDS(sigma1, file = "Nitrogen_sigma_parameters.rds")

# Setting up brms init function -------------------------------------------

# Initial kicks


init_func <- function(chain_id=1) {
  list ( Intercept  =  par2[1] ,
         beta  =  par2[-1] ,
         sigma  =  sigma1[1])
}
# See an example output
init_func(chain_id = 1)
# In your case just two chains is needed.
init_list <- list(
  init_func(chain_id = 1)
  #init_func(chain_id = 2),
  #init_func(chain_id = 3),
  #init_func(chain_id = 4)
  # init_func(chain_id = 5),
  # init_func(chain_id = 6),
  # init_func(chain_id = 7),
  # init_func(chain_id = 8),
  # init_func(chain_id = 9)
)

# pars1 <- fixef(brms_1)
# pars1_1 <- pars1[,1]
# init_func <- function(chain_id=1) {
#   list ( Intercept  =  pars1_1[1] ,
#          beta  =  pars1_1[-1] ,
#          sigma  =  3.72)
# }

