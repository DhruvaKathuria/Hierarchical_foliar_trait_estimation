library(cowplot)
rm(list = ls())



# The below code gives "data_mat_test" which gives us the test matrix for a trait with predictions as "pred" and observations as "trait"
source(paste0(Github_dir, "R_codes/Regression_algorithms/ECOSIS_Implementation_file_for_Bayesian_ML.R"))

###################################################Functions#####################################################

function_for_plotting <- function(data_mat, name1) # name1 is either "Global" or "a dataset name out of the datsets"
{
  gg_pred <- gg_error <- list()  # these are the ggplot elements for each of the metadata_vector, one is for plotting the predicitons, and the other is for plotting the errors
  for(jj in 1: 3)
  {
    metadata_filter = metadata_vector[jj]
    
    data_mat = data_mat[!data_mat[, metadata_filter] %in% c("Data unavailable", "Error in extraction, check"), ]
    data_mat$trait = as.numeric(data_mat$trait)
    data_mat$error = (data_mat$pred - data_mat$trait)
    
    gg_pred[[metadata_filter]] <-  ggplot(data_mat, aes(x = pred, y = trait, color = .data[[metadata_filter]], alpha = 0.5)) +
      geom_point() + geom_abline()+ ggtitle(name1) + scale_fill_manual(values = colors_list[[jj]]) +
      theme(plot.title = element_text(size = 6))
    
    gg_error[[metadata_filter]] <- ggplot(data_mat, aes(x = trait, y = error, color = .data[[metadata_filter]], alpha = 0.5)) +
      geom_point() + ggtitle(name1) +scale_fill_manual(values = colors_list[[jj]]) +
      theme(plot.title = element_text(size = 6)) #+ geom_abline()
  }
  list(pred = gg_pred, error = gg_error)
}
#################################################################################################################

##########################################Making plots for the predictions and observations#########################

metadata_vector = c("Growth_form", "Leaf", "Phenology") # this is from get_growth_form_phenology_leaf_type_etc_from_Wiki.R

######################################Color_lists#########################################
colors_list <- list()
colors_list[[1]] <- c("tree" = "#FF0000", "grass" = "#00FF00", "herbaceous" = "#0000FF",
                      "vine" = "#FFFF00", "shrub" = "#00FFFF")
colors_list[[2]] <- c("broad" = "#FF0000", "needle" = "#00FFFF")
colors_list[[3]] <- c("deciduous" = "#FF0000", "evergreen" = "#00FFFF")
############################################################################################


if(filtering_type == "Global")
{
  out_plots = function_for_plotting(data_mat_test, "Global")
}else if(filtering_type == "Site_specific")
{
  out_plots = mapply(function_for_plotting, data_mat_test, datasets_to_take_for_trait, SIMPLIFY = F)
}

###Example for combined plot for Site_specific
if(filtering_type == "Global")
{
  for(metadata_filter in metadata_vector)
  {
    out_plots[["pred"]]
    out_plots[["error"]]
  }
}


if(filtering_type == "Site_specific")
{
  for(metadata_filter in metadata_vector)
  {
    plot_grid(out_plots[[datasets_to_take_for_trait[1]]] [["pred"]] [[metadata_filter]], out_plots[[datasets_to_take_for_trait[2]]] [["pred"]] [[metadata_filter]], out_plots[[datasets_to_take_for_trait[3]]] [["pred"]] [[metadata_filter]],
              out_plots[[datasets_to_take_for_trait[4]]] [["pred"]] [[metadata_filter]], out_plots[[datasets_to_take_for_trait[5]]] [["pred"]] [[metadata_filter]], out_plots[[datasets_to_take_for_trait[6]]] [["pred"]] [[metadata_filter]])
    
    plot_grid(out_plots[[datasets_to_take_for_trait[1]]] [["error"]] [[metadata_filter]], out_plots[[datasets_to_take_for_trait[2]]] [["error"]] [[metadata_filter]], out_plots[[datasets_to_take_for_trait[3]]] [["error"]] [[metadata_filter]],
              out_plots[[datasets_to_take_for_trait[4]]] [["error"]] [[metadata_filter]], out_plots[[datasets_to_take_for_trait[5]]] [["error"]] [[metadata_filter]], out_plots[[datasets_to_take_for_trait[6]]] [["error"]] [[metadata_filter]])
  
  }
}
##########################################################################################################