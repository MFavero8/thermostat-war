# ==============================================================================
# Script: 10_risk_prediction_risk_difference_effect_modification.R
#
# Description:
#   Calculate model-based predicted probabilities for heating and cooling
#   interventions, followed by risk prediction, risk difference, and effect
#   modification analyses.
#
# Author: Matteo Favero
# ==============================================================================

message("----------------------------------------")
message("Calculation model prediction\n")

# ==============================================================================
# 0. Setup
# ==============================================================================

before_section <- 
  ls(all.names = TRUE)


# 0.1 Check project root --------------------------------------------------------

if (!dir.exists("src/data")) {
  
  stop("Project root not detected. Please open the .Rproj file.")
  
}


# 0.2 Load packages -------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidybayes)
  library(patchwork)
  library(grid)
  library(readr)
})


# 0.3 Load configuration --------------------------------------------------------

source("src/scripts/config.R")
source("src/scripts/parameters.R")
source("src/scripts/development_settings.R")
source("src/scripts/prediction_and_intervention_settings.R")


# ==============================================================================
# 1. Define Intervention Scenarios
# ==============================================================================

# 1.1 Identify intervention scenario -------------------------------------------

scenario <- paste0("met_", sprintf("%.2f", metabolic_rate))


# 1.2 Extract heating intervention values --------------------------------------

temp_values_h <- heating_interventions[[scenario]]$temperature

rh_values_h <- heating_interventions[[scenario]]$humidity


# 1.3 Extract cooling intervention values --------------------------------------

temp_values_c <- cooling_interventions[[scenario]]$temperature

rh_values_c <- cooling_interventions[[scenario]]$humidity


# 1.4 Define intervention contrast labels --------------------------------------

contrast_name_h <- 
  paste0(temp_values_h[2], "t", rh_values_h[2], "rh_",
         temp_values_h[1], "t", rh_values_h[1], "rh")

contrast_name_c <- 
  paste0(temp_values_c[2], "t", rh_values_c[2], "rh_",
         temp_values_c[1], "t", rh_values_c[1], "rh")


# ==============================================================================
# 2. Models to Process
# ==============================================================================

models_to_process <- models_to_run


# ==============================================================================
# 3. Calculate Model Predictions
# ==============================================================================

for (model_name in models_to_process) {
  
  message("Processing ",
          model_name)
  
  # 3.1 Define prediction output folder ----------------------------------------
  
  output_prediction_folder <-
    file.path(prediction_folder,
              model_name,
              "intervention")

  if (!dir.exists(output_prediction_folder)) {
    
    dir.create(output_prediction_folder,
               recursive = TRUE)
    
  }

  
  # 3.2 Define prediction output files -----------------------------------------
  
  output_file_heating <- 
    file.path(output_prediction_folder,
              paste0("epred_", 
                     contrast_name_h, 
                     ".rds"))
  
  output_file_cooling <- 
    file.path(output_prediction_folder,
              paste0("epred_", 
                     contrast_name_c, 
                     ".rds"))
  
  # 3.3 Check whether predictions already exist --------------------------------
  
  if (file.exists(output_file_heating) && 
      file.exists(output_file_cooling)) {
    
    message("\  - Skipping since calculations already exist")
    
  } else {
    
    message("\  - Files not found. Calculating now")
    
    
    # 3.4 Load fitted model ----------------------------------------------------
    
    fit <- 
      readRDS(file.path(fitted_model_folder,
                        paste0(model_name,
                               ".rds")))
  
    
    # ==========================================================================
    # 4. Define Prediction Grids
    # ==========================================================================
    
    # 4.1 Heating prediction grid ----------------------------------------------
    
    nd.pred_heating <- 
      crossing(c_air.temp = temp_values_h - air.temp_reference,
               c_rel.hum = (rh_values_h / rh_step) - (rh_reference / rh_step),
               sex = c("male", "female"),
               time.day = c("afternoon", "evening"),
               c_time.onset.h = c(0, 1, 2), #c(0, 0.5, 1, 1.5, 2),
               count = 1)
    
    
    # 4.2 Cooling prediction grid ----------------------------------------------
    
    nd.pred_cooling <- 
      crossing(c_air.temp = temp_values_c - air.temp_reference,
               c_rel.hum = (rh_values_c / rh_step) - (rh_reference / rh_step),
               sex = c("male", "female"),
               time.day = c("afternoon", "evening"),
               c_time.onset.h = c(0, 1, 2), #c(0, 0.5, 1, 1.5, 2),
               count = 1)
    
    
    # ==========================================================================
    # 5. Generate Posterior Predictions
    # ==========================================================================
    
    # 5.1 Sample-averaged predictions ------------------------------------------
  
    
      # 5.1.1 Specify population-average condition -----------------------------
      
      nd.pred_heating$condition_id <- "pop.average"
      nd.pred_cooling$condition_id <- "pop.average"
      
      
      # 5.1.2 Generate heating predictions -------------------------------------
      
      epred_filter_heating <- 
        fit %>%
        epred_draws(newdata = nd.pred_heating,
                    seed = intervention_seed,
                    re_formula = NULL,
                    allow_new_levels = TRUE,
                    sample_new_levels = "uncertainty") %>%
        rename(vote = .category,
               prob = .epred) %>%
        mutate(air.temp = c_air.temp + air.temp_reference,
               rel.hum = (c_rel.hum + (rh_reference / rh_step)) * rh_step,
               time.onset.h =  c_time.onset.h + onset_time_reference)
      
      
     # 5.1.3 Generate cooling predictions --------------------------------------
      
      epred_filter_cooling <- 
        fit %>%
        epred_draws(newdata = nd.pred_cooling,
                    seed = interventnion_seed,
                    re_formula = NULL,
                    allow_new_levels = TRUE,
                    sample_new_levels = "uncertainty") %>%
        rename(vote = .category,
               prob = .epred) %>%
        mutate(air.temp = c_air.temp + air.temp_reference,
               rel.hum = (c_rel.hum + (rh_reference / rh_step)) * rh_step,
               time.onset.h =  c_time.onset.h + onset_time_reference)
      
      
      # 5.1.4 Save predictions -------------------------------------------------
      
      saveRDS(epred_filter_heating, output_file_heating)
      saveRDS(epred_filter_cooling, output_file_cooling)
      
      
      # 5.1.5 Remove temporary objects -----------------------------------------
      
      rm(fit, 
         nd.pred_heating, 
         nd.pred_cooling,
         epred_filter_heating,
         epred_filter_cooling)
      
  }
}
  

# ==============================================================================
# 6. Completion
# ==============================================================================

message("\nCalculation model prediction completed")
message("----------------------------------------\n")


# ==============================================================================
# 7. Risk Prediction Analysis
# ==============================================================================

# 7.1 Calculate risk predictions -----------------------------------------------

source("src/scripts/10/10a_1_risk_prediction.R") 


# 7.2 Generate risk prediction visualisations ----------------------------------

source("src/scripts/10/10a_2_visualisation.R") 


## ==============================================================================
# 8. Risk Difference and Effect Modification Analysis
# ==============================================================================

# 8.1 Calculate risk differences ------------------------------------------------

source("src/scripts/10/10b_1_risk_difference.R") 


# 8.2 Calculate effect modification ---------------------------------------------

source("src/scripts/10/10b_2_effect_modification.R") 


# 8.3 Generate risk difference and effect modification visualisations ----------

source("src/scripts/10/10b_3_visualisation.R") 


# ==============================================================================
# 9. Unload Packages
# ==============================================================================

packages_to_unload <- 
  c("tidyverse",
    "tidybayes",
    "patchwork",
    "grid",
    "readr")

for (package in packages_to_unload) {
  
  if (paste0("package:", package) %in% search()) {
    
    detach(paste0("package:", package),
           unload = FALSE,
           character.only = TRUE)
    
  }
  
}


# ==============================================================================
# 10. Clean Workspace
# ==============================================================================

after_section <- 
  ls(all.names = TRUE)

new_objects <- 
  setdiff(after_section,
          c(before_section,
            "before_section"))

rm(list = new_objects)

rm(before_section,
   after_section,
   new_objects)

gc()
