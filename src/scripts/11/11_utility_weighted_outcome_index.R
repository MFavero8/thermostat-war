# ==============================================================================
# Script: 11_utility_weighted_outcome_index.R
#
# Description: Main script for calculating and visualising the Utility-Weighted
#              Thermal Discomfort Index (UWTDI), including temperature-based,
#              thermal comfort and intervention analyses
#
# Author: Matteo Favero
# ==============================================================================


# ==============================================================================
# 0. Setup
# ==============================================================================

before_section <- 
  ls(all.names = TRUE)


# 0.1 Check project root -------------------------------------------------------

if (!dir.exists("src/data")) {
  
  stop("Project root not detected. Please open the .Rproj file.")
  
}


# 0.2 Load packages ------------------------------------------------------------

suppressPackageStartupMessages({
  library(modelr)
  library(data.table)
  library(tidyverse)
  library(tidybayes)
  library(brms)
  library(comf)
  library(metR)
})

# ==============================================================================
# 1. Load Configuration
# ==============================================================================

# 1.1 Load configuration files ------------------------------------------------

source("src/scripts/config.R")
source("src/scripts/parameters.R")
source("src/scripts/development_settings.R")
source("src/scripts/prediction_and_intervention_settings.R")
#source("src/scripts/intervention_settings.R")
source("src/scripts/11/cost_schemes.R")


# 1.2 Define intervention scenario ---------------------------------------------

scenario <- paste0("met_", sprintf("%.2f", metabolic_rate))


# 1.3 Extract heating intervention values --------------------------------------

temp_values_h <- heating_interventions[[scenario]]$temperature

rh_values_h <- heating_interventions[[scenario]]$humidity


# 1.4 Extract cooling intervention values --------------------------------------

temp_values_c <- cooling_interventions[[scenario]]$temperature

rh_values_c <- cooling_interventions[[scenario]]$humidity


# 1.5 Construct heating intervention data --------------------------------------

heating_intervention <- 
  data.frame(temp = temp_values_h,
             rh = rh_values_h)


# 1.6 Construct cooling intervention data --------------------------------------
cooling_intervention <- 
  data.frame(temp = temp_values_c,
             rh = rh_values_c)


# 1.7 Define heating contrast name ---------------------------------------------

contrast_name_h <- 
  paste0(temp_values_h[2], "t", rh_values_h[2], "rh_",
         temp_values_h[1], "t", rh_values_h[1], "rh")


# 1.8 Define cooling contrast name ---------------------------------------------

contrast_name_c <- 
  paste0(temp_values_c[2], "t", rh_values_c[2], "rh_",
         temp_values_c[1], "t", rh_values_c[1], "rh")


# ==============================================================================
# 2. Load Scripts
# ==============================================================================

# 2.1 Load temperature and thermal comfort analysis scripts --------------------

source("src/scripts/11/11a_0_UWTDI.R")
source("src/scripts/11/11a_1_visualisation_temperature.R")
source("src/scripts/11/11a_2_visualisation_heatmap.R")


# 2.2 Load intervention analysis scripts --------------------------------------

source("src/scripts/11/11b_0_UWTDI_intervention.R")
source("src/scripts/11/11b_1_visualisation_distribution.R")



# ==============================================================================
# 3. Unload Packages
# ==============================================================================

packages_to_unload <- 
  c("modelr",
    "data.table",
    "tidyverse",
    "tidybayes",
    "brms",
    "comf",
    "metR")

        
for (package in packages_to_unload) {
  
  if (paste0("package:", package) %in% search()) {
    
    detach(paste0("package:", package),
           unload = FALSE,
           character.only = TRUE)
    
  }
  
}


# ==============================================================================
# 4. Clean Workspace
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
  