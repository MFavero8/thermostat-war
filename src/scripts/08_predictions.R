# ==============================================================================
# Script: 07b_predictions.R
#
# Description: Generate posterior expected predictions from fitted Bayesian models
#
#   Two prediction types are possible:
#
#   1. sample-averaged:
#      marginal predictions including uncertainty in random effects.
#
#   2. cluster-specific:
#      prediction with random effects set to zero.
#
#   Predictions are saved in chunks to avoid memory problems.
#
# Author: Matteo Favero
# ==============================================================================

message("-------------------------------------------")
message("Model predictions\n")

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
  library(tidyverse)
  library(brms)
  library(tidybayes)
})


# 0.3 Load configuration -------------------------------------------------------

source("src/scripts/config.R")
source("src/scripts/parameters.R")
source("src/scripts/development_settings.R")
source("src/scripts/prediction_and_intervention_settings.R")


# ==============================================================================
# 1. Prediction Settings
# ==============================================================================

# 1.1 Select models to predict --------------------------------------------------

models_to_predict <- models_to_run


# ==============================================================================
# 2. Prediction Grid
# ==============================================================================

# 2.1 Define prediction values -------------------------------------------------


temperature_values <- 
  seq(from = 18.5,
      to = 28.5,
      length.out = 101)

humidity_values <- 
  seq(from = 15,
      to = 85,
      length.out = 71)

onset_values <- c(0, 0.5, 1, 1.5, 2)
  


# 2.2 Construct prediction grid ------------------------------------------------

nd.pred <- 
  crossing(c_air.temp = temperature_values - air.temp_reference,
           c_rel.hum = (humidity_values / rh_step) - (rh_reference / rh_step),
           sex = c("male", "female"),
           time.day = c("afternoon", "evening"),
           c_time.onset.h = onset_values,
           count = 1)


# ==============================================================================
# 3. Random-Effect Specification
# ==============================================================================

# 3.1 Define random-effect condition -------------------------------------------

nd.pred$condition_id <- "pop.average"



# ==============================================================================
# 4. Prediction Chunking
# ==============================================================================

# 4.1 Calculate chunk size -----------------------------------------------------

rows_per_intervention <-
  n_distinct(nd.pred$sex) *
  n_distinct(nd.pred$time.day) *
  n_distinct(nd.pred$c_time.onset.h)

interventions_per_chunk <- 20

chunk_size <- 
  rows_per_intervention *
  interventions_per_chunk


# 4.2 Report chunk settings ----------------------------------------------------


message("Prediction chunk settings")

message("  Rows per intervention point: ",
        rows_per_intervention)

message("  Interventions per chunk: ",
        interventions_per_chunk)

message("  Rows per chunk: ",
        chunk_size)


# 4.3 Assign intervention identifiers ------------------------------------------

nd.pred <- 
  nd.pred %>%
  mutate(intervention_id = paste(c_air.temp,
                                 c_rel.hum,
                                 sep = "_"))


# 4.4 Split prediction grid into chunks ----------------------------------------

intervention_ids <- 
  unique(nd.pred$intervention_id)

chunk_ids <- 
  split(intervention_ids,
        ceiling(seq_along(intervention_ids) /
                  interventions_per_chunk))

nd_chunks <- 
  lapply(chunk_ids,
  
  function(ids) {
    
    nd.pred %>%
      filter(intervention_id %in% ids) %>%
      select(-intervention_id)
    
  }
  
)

# 4.5 Report number of chunks --------------------------------------------------

message("Total chunks: ",
        length(nd_chunks))


# ==============================================================================
# 5. Generate Predictions
# ==============================================================================

# 5.1 Loop over fitted models --------------------------------------------------

for (model_name in models_to_predict) {
  

  message("\nPredicting: ",
          model_name)

  
  # 5.1.1 Load fitted model ----------------------------------------------------
  
  fit <- 
    readRDS(file.path(
      fitted_model_folder,
      paste0(model_name,
             ".rds")))
  
  
  # 5.1.2 Define output folder -------------------------------------------------
  
  output_folder <- 
    file.path(prediction_folder,
              model_name,
              "chunks")
  
  if (!dir.exists(output_folder)) {
    
    dir.create(output_folder,
               recursive = TRUE)
    
  }
  
  
  # 5.1.3 Identify existing prediction chunks ----------------------------------
  
  chunk_files <- 
    list.files(output_folder,
               pattern = "epred.chunk_.*\\.rds$",
               full.names = TRUE)
  
  existing_chunks <- 
    as.integer(gsub(".*epred.chunk_(\\d+)\\.rds",
                    "\\1",
                    chunk_files))
  
  message("Already completed chunks: ",
          length(existing_chunks))
  
  
  # ============================================================================
  # 5.2 Prediction Loop
  # ============================================================================
  
  for (i in seq_along(nd_chunks)) {
    
    
    # Skip previously completed chunks -----------------------------------------
    
    if (i %in% existing_chunks) {
      
      message("\nSkipping chunk ",
              i)
      next
      
    }
    
    
    # Report prediction progress -----------------------------------------------
    
    message("\nProcessing chunk ",
            i,
            " of ",
            length(nd_chunks))
    
    
    # Generate sample-averaged predictions -------------------------------------

      epred_chunk <- 
        fit %>%
        epred_draws(newdata = nd_chunks[[i]],
                    seed = prediction_seed,
                    re_formula = NULL,
                    allow_new_levels = TRUE,
                    sample_new_levels = "uncertainty") %>%
        rename(vote = .category,
               prob = .epred)
    
    # Save prediction chunk ----------------------------------------------------
    
    saveRDS(epred_chunk,
            file.path(output_folder,
                      paste0("epred.chunk_",
                             i,
                             ".rds")))
    
    
    # Release memory -----------------------------------------------------------
    
    rm(epred_chunk)
    gc()
    
  }
  
  
  # Release fitted model from memory -------------------------------------------
  
  rm(fit)
  gc()
  
}

# ==============================================================================
# 6. Completion
# ==============================================================================

message("\nPrediction completed successfully.")
message("----------------------------------------\n")


# ==============================================================================
# 11. Unload Packages
# ==============================================================================

packages_to_unload <- 
  c("tidyverse",
    "brms",
    "tidybayes")

for (package in packages_to_unload) {
  
  if (paste0("package:", package) %in% search()) {
    
    detach(paste0("package:", package),
           unload = FALSE,
           character.only = TRUE)
    
  }
  
}


# ==============================================================================
# 7. Clean Workspace
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
