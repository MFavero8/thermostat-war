# ==============================================================================
# Script: 09_average_thermal_sensation.R
#
# Description: Combine posterior expected predictions and calculate
#              average thermal sensation
#
#              Average thermal sensation:
#              TS = sum(probability * thermal_vote_score)
#
#
# Author: Matteo Favero
# ==============================================================================


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
})


# 0.3 Load configuration --------------------------------------------------------

before_section <- ls(all.names = TRUE) # Record what is currently in the environment

source("src/scripts/config.R")
source("src/scripts/parameters.R")
source("src/scripts/development_settings.R")
source("src/scripts/prediction_and_intervention_settings.R")


# ==============================================================================
# 1. Define Models to Process
# ==============================================================================

models_to_process <- models_to_run


# ==============================================================================
# 2. Process Models
# ==============================================================================

for (model_name in models_to_process) {
  
  message("Processing: ",
          model_name)
  
  # 2.1 Define prediction folders -----------------------------------------------
  
  input_folder <- 
    file.path(prediction_folder,
              model_name,
              "chunks")
  
  if (!dir.exists(input_folder)) {
    
    stop("Prediction folder missing:\n",
         input_folder)
    
  }
  
  output_folder <-
    file.path(prediction_folder,
              model_name,
              "pmv_comparison")
  
  if (!dir.exists(output_folder)) {
    
    dir.create(output_folder,
               recursive = TRUE)
    
  }
  
  output_file <-
    file.path(output_folder,
              "epred_mean_TSV.rds")
  
  
  # 2.2 Skip existing output ----------------------------------------------------
  
  if (file.exists(output_file)) {
    
    message("Output already exists. Skipping.")
    next
    
  }
  
  
  # 2.3 Find prediction chunks --------------------------------------------------
  
  chunk_files <-
    list.files(input_folder,
               pattern = "epred.chunk_.*\\.rds$",
               full.names = TRUE)
  
  if (length(chunk_files) == 0) {
    
    stop("No prediction chunks found for ",
         model_name,
         " in ",
         input_folder)
    
  }
  

  message("Number of chunks: ",
          length(chunk_files))
  
  
  # 2.4 Process prediction chunks -----------------------------------------------
  
  thermal_list <- list()
  
  for (i in seq_along(chunk_files)) {
    
    message("Reading chunk ",
            i,
            " of ",
            length(chunk_files))
    
    
    # Load prediction chunk
    
    epred_chunk <- readRDS(chunk_files[[i]])
    
    
    # Calculate average thermal sensation for each posterior draw
    
    thermal_chunk <-
      epred_chunk %>%
      mutate(vote_score = thermal_vote_scores[vote]) %>%
      group_by(c_air.temp,
               c_rel.hum,
               sex,
               time.day,
               c_time.onset.h,
               .draw) %>%
      summarise(mean.vote = sum(prob * vote_score),
                .groups = "drop") %>%
      group_by(c_air.temp,
               c_rel.hum,
               sex,
               time.day,
               c_time.onset.h) %>%
      mean_qi(.width = 0.95) %>%
      ungroup()
    
    
    # Store processed chunk
    
    thermal_list[[i]] <- thermal_chunk
    
    
    # Remove temporary objects
    
    rm(epred_chunk,
       thermal_chunk)
    
    gc()
    
  }
  
  
  # 2.5 Combine processed chunks ------------------------------------------------
  
  average_thermal_sensation <- bind_rows(thermal_list)
  

  # 2.6 Back-transform temperature, humidity and onset time ---------------------
  
  average_thermal_sensation <-
    average_thermal_sensation %>%
    mutate(air.temp = c_air.temp + air.temp_reference,
           rel.hum = (c_rel.hum + (rh_reference / rh_step)) * rh_step,
           time.onset.h =  c_time.onset.h + onset_time_reference)
  
  
  # 2.7 Save average thermal sensation ------------------------------------------
  
  saveRDS(average_thermal_sensation,
          output_file)
  
  
  message("Saved:")
  message(output_file)
  
  # Remove model-specific objects before processing the next model
  
  rm(thermal_list,
     average_thermal_sensation)
  
  gc()
  
}

message("Average thermal sensation calculation completed\n")


# ==============================================================================
# 3. Load Visualisation Scripts
# ==============================================================================

source("src/scripts/09/09a_1_visualisation_heatmap.R")
source("src/scripts/09/09a_2_visualisation_temperature.R")


# ==============================================================================
# 4. Unload Packages
# ==============================================================================

packages_to_unload <- 
  c("tidyverse",
    "tidybayes",
    "comf",
    "metR",
    "patchwork")

for (package in packages_to_unload) {
  
  if (paste0("package:", package) %in% search()) {
    
    detach(paste0("package:", package),
           unload = FALSE,
           character.only = TRUE)
    
  }
  
}


# ==============================================================================
# 5. Clean Workspace
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

