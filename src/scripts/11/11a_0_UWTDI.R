# ==============================================================================
# Script: 11a_0_UWTDI.R
#
# Description: Calculation of the Unweighted Thermal Discomfort Degree Index
#              (UWTDI) from posterior expected predictions across fitted models
#
# Author: Matteo Favero
# ==============================================================================

message("----------------------------------------")
message("Calculation UWTDI\n")


# ==============================================================================
# 0. Setup
# ==============================================================================

# 0.1 Check project root -------------------------------------------------------

if (!dir.exists("src/data")) {
  
  stop("Project root not detected. Please open the .Rproj file.")
  
}


# 0.2 Define models to process -------------------------------------------------

models_to_process <- models_to_run


# ==============================================================================
# 1. Process Models
# ==============================================================================

for (model_name in models_to_process) {
  
  message("Processing ",
          model_name)

  
# 1.1 Define input folder ------------------------------------------------------
  
  input_folder <- 
    file.path(prediction_folder,
              model_name,
              "chunks")
  
  
# 1.2 Check input folder -------------------------------------------------------
  
  if (!dir.exists(input_folder)) {
    
    stop("  Stop! Prediction folder (", input_folder, ") missing\n")
    
  }
  
# 1.3 Find prediction chunks ---------------------------------------------------
  
  chunk_files <-
    list.files(input_folder,
               pattern = "epred.chunk_.*\\.rds$",
               full.names = TRUE)
  
  
# 1.4 Check prediction chunks --------------------------------------------------
  
  if (length(chunk_files) == 0) {
    
    stop("- No prediction chunks found for ",
         model_name,
         " in ",
         input_folder)
    
  }
  
  message("(number of chunks: ",
          length(chunk_files), ")")
  
  
# 1.5 Define output folder -----------------------------------------------------

  output_folder <-
    file.path(prediction_folder,
              model_name,
              "cost_index")

  
# 1.6 Create output folder if necessary ----------------------------------------
  
  if (!dir.exists(output_folder)) {
    
    dir.create(output_folder,
               recursive = TRUE)
    
  }

  
# ==============================================================================
# 2. Process Cost Schemes
# ==============================================================================
  
# 2.1 Loop over cost schemes ---------------------------------------------------
  
  for (scheme_name in names(cost_schemes)) {
    
    output_file <- 
      file.path(output_folder,
                paste0("cost.epred_", scheme_name, ".rds"))
    
    
# 2.2 Skip existing calculations -----------------------------------------------
    
    if (file.exists(output_file)) {
      
      message("  - Skipping ", scheme_name, " scheme since calculations already exist")
      next
    }
    
    message("  - Processing ", scheme_name, " scheme")
  
      
# 2.3 Extract cost mapping -----------------------------------------------------
    
    cost_map <- cost_schemes[[scheme_name]]
    
    cost_list <- list()
    
    
# ==============================================================================
# 3. Process Prediction Chunks
# ==============================================================================
    
# 3.1 Loop over prediction chunks ----------------------------------------------
    
    for (i in seq_along(chunk_files)) {
      
      message("Reading chunk ",
              i,
              " of ",
              length(chunk_files))
      
      
# 3.2 Load prediction chunk ----------------------------------------------------
      
      epred_chunk <- readRDS(chunk_files[[i]])
      
      
# 3.3 Calculate posterior cost estimates ---------------------------------------
      
      cost_chunk <- 
        epred_chunk %>%
        ungroup() %>%
        select(-c(.row, .chain, .iteration, count, condition_id)) %>%
        mutate(cost.function = cost_map[vote],
               cost = cost.function * prob) %>%
        group_by(c_air.temp, c_rel.hum, sex, c_time.onset.h, time.day, .draw) %>%
        summarise(total.cost = sum(cost), .groups = "drop") %>%
        group_by(c_air.temp, c_rel.hum, sex, c_time.onset.h, time.day) %>%
        mean_qi(.width = 0.95) %>%
        ungroup()
      
      
# 3.4 Store chunk results ------------------------------------------------------     
      
      cost_list[[i]] <- cost_chunk
      
      
# 3.5 Clean chunk objects ------------------------------------------------------
      
      rm(epred_chunk, 
         cost_chunk)
      
      gc()
      
    }
    
    
# 3.6 Combine all processed chunks ---------------------------------------------
    
    cost.epred <- bind_rows(cost_list)

        

# 3.7 Back-transform air temperature, relative humidity and onset time ---------
    
    cost.epred <-
      cost.epred %>%
      mutate(air.temp = c_air.temp + air.temp_reference,
             rel.hum = (c_rel.hum + (rh_reference / rh_step)) * rh_step,
             time.onset.h =  c_time.onset.h + onset_time_reference)
    

# 3.8 Save cost estimates ------------------------------------------------------
    
    saveRDS(cost.epred, output_file)
    
    message("Saved:")
    message(output_file)
    
    
# 3.9 Clean scheme objects -----------------------------------------------------
    
    rm(cost.epred, 
       cost_list)
    
    gc()
    
  }
  
}


# ==============================================================================
# 4. Completion
# ==============================================================================

message("\nCalculation UWTDI completed")
message("----------------------------------------\n")
