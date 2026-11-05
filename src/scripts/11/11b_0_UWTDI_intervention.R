# ==============================================================================
# Script: 11b_0_UWTDI_intervention.R
#
# Description: Calculation of the Unweighted Thermal Discomfort Degree Index
#              intervention distributions from posterior expected predictions
#              across fitted models and cost schemes
#
# Author: Matteo Favero
# ==============================================================================

message("----------------------------------------")
message("Calculation UWTDI intervention\n")


# ==============================================================================
# 0. Setup
# ==============================================================================

# 0.1 Define models to process -------------------------------------------------

models_to_process <- models_to_run


# ==============================================================================
# 1. Process Models
# ==============================================================================

for (model_name in models_to_process) {
  
  message("Processing ",
          model_name)
  
  
# 1.1 Define input prediction folder -------------------------------------------
  
  intput_prediction_folder <-
    file.path(prediction_folder,
              model_name,
              "intervention")

# 1.2 Define intervention prediction files -------------------------------------
  
  intput_file_heating <- 
    file.path(intput_prediction_folder,
              paste0("epred_", 
                     contrast_name_h, 
                     ".rds"))
  
  intput_file_cooling <- 
    file.path(intput_prediction_folder,
              paste0("epred_", 
                     contrast_name_c, 
                     ".rds"))
  
  
# 1.3 Check and load intervention prediction files -----------------------------
  
   if (file.exists(intput_file_heating) && 
      file.exists(intput_file_cooling)) {
     
     epred_filter_heating <- readRDS(intput_file_heating)
     epred_filter_cooling <- readRDS(intput_file_cooling)
      
   } else {
    
     stop("  Stop! Input files do not exist.")
     
     }
  
  
# 1.4 Define output prediction folder ------------------------------------------
  
  output_prediction_folder <-
    file.path(prediction_folder,
              model_name,
              "cost_index_intervention_distribution")

    
# 1.5 Create output folder if necessary ----------------------------------------
  
  if (!dir.exists(output_prediction_folder)) {
    
    dir.create(output_prediction_folder,
               recursive = TRUE)
    
  }
    
  
# ==============================================================================
# 2. Process Cost Schemes
# ==============================================================================
  
# 2.1 Define input cost folder -------------------------------------------------
  
  input_cost_folder <- 
    file.path(prediction_folder,
              model_name,
              "cost_index")
  
  
# 2.2 Find cost files ---------------------------------------------------------- 
  
  cost_files <- 
    list.files(input_cost_folder,
               pattern = "^cost\\.epred_.*\\.rds$",
               full.names = TRUE)

    
# 2.3 Loop over cost schemes ---------------------------------------------------
  
  for (cost_file in cost_files) {
    
    
# 2.4 Extract cost scheme name -------------------------------------------------    
    
    scheme_name <- sub("cost\\.epred_(.*)\\.rds",
                       "\\1",
                       basename(cost_file))  

        
# 2.5 Define output files ------------------------------------------------------
    
    output_file_heating_cost_distr <- 
      file.path(output_prediction_folder,
                paste0("cost.heating.epred_", scheme_name, ".rds"))
    
    output_file_heating_cost_distr.diff <- 
      file.path(output_prediction_folder,
                paste0("cost.heating.diff.epred_", scheme_name, ".rds"))
    
    
    output_file_cooling_cost_distr <- 
      file.path(output_prediction_folder,
                paste0("cost.cooling.epred_", scheme_name, ".rds"))
    
    output_file_cooling_cost_distr.diff <- 
      file.path(output_prediction_folder,
                paste0("cost.cooling.diff.epred_", scheme_name, ".rds"))
    
    
# 2.6 Skip existing calculations -----------------------------------------------
    
    if (file.exists(output_file_heating_cost_distr) && 
        file.exists(output_file_heating_cost_distr.diff) && 
        file.exists(output_file_cooling_cost_distr) && 
        file.exists(output_file_cooling_cost_distr.diff)
        ) {
      
      message("  - Skipping ", scheme_name, " scheme since calculations already exist")
      next
      
    }
    
    message("  - Processing ", scheme_name, " scheme")

        
# 2.7 Extract cost mapping -----------------------------------------------------    
    
    cost_map <- cost_schemes[[scheme_name]]
    
    
# ==============================================================================
# 3. Calculate Heating Intervention Costs
# ==============================================================================
    
# 3.1 Calculate posterior heating cost distributions ---------------------------
    
    heating_cost_distr <- epred_filter_heating %>%
      ungroup() %>%
      select(-c(.row, .chain, .iteration, count, condition_id)) %>%
      mutate(cost.function = cost_map[vote],
             cost = cost.function * prob) %>%
      group_by(air.temp, rel.hum, sex, time.day, time.onset.h, .draw) %>%
      summarise(total.cost = sum(cost), .groups = "drop") 

        
# 3.2 Calculate heating intervention cost differences --------------------------
    
    heating_cost_distr.diff <- heating_cost_distr %>%
      pivot_wider(names_from =  c(air.temp, rel.hum),
                  values_from = "total.cost",
                  names_glue = "{air.temp}t.{rel.hum}rh") %>%
      mutate(!!contrast_name_h := .data[[paste0(temp_values_h[2], "t.", rh_values_h[2], "rh")]] - 
               .data[[paste0(temp_values_h[1], "t.", rh_values_h[1], "rh")]]) %>%
      select(c(sex, time.onset.h, time.day, .draw, all_of(contrast_name_h))) %>%
      mutate(setpoint = paste("a","\u2212", "a'"))
    
    
# ==============================================================================
# 4. Calculate Cooling Intervention Costs
# ==============================================================================
    
# 4.1 Calculate posterior cooling cost distributions ---------------------------
    
    cooling_cost_distr <- epred_filter_cooling %>%
      ungroup() %>%
      select(-c(.row, .chain, .iteration, count, condition_id)) %>%
      mutate(cost.function = cost_map[vote],
             cost = cost.function * prob) %>%
      group_by(air.temp, rel.hum, sex, time.day, time.onset.h, .draw) %>%
      summarise(total.cost = sum(cost), .groups = "drop") 
    
    
# 4.2 Calculate cooling intervention cost differences --------------------------
    
    cooling_cost_distr.diff <- cooling_cost_distr %>%
      pivot_wider(names_from =  c(air.temp, rel.hum),
                  values_from = "total.cost",
                  names_glue = "{air.temp}t.{rel.hum}rh") %>%
      mutate(!!contrast_name_c := .data[[paste0(temp_values_c[2], "t.", rh_values_c[2], "rh")]] - 
               .data[[paste0(temp_values_c[1], "t.", rh_values_c[1], "rh")]]) %>%
      select(c(sex, time.onset.h, time.day, .draw, all_of(contrast_name_c))) %>%
      mutate(setpoint = paste("a","\u2212", "a'"))
    
    
# ==============================================================================
# 5. Save Results
# ==============================================================================
  
# 5.1 Save heating cost distributions ------------------------------------------
    
    saveRDS(heating_cost_distr, output_file_heating_cost_distr)
    saveRDS(heating_cost_distr.diff, output_file_heating_cost_distr.diff)
    
    
# 5.2 Save cooling cost distributions ------------------------------------------
    
    saveRDS(cooling_cost_distr, output_file_cooling_cost_distr)
    saveRDS(cooling_cost_distr.diff, output_file_cooling_cost_distr.diff)
    
    
    message("    ", scheme_name," scheme saved")
    
    
# 5.3 Clean scheme objects -----------------------------------------------------    
    
    rm(heating_cost_distr, 
       heating_cost_distr.diff,
       cooling_cost_distr, 
       cooling_cost_distr.diff)
    
    gc()
    
  }
  
} 

# ==============================================================================
# 6. Completion
# ==============================================================================

message("\nCalculation UWTDI intervention completed")
message("----------------------------------------\n")
