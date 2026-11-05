# ==============================================================================
# Script: 10a_risk_prediction.R
#
# Description:
#   Calculation of risk predictions from expected posterior predictions
#   for heating and cooling intervention contrasts.
#
# Author: Matteo Favero
# ==============================================================================

message("\n----------------------------------------")
message("Calculation risk prediction\n")


# ==============================================================================
# 0. Loop over Models
# ==============================================================================

for (model_name in models_to_process) {
  
  message("Processing ",
          model_name)
  
  # ============================================================================
  # 1. Define Output Folders
  # ============================================================================
  
  # 1.1 Define table folder ----------------------------------------------------
  
  output_table_folder <-
    file.path(table_folder,
              model_name,
              "risk_prediction")
  
  if (!dir.exists(output_table_folder)) {
    
    dir.create(output_table_folder,
               recursive = TRUE)
    
  }
  
  
  # 1.2 Define prediction folder -----------------------------------------------
  
  output_prediction_folder <-
    file.path(prediction_folder,
              model_name,
              "intervention")

  
  # ============================================================================
  # 2. Define Output Files
  # ============================================================================
  
  output_file_heating <- 
    file.path(output_prediction_folder,
              paste0("risk_pred_", 
                     contrast_name_h, 
                     ".rds"))
  
  output_file_cooling <- 
    file.path(output_prediction_folder,
              paste0("risk_pred_", 
                     contrast_name_c, 
                     ".rds"))
  
  
  # ============================================================================
  # 3. Calculate Risk Predictions
  # ============================================================================
  
  if (file.exists(output_file_heating) && 
      file.exists(output_file_cooling)) {
    
    message("  - Skipping since calculations already exist. Creating only excel file")
    
    # Load risk predictions ----------------------------------------------------
    
    risk.pred_heating <- readRDS(output_file_heating)
    risk.pred_cooling <- readRDS(output_file_cooling)
    
  
    # Save heating risk prediction table ---------------------------------------
    
    risk.pred_heating <-
      risk.pred_heating %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(air.temp,
             rel.hum,
             sex,
             time.onset.h,  
             time.day, 
             vote, 
             prob,    
             `.lower`,   
             `.upper`,       
             `.width`,      
             `.point`,  
             `.interval`,      
             facet_setpoint)
    
    write.csv(risk.pred_heating, 
              file.path(output_table_folder,
                        paste0("risk.pred_heating_",
                               contrast_name_h, 
                               ".csv")))
    
    
    # Save cooling risk prediction table ---------------------------------------
    
    risk.pred_cooling <-
      risk.pred_cooling %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(air.temp,
             rel.hum,
             sex,
             time.onset.h,  
             time.day, 
             vote, 
             prob,        
             `.lower`,    
             `.upper`,    
             `.width`,    
             `.point`,    
             `.interval`, 
             facet_setpoint)
    
    write.csv(risk.pred_cooling,
              file.path(output_table_folder,
                        paste0("risk.pred_cooling_",
                               contrast_name_c, 
                               ".csv")))
    
    # Remove intermediate sex effect modification objects
    
    rm(risk.pred_heating, risk.pred_cooling)
    
    
    
  } else {
    
    message("  - Files not found. Calculating now")
    
    
    # 3.1 Define input prediction folder ---------------------------------------
    
    input_prediction_folder <-
      file.path(prediction_folder,
                model_name,
                "intervention")
    
    
    # 3.2 Define input prediction files ----------------------------------------
    
    input_file_heating <- 
      file.path(input_prediction_folder,
                paste0("epred_", 
                       contrast_name_h, 
                       ".rds"))
    
    input_file_cooling <- 
      file.path(input_prediction_folder,
                paste0("epred_", 
                       contrast_name_c, 
                       ".rds"))
    
    
    # 3.3 Load expected posterior predictions ----------------------------------
    
    epred_filter_heating <- readRDS(input_file_heating)
    epred_filter_cooling <- readRDS(input_file_cooling)
    
    
    # 3.4 Calculate heating risk prediction ------------------------------------
    
    risk.pred_heating <- 
      epred_filter_heating %>%
      ungroup() %>%
      select(-c(.row, .chain, .iteration, count, condition_id)) %>%
      group_by(c_air.temp, c_rel.hum, sex, c_time.onset.h, time.day, vote, air.temp, rel.hum, time.onset.h) %>%
      mean_qi(.width = 0.95) %>%
      mutate(facet_setpoint = case_when(air.temp == temp_values_h[1] & rel.hum == rh_values_h[1] ~ paste0("a' = (", air.temp, "°C", ", ", rel.hum, "%rh)"),
                                        air.temp == temp_values_h[2] & rel.hum == rh_values_h[2] ~ paste0("a = (", air.temp, "°C", ", ", rel.hum, "%rh)"),
                                        TRUE ~ NA_character_))
    
    
    # 3.5 Calculate cooling risk prediction ------------------------------------
    
    risk.pred_cooling <- 
      epred_filter_cooling %>%
      ungroup() %>%
      select(-c(.row, .chain, .iteration, count, condition_id)) %>%
      group_by(c_air.temp, c_rel.hum, sex, c_time.onset.h, time.day, vote, air.temp, rel.hum, time.onset.h) %>%
      mean_qi(.width = 0.95) %>%
      mutate(facet_setpoint = case_when(air.temp == temp_values_c[1] & rel.hum == rh_values_c[1] ~ paste0("a' = (", air.temp, "°C", ", ", rel.hum, "%rh)"),
                                        air.temp == temp_values_c[2] & rel.hum == rh_values_c[2] ~ paste0("a = (", air.temp, "°C", ", ", rel.hum, "%rh)"),
                                        TRUE ~ NA_character_))
  
  
    # ==========================================================================
    # 4. Save Risk Predictions
    # ==========================================================================

    # 4.1 Save heating risk predictions ----------------------------------------
    
    saveRDS(risk.pred_heating, output_file_heating)
    
    
    # 4.2 Save cooling risk predictions ----------------------------------------
    
    saveRDS(risk.pred_cooling, output_file_cooling)
  
    
    # ==========================================================================
    # 5. Save Risk Prediction Tables
    # ==========================================================================
    
    # 5.1 Save heating risk prediction table ----------------------------------
  
    risk.pred_heating <-
      risk.pred_heating %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(air.temp,
             rel.hum,
             sex,
             time.onset.h,  
             time.day, 
             vote, 
             prob,    
             `.lower`,   
             `.upper`,       
             `.width`,      
             `.point`,  
             `.interval`,      
             facet_setpoint)
    
    write.csv(risk.pred_heating, 
              file.path(output_table_folder,
                        paste0("risk.pred_heating_",
                               contrast_name_h, 
                               ".csv")))
    
    
    # 5.2 Save cooling risk prediction table -----------------------------------
    
      risk.pred_cooling <-
        risk.pred_cooling %>%
        mutate(vote = thermal_vote_scores[vote]) %>%
        select(air.temp,
               rel.hum,
               sex,
               time.onset.h,  
               time.day, 
               vote, 
               prob,        
               `.lower`,    
               `.upper`,    
               `.width`,    
               `.point`,    
               `.interval`, 
               facet_setpoint)
      
    write.csv(risk.pred_cooling,
              file.path(output_table_folder,
                        paste0("risk.pred_cooling_",
                               contrast_name_c, 
                               ".csv")))
    
    
    # ==========================================================================
    # 6. Clean Model-Specific Objects
    # ==========================================================================
    
    rm(risk.pred_heating, risk.pred_cooling)
  
  }
}  


# ==============================================================================
# 7. Completion
# ==============================================================================

message("\nCalculation risk prediction completed")
message("----------------------------------------\n")
