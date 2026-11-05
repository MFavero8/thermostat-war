# ==============================================================================
# Script: 10b_1_risk_difference.R
#
# Description:
#   Calculation of conditional average treatment effects (CATE) expressed as
#   risk differences for heating and cooling interventions, including
#   directional posterior probabilities.
#
# Author: Matteo Favero
# ==============================================================================

message("----------------------------------------")
message("Calculation risk difference (CATE)\n")

# ==============================================================================
# 0. Loop Over Models
# ==============================================================================

for (model_name in models_to_process) {
  
  message("Processing ",
          model_name)
  
  # ============================================================================
  # 1. Define Output Folders and Files
  # ============================================================================
  
  # 1.1 Define table output folder ---------------------------------------------
  
  output_table_folder <-
    file.path(table_folder,
              model_name,
              "risk_difference")
  
  if (!dir.exists(output_table_folder)) {
    
    dir.create(output_table_folder,
               recursive = TRUE)
    
  }
  
  # 1.2 Define prediction output folder ----------------------------------------
  
  output_prediction_folder <-
    file.path(prediction_folder,
              model_name,
              "intervention")
  
  
  # 1.3 Define risk difference output files ------------------------------------
  
  output_file_heating <- 
    file.path(output_prediction_folder,
              paste0("risk_diff_", 
                     contrast_name_h, 
                     ".rds"))
  
  output_file_cooling <- 
    file.path(output_prediction_folder,
              paste0("risk_diff_", 
                     contrast_name_c, 
                     ".rds"))
  
  
  # 1.4 Define directional probability output files ----------------------------
  
  output_file_heating_direction <- 
    file.path(output_prediction_folder,
              paste0("risk_diff_direction_", 
                     contrast_name_h, 
                     ".rds"))
  
  output_file_cooling_direction <- 
    file.path(output_prediction_folder,
              paste0("risk_diff_direction_", 
                     contrast_name_c, 
                     ".rds"))
  
  # ============================================================================
  # 2. Calculate Risk Differences
  # ============================================================================
  
  # 2.1 Check whether calculations already exist -------------------------------
  
  if (file.exists(output_file_heating) && 
      file.exists(output_file_cooling) && 
      file.exists(output_file_heating_direction) && 
      file.exists(output_file_cooling_direction)) {
    
    message("  - Skipping since calculations already exist. Creating only excel file")
    
  
    # Load risk difference predictions -----------------------------------------
    
    risk.diff_heating <- readRDS(output_file_heating)
    risk.diff_cooling <- readRDS(output_file_cooling)
    
    
    # Load directional posterior probabilities ---------------------------------
    
    risk.diff_heating_direction <- readRDS(output_file_heating_direction)
    risk.diff_cooling_direction <- readRDS(output_file_cooling_direction)
    

    # Save heating risk difference table ---------------------------------------
    
    risk.diff_heating <-
      risk.diff_heating %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint))
    
    write.csv(risk.diff_heating,
              file.path(output_table_folder,
                        paste0("risk.diff_heating_", 
                               contrast_name_h, 
                               ".csv")),
              row.names = FALSE)
    
    
    # Save cooling risk difference table ---------------------------------------
    
    risk.diff_cooling <-
      risk.diff_cooling %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint))
    
    write.csv(risk.diff_cooling,
              file.path(output_table_folder,
                        paste0("risk.diff_cooling_", 
                               contrast_name_c, 
                               ".csv")),
              row.names = FALSE)
    
    
    # Save heating directional probability table -------------------------------
    
    risk.diff_heating_direction <-
      risk.diff_heating_direction %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint))
    
    write.csv(risk.diff_heating_direction,
              file.path(output_table_folder,
                        paste0("risk.diff_heating_direction_", 
                               contrast_name_h, 
                               ".csv")),
              row.names = FALSE)
    
    
    # Save cooling directional probability table -------------------------------
    
    risk.diff_cooling_direction <-
      risk.diff_cooling_direction %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint))
    
    write.csv(risk.diff_cooling_direction,
              file.path(output_table_folder,
                        paste0("risk.diff_cooling_direction_", 
                               contrast_name_c, 
                               ".csv")),
              row.names = FALSE)
    
    
    # Remove intermediate sex effect modification objects
    
    rm(risk.diff_heating, 
       risk.diff_cooling, 
       risk.diff_heating_direction, 
       risk.diff_cooling_direction)
    
  } else {
    
    message("  - Files not found. Calculating now")
    
    
    # 2.2 Define input prediction files ----------------------------------------

    input_prediction_folder <-
      file.path(prediction_folder,
                model_name,
                "intervention")
    
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
    

    # 2.3 Load expected posterior predictions ----------------------------------

    epred_filter_heating <- readRDS(input_file_heating)
    epred_filter_cooling <- readRDS(input_file_cooling)    

    
    # 2.4 Calculate heating risk difference ------------------------------------

    risk.diff_heating <-
      epred_filter_heating %>%
      ungroup() %>%
      select(-c(c_air.temp, c_rel.hum, .row, .chain, .iteration, count, condition_id)) %>%
      pivot_wider(names_from =  c(air.temp, rel.hum),
                  values_from = "prob",
                  names_glue = "{air.temp}t{rel.hum}rh") %>%
      mutate(!!contrast_name_h := .data[[paste0(temp_values_h[2], "t", rh_values_h[2], "rh")]] - 
               .data[[paste0(temp_values_h[1], "t", rh_values_h[1], "rh")]]) %>%
      select(c(sex, time.onset.h, time.day, .draw, vote, all_of(contrast_name_h))) %>%
      group_by(sex, time.onset.h, time.day, vote) %>%
      mean_qi(.width = 0.95) %>%
      ungroup() %>%
      mutate(setpoint = "Heating setpoint")
    
    
    # 2.5 Calculate cooling risk difference ------------------------------------

    risk.diff_cooling <-
      epred_filter_cooling %>%
      ungroup() %>%
      select(-c(c_air.temp, c_rel.hum, .row, .chain, .iteration, count, condition_id)) %>%
      pivot_wider(names_from =  c(air.temp, rel.hum),
                  values_from = "prob",
                  names_glue = "{air.temp}t{rel.hum}rh") %>%
      mutate(!!contrast_name_c := .data[[paste0(temp_values_c[2], "t", rh_values_c[2], "rh")]] - 
               .data[[paste0(temp_values_c[1], "t", rh_values_c[1], "rh")]]) %>%
      select(c(sex, time.onset.h, time.day, .draw, vote, all_of(contrast_name_c))) %>%
      group_by(sex, time.onset.h, time.day, vote) %>%
      mean_qi(.width = 0.95) %>%
      ungroup() %>%
      mutate(setpoint = "Cooling setpoint")
    
    
    # ==========================================================================
    # 3. Calculate Directional Posterior Probabilities
    # ==========================================================================
    
    # 3.1 Heating directional posterior probabilities --------------------------
    
    risk.diff_heating_direction <-
      epred_filter_heating %>%
      ungroup() %>%
      select(-c(c_air.temp, c_rel.hum, .row, .chain, .iteration, count, condition_id)) %>%
      pivot_wider(names_from =  c(air.temp, rel.hum),
                  values_from = "prob",
                  names_glue = "{air.temp}t{rel.hum}rh") %>%
      mutate(!!contrast_name_h := .data[[paste0(temp_values_h[2], "t", rh_values_h[2], "rh")]] - 
               .data[[paste0(temp_values_h[1], "t", rh_values_h[1], "rh")]]) %>%
      select(c(sex, time.onset.h, time.day, .draw, vote, all_of(contrast_name_h))) %>%
      group_by(sex, time.onset.h, time.day, vote) %>%
      summarise(pr_gt_0 = mean(.data[[contrast_name_h]] > 0), 
                pr_lt_0 = mean(.data[[contrast_name_h]] < 0), 
                .groups = "drop") %>%
      mutate(setpoint = "Heating setpoint")
    
    
    # 3.2 Cooling directional posterior probabilities --------------------------
    
    risk.diff_cooling_direction <-
      epred_filter_cooling %>%
      ungroup() %>%
      select(-c(c_air.temp, c_rel.hum, .row, .chain, .iteration, count, condition_id)) %>%
      pivot_wider(names_from =  c(air.temp, rel.hum),
                  values_from = "prob",
                  names_glue = "{air.temp}t{rel.hum}rh") %>%
      mutate(!!contrast_name_c := .data[[paste0(temp_values_c[2], "t", rh_values_c[2], "rh")]] - 
               .data[[paste0(temp_values_c[1], "t", rh_values_c[1], "rh")]]) %>%
      select(c(sex, time.onset.h, time.day, .draw, vote, all_of(contrast_name_c))) %>%
      group_by(sex, time.onset.h, time.day, vote) %>%
      summarise(pr_gt_0 = mean(.data[[contrast_name_c]] > 0), 
                pr_lt_0 = mean(.data[[contrast_name_c]] < 0), 
                .groups = "drop") %>%
      mutate(setpoint = "Cooling setpoint")
    
    
    # ==========================================================================
    # 4. Save Prediction Results
    # ==========================================================================
    
    # 4.1 Save risk difference predictions -------------------------------------
    
    saveRDS(risk.diff_heating, output_file_heating)
    saveRDS(risk.diff_cooling, output_file_cooling)
    
    
    # 4.2 Save directional posterior probabilities -----------------------------
    
    saveRDS(risk.diff_heating_direction, output_file_heating_direction)
    saveRDS(risk.diff_cooling_direction, output_file_cooling_direction)
    
    
    # ==========================================================================
    # 5. Save Risk Difference Tables
    # ==========================================================================
    
    # 5.1 Save heating risk difference table -----------------------------------
    
    risk.diff_heating <-
      risk.diff_heating %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint))
    
    write.csv(risk.diff_heating,
              file.path(output_table_folder,
                        paste0("risk.diff_heating_", 
                               contrast_name_h, 
                               ".csv")),
              row.names = FALSE)
    
    
    # 5.2 Save cooling risk difference table -----------------------------------
    
    risk.diff_cooling <-
      risk.diff_cooling %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint))
    
    write.csv(risk.diff_cooling,
              file.path(output_table_folder,
                        paste0("risk.diff_cooling_", 
                               contrast_name_c, 
                               ".csv")),
              row.names = FALSE)
    
    
    # ==========================================================================
    # 6. Save Directional Probability Tables
    # ==========================================================================
    
    # 6.1 Save heating directional probability table ---------------------------
    
    risk.diff_heating_direction <-
      risk.diff_heating_direction %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint))
    
    write.csv(risk.diff_heating_direction,
              file.path(output_table_folder,
                        paste0("risk.diff_heating_direction_", 
                               contrast_name_h, 
                               ".csv")),
              row.names = FALSE)
    
    
    # 6.2 Save cooling directional probability table ---------------------------
    
    risk.diff_cooling_direction <-
      risk.diff_cooling_direction %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint))
    
    write.csv(risk.diff_cooling_direction,
              file.path(output_table_folder,
                        paste0("risk.diff_cooling_direction_", 
                               contrast_name_c, 
                               ".csv")),
              row.names = FALSE)
    
    
    # ==========================================================================
    # 7. Clean Intermediate Objects
    # ==========================================================================
    
    rm(risk.diff_heating, 
       risk.diff_cooling, 
       risk.diff_heating_direction, 
       risk.diff_cooling_direction)
    
  }
}


# ==============================================================================
# 8. Completion
# ==============================================================================

message("\nCalculation risk difference (CATE) completed")
message("----------------------------------------\n")