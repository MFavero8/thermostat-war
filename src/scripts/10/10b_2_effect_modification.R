# ==============================================================================
# Script: 10b_2_effect_modification.R
#
# Description: Calculation of effect modification using differences in CATE
#
# Author: Matteo Favero
# ==============================================================================

message("----------------------------------------")
message("Calculation effect modification (difference in CATE)\n")

# ==============================================================================
# 0. Loop over Models
# ==============================================================================

for (model_name in models_to_process) {
  
  message("Processing ",
          model_name)
  
  

  # 0.1 Define Output Table Folder ---------------------------------------------

  output_table_folder <-
    file.path(table_folder,
              model_name,
              "effect_modification")
  
  if (!dir.exists(output_table_folder)) {
    
    dir.create(output_table_folder,
               recursive = TRUE)
    
  }
  
  
  # 0.2 Define Prediction Folder -----------------------------------------------
  
  output_prediction_folder <-
    file.path(prediction_folder,
              model_name,
              "intervention")
  
  
  # 0.3 Define Output Files for Sex Effect Modification ------------------------
  
  output_file_sex_heating <- 
    file.path(output_prediction_folder,
              paste0("eff_mod_sex_", 
                     contrast_name_h, 
                     ".rds"))
  
  output_file_sex_cooling <- 
    file.path(output_prediction_folder,
              paste0("eff_mod_sex_", 
                     contrast_name_c, 
                     ".rds"))
  
  output_file_sex_heating_direction <- 
    file.path(output_prediction_folder,
              paste0("eff_mod_sex_direction_", 
                     contrast_name_h, 
                     ".rds"))
  
  output_file_sex_cooling_direction <- 
    file.path(output_prediction_folder,
              paste0("eff_mod_sex_direction_", 
                     contrast_name_c, 
                     ".rds"))
  
  
  # 0.4 Define Output Files for Day Effect Modification ------------------------
  
  output_file_day_heating <- 
    file.path(output_prediction_folder,
              paste0("eff_mod_day_", 
                     contrast_name_h, 
                     ".rds"))
  
  output_file_day_cooling <- 
    file.path(output_prediction_folder,
              paste0("eff_mod_day_", 
                     contrast_name_c, 
                     ".rds"))
  
  output_file_day_heating_direction <- 
    file.path(output_prediction_folder,
              paste0("eff_mod_day_direction_", 
                     contrast_name_h, 
                     ".rds"))
  
  output_file_day_cooling_direction <- 
    file.path(output_prediction_folder,
              paste0("eff_mod_day_direction_", 
                     contrast_name_c, 
                     ".rds"))

  
  # 0.5 Check Whether Calculations Already Exist -------------------------------
  
  if (file.exists(output_file_sex_heating) && 
      file.exists(output_file_sex_cooling) && 
      file.exists(output_file_sex_heating_direction) && 
      file.exists(output_file_sex_cooling_direction) &&
      
      file.exists(output_file_day_heating) && 
      file.exists(output_file_day_cooling) && 
      file.exists(output_file_day_heating_direction) && 
      file.exists(output_file_day_cooling_direction)) {
    
    message("  - Skipping since calculations already exist. Creating only excel file")
    
    
    # Load Sex Predictions -----------------------------------------------------
    
    eff.mod_sex_heating <- readRDS(output_file_sex_heating)
    eff.mod_sex_cooling <- readRDS(output_file_sex_cooling)
    
    eff.mod_sex_heating_direction <-readRDS(output_file_sex_heating_direction)
    eff.mod_sex_cooling_direction <- readRDS(output_file_sex_cooling_direction)
    
    
    # Save Sex Effect Modification Tables --------------------------------------
    
    eff.mod_sex_heating <-
      eff.mod_sex_heating %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint, diff))
    
    write.csv(eff.mod_sex_heating,
              file.path(output_table_folder,
                        paste0("eff.mod_sex_heating_",
                               contrast_name_h, 
                               ".csv")),
              row.names = FALSE)
    
    eff.mod_sex_cooling <-
      eff.mod_sex_cooling %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint, diff))
    
    write.csv(eff.mod_sex_cooling,
              file.path(output_table_folder,
                        paste0("eff.mod_sex_cooling_", 
                               contrast_name_c, 
                               ".csv")),
              row.names = FALSE)
    
    
    # Directional posterior probabilities
    
    eff.mod_sex_heating_direction <-
      eff.mod_sex_heating_direction %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint, diff))
    
    write.csv(eff.mod_sex_heating_direction,
              file.path(output_table_folder,
                        paste0("eff.mod_sex_heating_direction_", 
                               contrast_name_h, 
                               ".csv")),
              row.names = FALSE)
    
    eff.mod_sex_cooling_direction <-
      eff.mod_sex_cooling_direction %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint, diff))
    
    write.csv(eff.mod_sex_cooling_direction,
              file.path(output_table_folder,
                        paste0("eff.mod_sex_cooling_direction_", 
                               contrast_name_c, 
                               ".csv")),
              row.names = FALSE)
    
    
    # Remove intermediate sex effect modification objects
    
    rm(eff.mod_sex_heating,
       eff.mod_sex_cooling,
       eff.mod_sex_heating_direction,
       eff.mod_sex_cooling_direction)
    
    
    # Load Day Predictions -----------------------------------------------------
    
    eff.mod_day_heating <- readRDS(output_file_day_heating)
    eff.mod_day_cooling <- readRDS(output_file_day_cooling)
    
    eff.mod_day_heating_direction <- readRDS(output_file_day_heating_direction)
    eff.mod_day_cooling_direction <- readRDS(output_file_day_cooling_direction)
    
    
    # Save Day Effect Modification Tables --------------------------------------
    
    eff.mod_day_heating <-
      eff.mod_day_heating %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint, diff)) 
    
    write.csv(eff.mod_day_heating,
              file.path(output_table_folder,
                        paste0("eff.mod_day_heating_", 
                               contrast_name_h, 
                               ".csv")),
              row.names = FALSE)
    
    eff.mod_day_cooling <-
      eff.mod_day_cooling %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint, diff)) 
    
    write.csv(eff.mod_day_cooling,
              file.path(output_table_folder,
                        paste0("eff.mod_day_cooling_", 
                               contrast_name_c, 
                               ".csv")),
              row.names = FALSE)
    
    
    # Directional posterior probabilities
    
    eff.mod_day_heating_direction <-
      eff.mod_day_heating_direction %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint, diff)) 
    
    write.csv(eff.mod_day_heating_direction,
              file.path(output_table_folder,
                        paste0("eff.mod_day_heating_direction_", 
                               contrast_name_h, 
                               ".csv")),
              row.names = FALSE)
    
    eff.mod_day_cooling_direction <-
      eff.mod_day_cooling_direction %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint, diff)) 
    
    write.csv(eff.mod_day_cooling_direction,
              file.path(output_table_folder,
                        paste0("eff.mod_day_cooling_direction_", 
                               contrast_name_c, 
                               ".csv")),
              row.names = FALSE)
    
    
    # Remove intermediate day effect modification objects
    
    rm(eff.mod_day_heating,
       eff.mod_day_cooling,
       eff.mod_day_heating_direction,
       eff.mod_day_cooling_direction)
    
  } else {
    
    message("  - Files not found. Calculating now")
    
    
    # 0.6 Define Input Prediction Files ----------------------------------------
    
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
    
    epred_filter_heating <- readRDS(input_file_heating)
    epred_filter_cooling <- readRDS(input_file_cooling)    
    
    
    # ==========================================================================
    # 1. Effect Modification by Sex
    # ==========================================================================
    
    # 1.1 Calculate Sex Effect Modification for Heating ------------------------
    
    eff.mod_sex_heating <- 
      epred_filter_heating %>%
      ungroup() %>%
      select(-c(c_air.temp, c_rel.hum, .row, .chain, .iteration, count, condition_id)) %>%
      pivot_wider(names_from =  c(air.temp, rel.hum),
                  values_from = "prob",
                  names_glue = "{air.temp}t{rel.hum}rh") %>%
      mutate(!!contrast_name_h := .data[[paste0(temp_values_h[2], "t", rh_values_h[2], "rh")]] -
               .data[[paste0(temp_values_h[1], "t", rh_values_h[1], "rh")]]) %>%
      select(c(sex, time.onset.h, time.day, .draw, vote, all_of(contrast_name_h))) %>%
      pivot_wider(names_from = sex,
                  values_from = all_of(contrast_name_h)) %>%
      mutate(EM.sex_M.F = male - female) %>%
      select(-c(male, female)) %>%
      group_by(time.onset.h, time.day, vote) %>%
      mean_qi(.width = 0.95) %>%
      ungroup() %>%
      mutate(setpoint = "Heating setpoint", 
             diff = paste0("Male ", "\u2212", " Female"))
    
    
    # 1.2 Calculate Sex Effect Modification for Cooling ------------------------
    
    eff.mod_sex_cooling <- 
      epred_filter_cooling %>%
      ungroup() %>%
      select(-c(c_air.temp, c_rel.hum, .row, .chain, .iteration, count, condition_id)) %>%
      pivot_wider(names_from =  c(air.temp, rel.hum),
                  values_from = "prob",
                  names_glue = "{air.temp}t{rel.hum}rh") %>%
      mutate(!!contrast_name_c := .data[[paste0(temp_values_c[2], "t", rh_values_c[2], "rh")]] -
               .data[[paste0(temp_values_c[1], "t", rh_values_c[1], "rh")]]) %>%
      select(c(sex, time.onset.h, time.day, .draw, vote, all_of(contrast_name_c))) %>%
      pivot_wider(names_from = sex,
                  values_from = all_of(contrast_name_c)) %>%
      mutate(EM.sex_M.F = male - female) %>%
      select(-c(male, female)) %>%
      group_by(time.onset.h, time.day, vote) %>%
      mean_qi(.width = 0.95) %>%
      ungroup() %>%
      mutate(setpoint = "Cooling setpoint", 
             diff = paste0("Male ", "\u2212", " Female"))
    
    
    # 1.3 Calculate Directional Posterior Probabilities for Sex ----------------
    
    # Heating setpoint
    
    eff.mod_sex_heating_direction <- 
      epred_filter_heating %>%
      ungroup() %>%
      select(-c(c_air.temp, c_rel.hum, .row, .chain, .iteration, count, condition_id)) %>%
      pivot_wider(names_from =  c(air.temp, rel.hum),
                  values_from = "prob",
                  names_glue = "{air.temp}t{rel.hum}rh") %>%
      mutate(!!contrast_name_h := .data[[paste0(temp_values_h[2], "t", rh_values_h[2], "rh")]] -
               .data[[paste0(temp_values_h[1], "t", rh_values_h[1], "rh")]]) %>%
      select(c(sex, time.onset.h, time.day, .draw, vote, all_of(contrast_name_h))) %>%
      pivot_wider(names_from = sex,
                  values_from = all_of(contrast_name_h)) %>%
      mutate(EM.sex_M.F = male - female) %>%
      select(-c(male, female)) %>%
      group_by(time.onset.h, time.day, vote) %>%
      summarise(pr_gt_0 = mean(EM.sex_M.F > 0), 
                pr_lt_0 = mean(EM.sex_M.F < 0), 
                .groups = "drop") %>%
      mutate(setpoint = "Heating setpoint", 
             diff = paste0("Male ", "\u2212", " Female"))
    
    
    # Cooling setpoint
    
    eff.mod_sex_cooling_direction <- 
      epred_filter_cooling %>%
      ungroup() %>%
      select(-c(c_air.temp, c_rel.hum, .row, .chain, .iteration, count, condition_id)) %>%
      pivot_wider(names_from =  c(air.temp, rel.hum),
                  values_from = "prob",
                  names_glue = "{air.temp}t{rel.hum}rh") %>%
      mutate(!!contrast_name_c := .data[[paste0(temp_values_c[2], "t", rh_values_c[2], "rh")]] -
               .data[[paste0(temp_values_c[1], "t", rh_values_c[1], "rh")]]) %>%
      select(c(sex, time.onset.h, time.day, .draw, vote, all_of(contrast_name_c))) %>%
      pivot_wider(names_from = sex,
                  values_from = all_of(contrast_name_c)) %>%
      mutate(EM.sex_M.F = male - female) %>%
      select(-c(male, female)) %>%
      group_by(time.onset.h, time.day, vote) %>%
      summarise(pr_gt_0 = mean(EM.sex_M.F > 0), 
                pr_lt_0 = mean(EM.sex_M.F < 0), 
                .groups = "drop") %>%
      mutate(setpoint = "Cooling setpoint", 
             diff = paste0("Male ", "\u2212", " Female"))
    

    # 1.4 Save Sex Predictions -------------------------------------------------
    
    saveRDS(eff.mod_sex_heating, output_file_sex_heating)
    saveRDS(eff.mod_sex_cooling, output_file_sex_cooling)
    
    saveRDS(eff.mod_sex_heating_direction, output_file_sex_heating_direction)
    saveRDS(eff.mod_sex_cooling_direction, output_file_sex_cooling_direction)
    

    # 1.5 Save Sex Effect Modification Tables ----------------------------------
    
    eff.mod_sex_heating <-
      eff.mod_sex_heating %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint, diff))
    
    write.csv(eff.mod_sex_heating,
              file.path(output_table_folder,
                        paste0("eff.mod_sex_heating_",
                               contrast_name_h, 
                               ".csv")),
              row.names = FALSE)
    
    eff.mod_sex_cooling <-
      eff.mod_sex_cooling %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint, diff))
    
    write.csv(eff.mod_sex_cooling,
              file.path(output_table_folder,
                        paste0("eff.mod_sex_cooling_", 
                               contrast_name_c, 
                               ".csv")),
              row.names = FALSE)
    
    
    # Directional posterior probabilities
    
    eff.mod_sex_heating_direction <-
      eff.mod_sex_heating_direction %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint, diff))
    
    write.csv(eff.mod_sex_heating_direction,
              file.path(output_table_folder,
                        paste0("eff.mod_sex_heating_direction_", 
                               contrast_name_h, 
                               ".csv")),
              row.names = FALSE)
    
    eff.mod_sex_cooling_direction <-
      eff.mod_sex_cooling_direction %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint, diff))
    
    write.csv(eff.mod_sex_cooling_direction,
              file.path(output_table_folder,
                        paste0("eff.mod_sex_cooling_direction_", 
                               contrast_name_c, 
                               ".csv")),
              row.names = FALSE)
    
    
    # Remove intermediate sex effect modification objects
    
    rm(eff.mod_sex_heating,
       eff.mod_sex_cooling,
       eff.mod_sex_heating_direction,
       eff.mod_sex_cooling_direction)
    
    
    # ==========================================================================
    # 2. Effect Modification by Day
    # ==========================================================================
    
    # 2.1 Calculate Day Effect Modification for Heating ------------------------
    
    eff.mod_day_heating <- 
      epred_filter_heating %>%
      ungroup() %>%
      select(-c(c_air.temp, c_rel.hum, .row, .chain, .iteration, count, condition_id)) %>%
      pivot_wider(names_from =  c(air.temp, rel.hum),
                  values_from = "prob",
                  names_glue = "{air.temp}t{rel.hum}rh") %>%
      mutate(!!contrast_name_h := .data[[paste0(temp_values_h[2], "t", rh_values_h[2], "rh")]] -
               .data[[paste0(temp_values_h[1], "t", rh_values_h[1], "rh")]]) %>%
      select(c(sex, time.onset.h, time.day, .draw, vote, all_of(contrast_name_h))) %>%
      pivot_wider(names_from = time.day,
                  values_from = all_of(contrast_name_h)) %>%
      mutate(EM.day_A.E = afternoon - evening) %>%
      select(-c(afternoon, evening)) %>%
      group_by(time.onset.h, sex, vote) %>%
      mean_qi(.width = 0.95) %>%
      ungroup() %>%
      mutate(setpoint = "Heating setpoint", 
             diff = paste0("Afternoon ", "\u2212", " Evening"))
    
    
    # 2.2 Calculate Day Effect Modification for Cooling ------------------------
    
    eff.mod_day_cooling <- 
      epred_filter_cooling %>%
      ungroup() %>%
      select(-c(c_air.temp, c_rel.hum, .row, .chain, .iteration, count, condition_id)) %>%
      pivot_wider(names_from =  c(air.temp, rel.hum),
                  values_from = "prob",
                  names_glue = "{air.temp}t{rel.hum}rh") %>%
      mutate(!!contrast_name_c := .data[[paste0(temp_values_c[2], "t", rh_values_c[2], "rh")]] -
               .data[[paste0(temp_values_c[1], "t", rh_values_c[1], "rh")]]) %>%
      select(c(sex, time.onset.h, time.day, .draw, vote, all_of(contrast_name_c))) %>%
      pivot_wider(names_from = time.day,
                  values_from = all_of(contrast_name_c)) %>%
      mutate(EM.day_A.E = afternoon - evening) %>%
      select(-c(afternoon, evening)) %>%
      group_by(time.onset.h, sex, vote) %>%
      mean_qi(.width = 0.95) %>%
      ungroup() %>%
      mutate(setpoint = "Cooling setpoint", 
             diff = paste0("Afternoon ", "\u2212", " Evening"))
    
    
    # 2.3 Calculate Directional Posterior Probabilities for Day ----------------
    
    # Heating setpoint
    
    eff.mod_day_heating_direction <- 
      epred_filter_heating %>%
      ungroup() %>%
      select(-c(c_air.temp, c_rel.hum, .row, .chain, .iteration, count, condition_id)) %>%
      pivot_wider(names_from =  c(air.temp, rel.hum),
                  values_from = "prob",
                  names_glue = "{air.temp}t{rel.hum}rh") %>%
      mutate(!!contrast_name_h := .data[[paste0(temp_values_h[2], "t", rh_values_h[2], "rh")]] -
               .data[[paste0(temp_values_h[1], "t", rh_values_h[1], "rh")]]) %>%
      select(c(sex, time.onset.h, time.day, .draw, vote, all_of(contrast_name_h))) %>%
      pivot_wider(names_from = time.day,
                  values_from = all_of(contrast_name_h)) %>%
      mutate(EM.day_A.E = afternoon - evening) %>%
      select(-c(afternoon, evening)) %>%
      group_by(time.onset.h, sex, vote) %>%
      summarise(pr_gt_0 = mean(EM.day_A.E > 0), #probability estimated effect is greater than zero
                pr_lt_0 = mean(EM.day_A.E < 0), #probability estimated effect is lower than zero
                .groups = "drop") %>%
      mutate(setpoint = "Heating setpoint", 
             diff = paste0("Afternoon ", "\u2212", " Evening"))
    
    
    # Cooling setpoint
    
    eff.mod_day_cooling_direction <- 
      epred_filter_cooling %>%
      ungroup() %>%
      select(-c(c_air.temp, c_rel.hum, .row, .chain, .iteration, count, condition_id)) %>%
      pivot_wider(names_from =  c(air.temp, rel.hum),
                  values_from = "prob",
                  names_glue = "{air.temp}t{rel.hum}rh") %>%
      mutate(!!contrast_name_c := .data[[paste0(temp_values_c[2], "t", rh_values_c[2], "rh")]] -
               .data[[paste0(temp_values_c[1], "t", rh_values_c[1], "rh")]]) %>%
      select(c(sex, time.onset.h, time.day, .draw, vote, all_of(contrast_name_c))) %>%
      pivot_wider(names_from = time.day,
                  values_from = all_of(contrast_name_c)) %>%
      mutate(EM.day_A.E = afternoon - evening) %>%
      select(-c(afternoon, evening)) %>%
      group_by(time.onset.h, sex, vote) %>%
      summarise(pr_gt_0 = mean(EM.day_A.E > 0), #probability estimated effect is greater than zero
                pr_lt_0 = mean(EM.day_A.E < 0), #probability estimated effect is lower than zero
                .groups = "drop") %>%
      mutate(setpoint = "Cooling setpoint", 
             diff = paste0("Afternoon ", "\u2212", " Evening"))


    # 2.4 Save Day Predictions -------------------------------------------------
    
    saveRDS(eff.mod_day_heating, output_file_day_heating)
    saveRDS(eff.mod_day_cooling, output_file_day_cooling)
    
    saveRDS(eff.mod_day_heating_direction, output_file_day_heating_direction)
    saveRDS(eff.mod_day_cooling_direction, output_file_day_cooling_direction)
    
    
    # 2.5 Save Day Effect Modification Tables ----------------------------------
    
    eff.mod_day_heating <-
      eff.mod_day_heating %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint, diff)) 
    
    write.csv(eff.mod_day_heating,
              file.path(output_table_folder,
                        paste0("eff.mod_day_heating_", 
                               contrast_name_h, 
                               ".csv")),
              row.names = FALSE)
    
    eff.mod_day_cooling <-
      eff.mod_day_cooling %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint, diff)) 
    
    write.csv(eff.mod_day_cooling,
              file.path(output_table_folder,
                        paste0("eff.mod_day_cooling_", 
                               contrast_name_c, 
                               ".csv")),
              row.names = FALSE)
    
    
    # Directional posterior probabilities
    
    eff.mod_day_heating_direction <-
      eff.mod_day_heating_direction %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint, diff)) 
    
    write.csv(eff.mod_day_heating_direction,
              file.path(output_table_folder,
                        paste0("eff.mod_day_heating_direction_", 
                               contrast_name_h, 
                               ".csv")),
              row.names = FALSE)
    
    eff.mod_day_cooling_direction <-
      eff.mod_day_cooling_direction %>%
      mutate(vote = thermal_vote_scores[vote]) %>%
      select(-c(setpoint, diff)) 
    
    write.csv(eff.mod_day_cooling_direction,
              file.path(output_table_folder,
                        paste0("eff.mod_day_cooling_direction_", 
                               contrast_name_c, 
                               ".csv")),
              row.names = FALSE)
    
    
    # Remove intermediate day effect modification objects
    
    rm(eff.mod_day_heating,
       eff.mod_day_cooling,
       eff.mod_day_heating_direction,
       eff.mod_day_cooling_direction)
    
  }
}
 

# ==============================================================================
# 3. Completion
# ==============================================================================   

message("\nCalculation effect modification (difference in CATE) completed")
message("----------------------------------------\n")