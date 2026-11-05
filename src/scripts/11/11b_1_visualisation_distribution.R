# ==============================================================================
# Script: 11b_1_visualisation_distribution.R
#
# Description: Visualisation of posterior UWTDI distributions and intervention
#              differences across heating and cooling conditions, sex and
#              thermal onset time
#
# Author: Matteo Favero
# ==============================================================================

message("----------------------------------------")
message("Distribution visualisation in progress\n")


# ==============================================================================
# 0. Setup
# ==============================================================================

# 0.1 Load packages ------------------------------------------------------------

suppressPackageStartupMessages({
  library(patchwork)
})


# 0.2 Load visualisation settings ----------------------------------------------

source("src/scripts/11/visualisation_setting.R")


# 0.3 Define models to process -------------------------------------------------

models_to_process <- models_to_run


# ==============================================================================
# 1. Process Models
# ==============================================================================

for (model_name in models_to_process) {
  
  message("Processing ",
          model_name)
  
    
# 1.1 Define output figure folder ----------------------------------------------
  
  output_figure_folder <-
    file.path(figure_folder,
              model_name,
              "uwtdi_distribution")
  
  
# 1.2 Create output figure folder if necessary ---------------------------------  
  
  if (!dir.exists(output_figure_folder)) {
    
    dir.create(output_figure_folder,
               recursive = TRUE)
    
  }
  

# 1.3 Define output table folder -----------------------------------------------
  
  output_table_folder <-
    file.path(table_folder,
              model_name,
              "uwtdi_distribution")

    
# 1.4 Create output table folder if necessary ----------------------------------  
  
  if (!dir.exists(output_table_folder)) {
    
    dir.create(output_table_folder,
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

    
# 2.2 Find cost prediction files ----------------------------------------------- 
  
  cost_files <- 
    list.files(input_cost_folder,
               pattern = "^cost\\.epred_.*\\.rds$",
               full.names = TRUE)
  

# 2.3 Loop over cost files ----------------------------------------------------- 
  
  for (cost_file in cost_files) {
    
    scheme_name <- sub("cost\\.epred_(.*)\\.rds",
                       "\\1",
                       basename(cost_file))  

        
# 2.4 Define intervention distribution input folder ----------------------------
    
    intput_prediction_folder <-
      file.path(prediction_folder,
                model_name,
                "cost_index_intervention_distribution")
    
    
# 2.5 Define heating intervention distribution files ---------------------------    
    
    intput_file_heating_cost_distr <- 
      file.path(output_prediction_folder,
                paste0("cost.heating.epred_", scheme_name, ".rds"))
    
    intput_file_heating_cost_distr.diff <- 
      file.path(output_prediction_folder,
                paste0("cost.heating.diff.epred_", scheme_name, ".rds"))
    
    
# 2.6 Define cooling intervention distribution files ---------------------------    
    
    intput_file_cooling_cost_distr <- 
      file.path(output_prediction_folder,
                paste0("cost.cooling.epred_", scheme_name, ".rds"))
    
    intput_file_cooling_cost_distr.diff <- 
      file.path(output_prediction_folder,
                paste0("cost.cooling.diff.epred_", scheme_name, ".rds"))
    
    
# ==============================================================================
# 3. Load and Prepare Intervention Distributions
# ==============================================================================
    
# 3.1 Check intervention distribution files ------------------------------------
    
    if (file.exists(intput_file_heating_cost_distr) && 
        file.exists(intput_file_heating_cost_distr.diff) && 
        file.exists(intput_file_cooling_cost_distr) && 
        file.exists(intput_file_cooling_cost_distr.diff))  {

      heating_cost_distr <- readRDS(intput_file_heating_cost_distr)
      heating_cost_distr.diff <- readRDS(intput_file_heating_cost_distr.diff)
      
      cooling_cost_distr <- readRDS(intput_file_cooling_cost_distr)
      cooling_cost_distr.diff <- readRDS(intput_file_cooling_cost_distr.diff)
              
    } else {
      
      message("  Skipping. Files for ", scheme_name,
              " scheme not found.\n")
      next
      
    }
    
    message("  - Processing ", scheme_name, " scheme")
    
# ==============================================================================
# 4. Calculate Posterior Probabilities
# ==============================================================================
    
# 4.1 Calculate probability of UWTDI exceeding 20%: heating --------------------
  
    heating_greater20 <- heating_cost_distr %>%
      mutate(greater20 = case_when(total.cost > 0.2 ~ 1,
                                   total.cost <= 0.2 ~ 0,
                                   .default = NA)) %>%
      group_by(air.temp, rel.hum, sex, time.day, time.onset.h) %>%
      summarise(pr_greater20 = mean(greater20), .groups = "drop")
    

# 4.2 Calculate probability of UWTDI exceeding 20%: cooling --------------------    
    
    cooling_greater20 <- cooling_cost_distr %>%
      mutate(greater20 = case_when(total.cost > 0.2 ~ 1,
                                   total.cost <= 0.2 ~ 0,
                                   .default = NA)) %>%
      group_by(air.temp, rel.hum, sex, time.day, time.onset.h) %>%
      summarise(pr_greater20 = mean(greater20), .groups = "drop")
    
    
# 4.3 Calculate probability of positive intervention difference: heating -------
    
    heating.diff_greater0 <- heating_cost_distr.diff %>%
      mutate(greater0 = case_when(.data[[contrast_name_h]] > 0 ~ 1,
                                  .data[[contrast_name_h]] <= 0 ~ 0,
                                  .default = NA)) %>%
      group_by(sex, time.day, time.onset.h) %>%
      summarise(pr_greater0 = mean(greater0), .groups = "drop")

       
# 4.4 Calculate probability of positive intervention difference: cooling -------     
    
    cooling.diff_greater0 <- cooling_cost_distr.diff %>%
      mutate(greater0 = case_when(.data[[contrast_name_c]] > 0 ~ 1,
                                  .data[[contrast_name_c]] <= 0 ~ 0,
                                  .default = NA)) %>%
      group_by(sex, time.day, time.onset.h) %>%
      summarise(pr_greater0 = mean(greater0), .groups = "drop")
    
    
# ==============================================================================
# 5. Calculate Posterior Summaries
# ==============================================================================
    
# 5.1 Summarise heating UWTDI distributions ------------------------------------    

    heating_cost_distr_summary <- 
      heating_cost_distr %>%
      group_by(air.temp, rel.hum, sex, time.day, time.onset.h) %>%
      mean_qi()

        
# 5.2 Summarise heating intervention differences -------------------------------    
    
    heating_cost_distr.diff_summary <- 
      heating_cost_distr.diff %>%
      group_by(sex, time.day, time.onset.h, setpoint) %>%
      mean_qi()

    
# 5.3 Summarise cooling UWTDI distributions ------------------------------------
    
    cooling_cost_distr_summary <- 
      cooling_cost_distr %>%
      group_by(air.temp, rel.hum, sex, time.day, time.onset.h) %>%
      mean_qi()
    
    
# 5.4 Summarise cooling intervention differences ------------------------------- 
    
    cooling_cost_distr.diff_summary <- 
      cooling_cost_distr.diff %>%
      group_by(sex, time.day, time.onset.h, setpoint) %>%
      mean_qi()
    
    
# ==============================================================================
# 6. Combine Posterior Summaries and Probabilities
# ==============================================================================
    
# 6.1 Add probability of UWTDI exceeding 20% to heating summary ----------------  

      heating_cost_distr_summary <-
        left_join(heating_cost_distr_summary, heating_greater20, 
                  by = c("air.temp", "rel.hum", "sex", "time.day", "time.onset.h"))

          
# 6.2 Add probability of positive difference to heating summary ----------------
    
      heating_cost_distr.diff_summary <-
        left_join(heating_cost_distr.diff_summary, heating.diff_greater0, 
                  by = c("sex", "time.day", "time.onset.h"))

          
# 6.3 Add probability of UWTDI exceeding 20% to cooling summary ----------------
    
      cooling_cost_distr_summary <-
        left_join(cooling_cost_distr_summary, cooling_greater20, 
                  by = c("air.temp", "rel.hum", "sex", "time.day", "time.onset.h"))

    
# 6.4 Add probability of positive difference to cooling summary ----------------
          
      cooling_cost_distr.diff_summary <-
        left_join(cooling_cost_distr.diff_summary, cooling.diff_greater0, 
                  by = c("sex", "time.day", "time.onset.h"))
      
    
# ==============================================================================
# 7. Save Posterior Summary Tables
# ==============================================================================
    
# 7.1 Save heating UWTDI summary -----------------------------------------------      

      write.csv(heating_cost_distr_summary, 
                file.path(output_table_folder, 
                          paste0(scheme_name, "_", "heating.intervention_summary.csv")),
                row.names = FALSE)
    
      
# 7.2 Save heating intervention difference summary -----------------------------
    
      write.csv(heating_cost_distr.diff_summary, 
                file.path(output_table_folder, 
                          paste0(scheme_name, "_", "heating.diff.intervention_summary.csv")),
                row.names = FALSE)
    
      
# 7.3 Save cooling UWTDI summary -----------------------------------------------
    
      write.csv(cooling_cost_distr_summary, 
                file.path(output_table_folder, 
                          paste0(scheme_name, "_", "cooling.intervention_summary.csv")),
                row.names = FALSE)
      
    
# 7.4 Save cooling intervention difference summary -----------------------------
    
      write.csv(cooling_cost_distr.diff_summary, 
                file.path(output_table_folder, 
                          paste0(scheme_name, "_", "cooling.diff.intervention_summary.csv")),
                row.names = FALSE)
      
    
# ==============================================================================
# 8. Generate Distribution Visualisations
# ==============================================================================
    
# 8.1 Loop over temperature conditions -----------------------------------------
      
      for (i in seq_len(nrow(values_df_day))) {
        
        values <- values_df_day[i, , drop = FALSE]
        
        new_df <- values_df_day[i, ]
        names(new_df) <- name_day_map[col_day]
        
# 8.2 Create dynamic filtering expressions -------------------------------------
        
        filter_expr <- lapply(names(values), function(col_name) {

          expr(!!sym(col_name) == !!values[[col_name]])
          
        })
        
        
# 8.3 Combine filtering conditions ---------------------------------------------
        
        combined_filter <- Reduce(function(x) expr(!!x), filter_expr)
        
        
# ==============================================================================
# 9. Define Distribution Reference Data
# ==============================================================================
        
# 9.1 Calculate mean heating distribution --------------------------------------
        
        rug_dist_heating <- heating_cost_distr %>%
          mutate(facet_setpoint = case_when(air.temp == temp_values_h[1] & rel.hum == rh_values_h[1] ~ paste0("a' = (", sprintf("%0.1f", air.temp), "°C", ", ", rel.hum, "%rh)"),
                                            air.temp == temp_values_h[2] & rel.hum == rh_values_h[2] ~ paste0("a = (", sprintf("%0.1f", air.temp), "°C", ", ", rel.hum, "%rh)"),
                                            TRUE ~ NA_character_)) %>%
          filter(!!combined_filter) %>%
          filter(!is.na(facet_setpoint)) %>%
          group_by(time.onset.h, facet_setpoint, sex) %>%
          summarise(mean_cost = mean(total.cost, na.rm = TRUE),
                    .groups = "drop")
        
# 9.2 Calculate mean cooling distribution --------------------------------------
        
        rug_dist_cooling <- cooling_cost_distr %>%
          mutate(facet_setpoint = case_when(air.temp == temp_values_c[1] & rel.hum == rh_values_c[1] ~ paste0("a' = (", sprintf("%0.1f", air.temp), "°C", ", ", rel.hum, "%rh)"),
                                            air.temp == temp_values_c[2] & rel.hum == rh_values_c[2] ~ paste0("a = (", sprintf("%0.1f", air.temp), "°C", ", ", rel.hum, "%rh)"),
                                            TRUE ~ NA_character_)) %>%
          filter(!!combined_filter) %>%
          filter(!is.na(facet_setpoint)) %>%
          group_by(time.onset.h, facet_setpoint, sex) %>%
          summarise(mean_cost = mean(total.cost, na.rm = TRUE),
                    .groups = "drop")
        
        
# 9.3 Calculate mean heating distribution difference --------------------------- 
        
        rug_dist_heating.diff <- heating_cost_distr.diff %>%
          filter(!!combined_filter) %>%
          group_by(time.onset.h, sex) %>%
          summarise(mean_cost.diff = mean(.data[[contrast_name_h]], na.rm = TRUE),
                    .groups = "drop")
        
        
# 9.4 Calculate mean cooling distribution difference --------------------------- 
        
        rug_dist_cooling.diff <- cooling_cost_distr.diff %>%
          filter(!!combined_filter) %>%
          group_by(time.onset.h, sex) %>%
          summarise(mean_cost.diff = mean(.data[[contrast_name_c]], na.rm = TRUE),
                    .groups = "drop")
   
        
# ==============================================================================
# 10. Generate Heating Distribution Plot
# ==============================================================================
        
# 10.1 Generate heating UWTDI distribution plot --------------------------------     
      
        plot_dist_heating <- 
          heating_cost_distr %>%
          mutate(facet_setpoint = case_when(air.temp == temp_values_h[1] & rel.hum == rh_values_h[1] ~ paste0("a' = (", sprintf("%0.1f", air.temp), "°C", ", ", rel.hum, "%rh)"),
                                            air.temp == temp_values_h[2] & rel.hum == rh_values_h[2] ~ paste0("a = (", sprintf("%0.1f", air.temp), "°C", ", ", rel.hum, "%rh)"),
                                            TRUE ~ NA_character_)) %>%
          filter(!!combined_filter) %>%
          ggplot(aes(x = total.cost, fill = sex)) + 
          
          stat_histinterval(density = "histogram",     
                            breaks = ggdist::breaks_fixed(width = 0.02), 
                            align = ggdist::align_boundary(at = 0), # forces the 0.01 bins to align perfectly starting from 0
                            outline_bars = TRUE,        
                            point_interval = mean_qi, 
                            .width = .95,
                            shape = 16, 
                            stroke = 1/4, 
                            point_size = 2.3, 
                            point_color = NA, 
                            point_fill = NA,
                            color = NA, 
                            alpha = 0.50)  +  
          
          geom_segment(data = rug_dist_heating, 
                       aes(x = mean_cost, xend = mean_cost, 
                           y = 0.08, yend = -0.08, 
                           color = sex), 
                       inherit.aes = FALSE, 
                       linewidth = 0.75,
                       show.legend = FALSE) +
          
          geom_vline(aes(xintercept = 0.2), linewidth = 0.30, linetype = "dotted", color = "grey30") +
 
          scale_x_continuous(expression("Posterior UWTDI"~("%")),
                             breaks = seq(0, 1, by = 0.20),
                             labels = function(x) x * 100) +
          
          coord_cartesian(xlim = c(0, 1), 
                          ylim = c(-0.025, NA)) +
          
          scale_color_manual(name = "",
                             values = palette_sex,
                             labels = label_sex) +
          
          scale_fill_manual(name = "",
                            values = palette_sex,
                            labels = label_sex) +
          
          facet_grid(time.onset.h ~ facet_setpoint, 
                     labeller = labeller(time.onset.h = onset_names)) +
          
          ggtitle("Heating") +
          
          theme(strip.background = element_rect(fill = NA, color = NA), 
                strip.text = element_text(size = 11, color = 'black'),
                plot.title = element_text(size = 12, face = "bold"),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.title.y = element_blank(), 
                axis.text.y = element_blank(), 
                axis.ticks.y = element_blank(),
                panel.border = element_rect(colour = "black", fill = NA),
                legend.position = 'bottom',     
                legend.direction = 'horizontal',  
                legend.box.spacing = unit(0.2, 'cm'),
                plot.caption = element_text(size = 9, hjust = 1, face = 'italic', color = 'gray30')) 
        
        
# ==============================================================================
# 11. Generate Heating Difference Distribution Plot
# ==============================================================================
      
# 11.1 Generate heating intervention difference distribution plot -------------- 
        
        plot_dist.diff_heating <-
          heating_cost_distr.diff %>%
          filter(!!combined_filter) %>%
          ggplot(aes(x = .data[[contrast_name_h]], fill = sex)) +  
          
          stat_histinterval(density = "histogram",     
                            breaks = ggdist::breaks_fixed(width = 0.02), 
                            align = ggdist::align_boundary(at = -1), # forces the 0.01 bins to align perfectly starting from 0
                            outline_bars = TRUE,        
                            point_interval = mean_qi, 
                            .width = .95,
                            shape = 16, 
                            stroke = 1/4, 
                            point_size = 2.3, 
                            point_color = NA, 
                            point_fill = NA,
                            color = NA, 
                            alpha = 0.50)  + 
          
          geom_segment(data = rug_dist_heating.diff, 
                       aes(x = mean_cost.diff, xend = mean_cost.diff, 
                           y = 0.08, yend = -0.08, 
                           color = sex), 
                       inherit.aes = FALSE, 
                       linewidth = 0.75,
                       show.legend = FALSE) +
          
          geom_vline(aes(xintercept = 0), linewidth = 0.30, linetype = "dotted", color = "grey30") +

          scale_x_continuous(expression("Difference in UWTDI"~("%")),
                             breaks = seq(-1, 1, by = 0.20),
                             labels = function(x) x * 100) +
          
          coord_cartesian(xlim = c(-0.45, 0.45), 
                          ylim = c(-0.025, NA)) +
          
          scale_color_manual(name = "",
                             values = palette_sex,
                             labels = label_sex) +
          
          scale_fill_manual(name = "",
                            values = palette_sex,
                            labels = label_sex) +
          
          facet_grid(time.onset.h ~ setpoint, 
                     labeller = labeller(time.onset.h = onset_names)) +
          
          theme(strip.background = element_rect(fill = NA, color = NA), 
                strip.text = element_text(size = 11, color = 'black'),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_rect(fill = "white"),
                axis.title.y = element_blank(), 
                axis.text.y = element_blank(), 
                axis.ticks.y = element_blank(),
                panel.border = element_rect(colour = "black", fill = NA),
                legend.position = 'bottom',      
                legend.direction = 'horizontal',  
                legend.box.spacing = unit(0.2, 'cm'),
                plot.caption = element_text(size = 9, hjust = 1, face = 'italic', color = 'gray30')) 
        
        
# ==============================================================================
# 12. Generate Cooling Distribution Plot
# ==============================================================================
        
# 12.1 Generate cooling UWTDI distribution plot --------------------------------
        
        plot_dist_cooling <-
          cooling_cost_distr %>%
          mutate(facet_setpoint = case_when(air.temp == temp_values_c[1] & rel.hum == rh_values_c[1] ~ paste0("a' = (", sprintf("%0.1f", air.temp), "°C", ", ", rel.hum, "%rh)"),
                                            air.temp == temp_values_c[2] & rel.hum == rh_values_c[2] ~ paste0("a = (", sprintf("%0.1f", air.temp), "°C", ", ", rel.hum, "%rh)"),
                                            TRUE ~ NA_character_)) %>%
          filter(!!combined_filter) %>%
          ggplot(aes(x = total.cost, fill = sex)) + 
          
          stat_histinterval(density = "histogram",     
                            breaks = ggdist::breaks_fixed(width = 0.02), 
                            align = ggdist::align_boundary(at = 0), # forces the 0.01 bins to align perfectly starting from 0
                            outline_bars = TRUE,        
                            point_interval = mean_qi, 
                            .width = .95,
                            shape = 16, 
                            stroke = 1/4, 
                            point_size = 2.3, 
                            point_color = NA, 
                            point_fill = NA,
                            color = NA, 
                            alpha = 0.50)  +  
          
          geom_segment(data = rug_dist_cooling, 
                       aes(x = mean_cost, xend = mean_cost, 
                           y = 0.08, yend = -0.08, 
                           color = sex), 
                       inherit.aes = FALSE, 
                       linewidth = 0.75,
                       show.legend = FALSE) +
          
          geom_vline(aes(xintercept = 0.2), linewidth = 0.30, linetype = "dotted", color = "grey30") +
          
          scale_x_continuous(expression("Posterior UWTDI"~("%")),
                             breaks = seq(0, 1, by = 0.20),
                             labels = function(x) x * 100) +
          
          coord_cartesian(xlim = c(0, 1), 
                          ylim = c(-0.025, NA)) +
          
          scale_color_manual(name = "",
                             values = palette_sex,
                             labels = label_sex) +
          
          scale_fill_manual(name = "",
                            values = palette_sex,
                            labels = label_sex) +
          
          facet_grid(time.onset.h ~ facet_setpoint, 
                     labeller = labeller(time.onset.h = onset_names)) +
          
          ggtitle("Cooling") +
          
          theme(strip.background = element_rect(fill = NA, color = NA), 
                strip.text = element_text(size = 11, color = 'black'),
                plot.title = element_text(size = 12, face = "bold"),
                panel.grid.major = element_blank(), ,
                panel.grid.minor = element_blank(), 
                panel.background = element_rect(fill = "white"),
                axis.title.y = element_blank(), 
                axis.text.y = element_blank(), 
                axis.ticks.y = element_blank(),
                panel.border = element_rect(colour = "black", fill = NA),
                legend.position = 'bottom',      
                legend.direction = 'horizontal', 
                legend.box.spacing = unit(0.2, 'cm'),
                plot.caption = element_text(size = 9, hjust = 1, face = 'italic', color = 'gray30')) 
        
        
# ==============================================================================
# 13. Generate Cooling Difference Distribution Plot
# ==============================================================================
        
# 13.1 Generate cooling intervention difference distribution plot --------------
        
        plot_dist.diff_cooling <-
          cooling_cost_distr.diff %>%
          filter(!!combined_filter) %>%
          ggplot(aes(x = .data[[contrast_name_c]], fill = sex)) + 
          
          stat_histinterval(density = "histogram",     
                            breaks = ggdist::breaks_fixed(width = 0.02), 
                            align = ggdist::align_boundary(at = -1), # forces the 0.01 bins to align perfectly starting from 0
                            outline_bars = TRUE,        
                            point_interval = mean_qi, 
                            .width = .95,
                            shape = 16, 
                            stroke = 1/4, 
                            point_size = 2.3, 
                            point_color = NA, 
                            point_fill = NA,
                            color = NA, 
                            alpha = 0.50)  + 
          
          geom_segment(data = rug_dist_cooling.diff, 
                       aes(x = mean_cost.diff, xend = mean_cost.diff, 
                           y = 0.08, yend = -0.08, 
                           color = sex), 
                       inherit.aes = FALSE, 
                       linewidth = 0.75,
                       show.legend = FALSE) +
          
          geom_vline(aes(xintercept = 0), linewidth = 0.30, linetype = "dotted", color = "grey30") +
          
          scale_x_continuous(expression("Difference in UWTDI"~("%")),
                             breaks = seq(-1, 1, by = 0.20),
                             labels = function(x) x * 100) +
          
          coord_cartesian(xlim = c(-0.45, 0.45), 
                          ylim = c(-0.025, NA)) +
          
          scale_color_manual(name = "",
                             values = palette_sex,
                             labels = label_sex) +
          
          scale_fill_manual(name = "",
                            values = palette_sex,
                            labels = label_sex) +
          
          facet_grid(time.onset.h ~ setpoint, 
                     labeller = labeller(time.onset.h = onset_names)) +
          
          theme(strip.background = element_rect(fill = NA, color = NA), 
                strip.text = element_text(size = 11, color = 'black'),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_rect(fill = "white"),
                axis.title.y = element_blank(), 
                axis.text.y = element_blank(), 
                axis.ticks.y = element_blank(),
                panel.border = element_rect(colour = "black", fill = NA),
                legend.position = 'bottom', 
                legend.direction = 'horizontal',  
                legend.box.spacing = unit(0.2, 'cm'),
                plot.caption = element_text(size = 9, hjust = 1, face = 'italic', color = 'gray30')) 
        
        
# ==============================================================================
# 14. Combine Distribution Plots
# ==============================================================================
        
# 14.1 Remove individual y-axis titles -----------------------------------------
        
        plot_dist_heating <- plot_dist_heating +
          theme(axis.title.y = element_blank()) 
        
        plot_dist_cooling <- plot_dist_cooling +
          theme(axis.title.y = element_blank()) 
        
        plot_dist.diff_heating <- plot_dist.diff_heating +
          theme(axis.title.y = element_blank()) 
        
        plot_dist.diff_cooling <- plot_dist.diff_cooling +
          theme(axis.title.y = element_blank()) 
        
        
# 14.2 Combine heating plots ---------------------------------------------------
        
        top_panel <-
          (plot_dist_heating |
             plot_dist.diff_heating) +
          plot_layout(heights = c(1, 1),
                      widths = c(1, 0.6))
        
        
# 14.3 Combine cooling plots ---------------------------------------------------
        
        bottom_panel <-
          (plot_dist_cooling | 
             plot_dist.diff_cooling) +
          plot_layout(heights = c(1, 1),
                      widths = c(1, 0.6))

        
# 14.4 Combine heating and cooling panels --------------------------------------
        
        combined_plot <-
          (top_panel / bottom_panel) +
          plot_layout(guides = "collect") &
          theme(legend.position = "bottom",
                plot.tag.position = c(0.03, 1),
                legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
                legend.box.margin = margin(t = -5, r = 0, b = 0, l = 0, unit = "pt"),
                plot.tag = element_text(size = 12,
                                        hjust = 0,
                                        face = "bold"))
        
        
# ==============================================================================
# 15. Save Distribution Visualisations
# ==============================================================================
        
# 15.1 Save combined distribution plot -----------------------------------------
        
        ggsave(combined_plot, 
               filename = file.path(output_figure_folder,
                                    paste0("Distribution_", scheme_name, "-scheme-by-sex_fix-", values[[1]],  ".png")),
               width = 18, #20,
               height = 18, #26,
               units = "cm",
               dpi = 900)
      }
    
  }
  
  
# ==============================================================================
# 16. Clean Model-Specific Objects
# ==============================================================================
  
  rm(heating_cost_distr,
     heating_cost_distr.diff, 
     cooling_cost_distr,
     cooling_cost_distr.diff,
     
     heating_greater20,
     heating.diff_greater0,
     heating_cost_distr_summary,
     heating_cost_distr.diff_summary,
     
     cooling_greater20,
     cooling.diff_greater0,
     cooling_cost_distr_summary,
     cooling_cost_distr.diff_summary)
  
}

gc()


# ==============================================================================
# 17. Completion
# ==============================================================================

message("\nDistribution visualisation completed")
message("----------------------------------------\n")


# ==============================================================================
# 18. Unload Packages
# ==============================================================================

packages_to_unload <- 
  c("patchwork")

for (package in packages_to_unload) {
  
  if (paste0("package:", package) %in% search()) {
    
    detach(
      paste0("package:", package),
      unload = FALSE,
      character.only = TRUE
    )
    
  }
  
}
