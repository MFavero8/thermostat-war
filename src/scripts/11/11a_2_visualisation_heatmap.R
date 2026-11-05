# ==============================================================================
# Script: 11a_2_visualisation_heatmap.R
#
# Description: Heatmap visualisation of posterior UWTDI estimates across air
#              temperature and relative humidity, including heating and cooling
#              intervention points and reference trajectories
#
# Author: Matteo Favero
# ==============================================================================

message("----------------------------------------")
message("Heatmap visualisation in progress\n")


# ==============================================================================
# 0. Setup
# ==============================================================================

# 0.1 Load visualisation settings ----------------------------------------------

source("src/scripts/11/visualisation_setting.R")


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
              "cost_index")

# 1.2 Check input folder -------------------------------------------------------
  
  if (!dir.exists(input_folder)) {
    
    stop("  Prediction folder missing:\n",
         input_folder)
    
  }
  
  
# 1.3 Find UWTDI prediction files ----------------------------------------------
  
  cost_files <- 
    list.files(input_folder,
               pattern = "^cost\\.epred_.*\\.rds$",
               full.names = TRUE)
  
  
# 1.4 Check UWTDI prediction files ---------------------------------------------
  
  if (length(cost_files) == 0) stop("No cost.epred files found.")

  
# 1.5 Define output figure folder ----------------------------------------------
  
  output_figure_folder <-
    file.path(figure_folder,
              model_name,
              "uwtdi")
  
# 1.6 Create output figure folder if necessary ---------------------------------
  
  if (!dir.exists(output_figure_folder)) {
    
    dir.create(output_figure_folder,
               recursive = TRUE)
    
  }
  

# ==============================================================================
# 2. Process Cost Schemes
# ==============================================================================
  
# 2.1 Loop over cost scheme files ----------------------------------------------
  
  for (cost_file in cost_files) {
    
# 2.2 Extract cost scheme name -------------------------------------------------  
    
    scheme_name <- sub("cost\\.epred_(.*)\\.rds",
                       "\\1",
                       basename(cost_file))  
    
    message("  - Processing ", scheme_name, " scheme")
    
    
# 2.3 Load UWTDI estimates -----------------------------------------------------
    
    cost.epred <- readRDS(cost_file) 
  
    
# ==============================================================================
# 3. Define Intervention Points
# ==============================================================================
    
# 3.1 Identify heating intervention points -------------------------------------    
    
    intervention.points_H <- 
      cost.epred %>%
      filter(time.onset.h %in% onset_filter) %>%  
      inner_join(heating_intervention, by = c("air.temp" = "temp",
                                              "rel.hum" = "rh")) %>%
      
      mutate(origin = case_when(air.temp == temp_values_h[1] & rel.hum == rh_values_h[1] ~ "start",
                                air.temp == temp_values_h[2] & rel.hum == rh_values_h[2] ~ "end",
                                TRUE ~ NA_character_))
    
# 3.2 Identify cooling intervention points -------------------------------------    
    
    intervention.points_C <- 
      cost.epred %>%
      filter(time.onset.h %in% onset_filter) %>% 
      inner_join(cooling_intervention, by = c("air.temp" = "temp",
                                              "rel.hum" = "rh")) %>%
      mutate(origin = case_when(air.temp == temp_values_c[1] & rel.hum == rh_values_c[1] ~ "start",
                                air.temp == temp_values_c[2] & rel.hum == rh_values_c[2] ~ "end",
                                TRUE ~ NA_character_))
    
    
# ==============================================================================
# 4. Heatmaps by Sex
# ==============================================================================
    
# 4.1 Loop over daily conditions -----------------------------------------------
    
    for (i in seq_len(nrow(values_df_day))) {
      
      values <- values_df_day[i, , drop = FALSE]
      
      new_df <- values_df_day[i, ]
      names(new_df) <- name_day_map[col_day]
      
      
# 4.2 Create dynamic filtering expressions -------------------------------------
      
      filter_expr <- lapply(names(values), function(col_name) {
        
        expr(!!sym(col_name) == !!values[[col_name]])
        
        })
      
      
# 4.3 Combine filtering conditions ---------------------------------------------
      
      combined_filter <- Reduce(function(x) expr(!!x), filter_expr)

      
# 4.4 Identify UWTDI optima ----------------------------------------------------
      
      opt_points <- 
        cost.epred %>%
        filter(time.onset.h %in% onset_filter) %>% 
        filter(!!combined_filter) %>%
        group_by(sex, time.onset.h) %>% 
        slice_min(total.cost, n = 1) 
      

# 4.5 Generate UWTDI heatmap by sex --------------------------------------------
      
      plot_sex <-
        cost.epred %>%
        filter(time.onset.h %in% onset_filter) %>%
        filter(!!combined_filter) %>%
        ggplot(aes(x = air.temp, y = rel.hum, z = total.cost)) +
        
        geom_raster(interpolate = FALSE, aes(fill = total.cost*100)) +
        
        geom_contour2(aes(label = sprintf("%.0f%%", after_stat(level * 100))),
                      label_size = 2.5,
                      linewidth = 0.27, 
                      colour = "white", 
                      linetype = "dotted",
                      breaks = fixed_levels,
                      skip = 1, 
                      label.placer = label_placer_fraction(frac = 0.40)) +
        
        geom_point(data = opt_points, aes(x = air.temp, y = rel.hum), 
                   color = "white", size = 1, shape = 3) +   
        
        geom_segment(data = intervention.points_H %>%
                       filter(!!combined_filter) %>%
                       select(sex, air.temp, rel.hum, time.onset.h, origin) %>%
                       pivot_wider(names_from = origin, 
                                   values_from = c(air.temp, rel.hum)) %>%
                       # Step 1: Remove points that haven"t moved to avoid division by zero
                       filter(air.temp_start != air.temp_end | rel.hum_start != rel.hum_end) %>%
                       # Step 2: Calculate the exact 0.1 offset
                       mutate(dx = air.temp_end - air.temp_start,
                              dy = rel.hum_end - rel.hum_start,
                              dist = sqrt(dx^2 + dy^2),
                              # Shift start forward by 0.1 and end backward by 0.1
                              x_s = air.temp_start + (0.2 * dx / dist),
                              y_s = rel.hum_start + (0.2 * dy / dist),
                              x_e = air.temp_end - (0.2 * dx / dist),
                              y_e = rel.hum_end - (0.2 * dy / dist)),
                     aes(x = x_s, y = y_s, xend = x_e, yend = y_e),
                     inherit.aes = FALSE,
                     color = palette_heating_setpoint, 
                     linewidth = 0.5,
                     arrow = arrow(length = unit(1.5, "mm"), 
                                   angle = 20,
                                   type = "closed")) +
        
        geom_point(data = intervention.points_H %>%
                     filter(!!combined_filter), 
                   aes(x = air.temp, y = rel.hum), 
                   shape = 21, 
                   color = "black", stroke = 0.4,
                   fill = palette_heating_setpoint, 
                   size = 1.5) +  
        
        geom_text(data = intervention.points_H %>%
                    filter(!!combined_filter), 
                  aes(x = air.temp, y = rel.hum, 
                      label = sprintf("%.1f%%", total.cost*100)), 
                  color = palette_heating_setpoint,  
                  fontface = "bold",
                  vjust = -1,   
                  hjust = 0.5,  
                  size = 2.7)  +
        
        geom_segment(data = intervention.points_C %>%
                       filter(!!combined_filter) %>%
                       select(sex, air.temp, rel.hum, time.onset.h, origin) %>%
                       pivot_wider(names_from = origin, 
                                   values_from = c(air.temp, rel.hum))%>%
                       # Step 1: Remove points that haven"t moved to avoid division by zero
                       filter(air.temp_start != air.temp_end | rel.hum_start != rel.hum_end) %>%
                       # Step 2: Calculate the exact 0.1 offset
                       mutate(dx = air.temp_end - air.temp_start,
                              dy = rel.hum_end - rel.hum_start,
                              dist = sqrt(dx^2 + dy^2),
                              # Shift start forward by 0.1 and end backward by 0.1
                              x_s = air.temp_start + (0.2 * dx / dist),
                              y_s = rel.hum_start + (0.2 * dy / dist),
                              x_e = air.temp_end - (0.2 * dx / dist),
                              y_e = rel.hum_end - (0.2 * dy / dist)),
                     aes(x = x_s, y = y_s, xend = x_e, yend = y_e),
                     inherit.aes = FALSE,
                     color = palette_cooling_setpoint,
                     linewidth = 0.5,
                     arrow = arrow(length = unit(1.5, "mm"), 
                                   angle = 20,
                                   type = "closed")) +
        
        geom_point(data = intervention.points_C %>%
                     filter(!!combined_filter), 
                   aes(x = air.temp, y = rel.hum), 
                   shape = 21, 
                   color = "black", stroke = 0.4,
                   fill = palette_cooling_setpoint,
                   size = 1.5) +  
        
        geom_text(data = intervention.points_C %>%
                    filter(!!combined_filter), 
                  aes(x = air.temp, y = rel.hum, 
                      label = sprintf("%.1f%%", total.cost*100)), 
                  color = palette_cooling_setpoint,
                  fontface = "bold",
                  vjust = -1,   
                  hjust = 0.5,  
                  size = 2.7)  +
        
        scale_x_continuous(expression("Air temperature"~("°"*C)),
                           breaks = seq(0, 50, by=1),
                           expand = c(0, 0)) +
        
        scale_y_continuous(expression("Relative humidity"~("%")),
                           breaks = seq(0, 100, by=10),
                           expand = c(0, 0)) +
        
        scale_fill_gradientn(colours = palette_UWTDI, 
                             limits = c(0, 100),
                             breaks = seq(0, 100, by = 10),
                             guide = guide_colourbar(title = "Posterior expected UWTDI (%)",
                                                     title.position = "top",     
                                                     title.hjust = 0.5,          
                                                     barwidth = 20,   
                                                     barheight = 1)) +
        
        facet_grid(time.onset.h ~ factor(sex, levels = c("male", "female")),
                   labeller = labeller(`factor(sex, levels = c("male", "female"))` = sex_names,
                                       time.onset.h = onset_names)) +
        
        theme(strip.background = element_rect(fill = NA, color = NA), 
              strip.text = element_text(size = 11, color = "black"),
              legend.position = "bottom",   
              legend.direction = "horizontal",  
              legend.box.spacing = unit(0.2, "cm"),
              plot.caption = element_text(size = 9, hjust = 1, face = "italic", color = "gray30"))
      
      
# 4.5 Save heatmap -------------------------------------------------------------      
      
      ggsave(plot_sex, 
             filename = file.path(output_figure_folder,
                                  paste0(scheme_name, "-scheme-by-sex_fix-", values[[1]],  ".png")),
             width = 15,
             height = 16,
             units = "cm",
             dpi = 900)
      }
      
# ==============================================================================
# 5. Generate Heatmap by Day
# ==============================================================================
    
# 5.1 Loop over sex-specific conditions ----------------------------------------
      
    for (i in seq_len(nrow(values_df_sex))) {
      
      values <- values_df_sex[i, , drop = FALSE]
      
      new_df <- values_df_sex[i, ]
      names(new_df) <- name_sex_map[col_sex]

            
# 5.2 Create dynamic filtering expressions -------------------------------------
      
      filter_expr <- lapply(names(values), function(col_name) {
   
        expr(!!sym(col_name) == !!values[[col_name]])
        
      })
      
      
# 5.3 Combine filtering conditions ---------------------------------------------
      
      combined_filter <- Reduce(function(x) expr(!!x), filter_expr)
      
      
# 5.4 Identify UWTDI optima ----------------------------------------------------    
      
      opt_points <- 
        cost.epred %>%
        filter(time.onset.h %in% onset_filter) %>%
        filter(!!combined_filter) %>%
        group_by(sex, time.onset.h) %>% 
        slice_min(total.cost, n = 1) 
     
       
# 5.5 Generate UWTDI heatmap by day --------------------------------------------    
      
      plot_day <-
        cost.epred %>%
        filter(time.onset.h %in% onset_filter) %>%
        filter(!!combined_filter) %>%
        ggplot(aes(x = air.temp, y = rel.hum, z = total.cost)) +
        
        geom_raster(interpolate = FALSE, aes(fill = total.cost*100)) +
        
        geom_contour2(aes(label = sprintf("%.0f%%", after_stat(level * 100))),
                      label_size = 2.5,
                      linewidth = 0.27,
                      colour = "white", 
                      linetype = "dotted",
                      breaks = fixed_levels,
                      skip = 1, 
                      label.placer = label_placer_fraction(frac = 0.40)) +
        
        geom_point(data = opt_points, aes(x = air.temp, y = rel.hum), 
                   color = "white", size = 1, shape = 3) +
        
        geom_segment(data = intervention.points_H %>%
                       filter(!!combined_filter) %>%
                       select(time.day, air.temp, rel.hum, time.onset.h, origin) %>%
                       pivot_wider(names_from = origin, 
                                   values_from = c(air.temp, rel.hum))%>%
                       # Step 1: Remove points that haven"t moved to avoid division by zero
                       filter(air.temp_start != air.temp_end | rel.hum_start != rel.hum_end) %>%
                       # Step 2: Calculate the exact 0.1 offset
                       mutate(dx = air.temp_end - air.temp_start,
                              dy = rel.hum_end - rel.hum_start,
                              dist = sqrt(dx^2 + dy^2),
                              # Shift start forward by 0.1 and end backward by 0.1
                              x_s = air.temp_start + (0.2 * dx / dist),
                              y_s = rel.hum_start + (0.2 * dy / dist),
                              x_e = air.temp_end - (0.2 * dx / dist),
                              y_e = rel.hum_end - (0.2 * dy / dist)),
                     aes(x = x_s, y = y_s, xend = x_e, yend = y_e),
                     inherit.aes = FALSE,
                     color = palette_heating_setpoint,
                     linewidth = 0.5,
                     arrow = arrow(length = unit(1.5, "mm"), 
                                   angle = 20,
                                   type = "closed")) +
        
        geom_point(data = intervention.points_H %>%
                     filter(!!combined_filter), 
                   aes(x = air.temp, y = rel.hum), 
                   shape = 21, 
                   color = "black", stroke = 0.4,
                   fill = palette_heating_setpoint, 
                   size = 1.5) +
        
        geom_text(data = intervention.points_H %>%
                    filter(!!combined_filter), 
                  aes(x = air.temp, y = rel.hum, 
                      label = sprintf("%.1f%%", total.cost*100)), 
                  color = palette_heating_setpoint, 
                  fontface = "bold",
                  vjust = -1,   
                  hjust = 0.5,  
                  size = 2.7)  +
        
        geom_segment(data = intervention.points_C %>%
                       filter(!!combined_filter) %>%
                       select(time.day, air.temp, rel.hum, time.onset.h, origin) %>%
                       pivot_wider(names_from = origin, 
                                   values_from = c(air.temp, rel.hum))%>%
                       # Step 1: Remove points that haven"t moved to avoid division by zero
                       filter(air.temp_start != air.temp_end | rel.hum_start != rel.hum_end) %>%
                       # Step 2: Calculate the exact 0.1 offset
                       mutate(dx = air.temp_end - air.temp_start,
                              dy = rel.hum_end - rel.hum_start,
                              dist = sqrt(dx^2 + dy^2),
                              # Shift start forward by 0.1 and end backward by 0.1
                              x_s = air.temp_start + (0.2 * dx / dist),
                              y_s = rel.hum_start + (0.2 * dy / dist),
                              x_e = air.temp_end - (0.2 * dx / dist),
                              y_e = rel.hum_end - (0.2 * dy / dist)),
                     aes(x = x_s, y = y_s, xend = x_e, yend = y_e),
                     inherit.aes = FALSE,
                     color = palette_cooling_setpoint,
                     linewidth = 0.5,
                     arrow = arrow(length = unit(1.5, "mm"), 
                                   angle = 20,
                                   type = "closed")) +
        
        geom_point(data = intervention.points_C %>%
                     filter(!!combined_filter), 
                   aes(x = air.temp, y = rel.hum), 
                   shape = 21, 
                   color = "black", stroke = 0.4,
                   fill = palette_cooling_setpoint,
                   size = 1.5) +  
        
        geom_text(data = intervention.points_C %>%
                    filter(!!combined_filter), 
                  aes(x = air.temp, y = rel.hum, 
                      label = sprintf("%.1f%%", total.cost*100)), 
                  color = palette_cooling_setpoint,
                  fontface = "bold",
                  vjust = -1,   
                  hjust = 0.5,  
                  size = 2.7)  +
        
        scale_x_continuous(expression("Air temperature"~("°"*C)),
                           breaks = seq(0, 50, by=1),
                           expand = c(0, 0)) +
        
        scale_y_continuous(expression("Relative humidity"~("%")),
                           breaks = seq(0, 100, by=10),
                           expand = c(0, 0)) +

        scale_fill_gradientn(colours = palette_UWTDI, 
                             limits = c(0, 100),
                             breaks = seq(0, 100, by = 10),
                             guide = guide_colourbar(title = "Posterior expected UWTDI (%)",
                                                     title.position = "top",     
                                                     title.hjust = 0.5,          
                                                     barwidth = 20,   
                                                     barheight = 1)) +
        
        facet_grid(time.onset.h ~ factor(time.day, levels = c("afternoon", "evening")),
                   labeller = labeller(`factor(time.day, levels = c("afternoon", "evening"))` = day_names,
                                       time.onset.h = onset_names)) +
        
        theme(strip.background = element_rect(fill = NA, color = NA), 
              strip.text = element_text(size = 11, color = "black"),
              legend.position = "bottom",   
              legend.direction = "horizontal",  
              legend.box.spacing = unit(0.2, "cm"),
              plot.caption = element_text(size = 9, hjust = 1, face = "italic", color = "gray30"))
      
   
# 5.5 Save heatmap -------------------------------------------------------------
      
      ggsave(plot_day, 
             filename = file.path(output_figure_folder,
                                  paste0(scheme_name, "-scheme-by-day_fix-", values[[1]],  ".png")),
             width = 15,
             height = 16,
             units = "cm",
             dpi = 900)
      }
    
  }
  
  rm(cost.epred,
     plot_sex,
     plot_day)
  
  gc()
  
}
    

# ==============================================================================
# 10. Completion
# ==============================================================================

message("\nHeatmap visualisation completed")
message("----------------------------------------\n")
