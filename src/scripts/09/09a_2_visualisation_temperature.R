# ============================================================
# Script: 09a_2_visualisation_temperature.R
#
# Author: Matteo Favero
# ==============================================================================

message("----------------------------------------")
message("PMV visualisation in progress\n")


#models_to_process <- models_to_run


# ==============================================================================
# 1. Process Models
# ==============================================================================

# 1.1 Loop over models ---------------------------------------------------------

for (model_name in models_to_process) {
  
  message("Processing: ",
          model_name)

  # 1.2 Define figure output folder --------------------------------------------
  
  output_figure_folder <-
    file.path(figure_folder,
              model_name,
              "pmv_comparison")
  
  if (!dir.exists(output_figure_folder)) {
    
    dir.create(output_figure_folder,
               recursive = TRUE)
    
  }
  

  # 1.3 Define prediction input folder -----------------------------------------
  
  input_prediction_folder <-
    file.path(prediction_folder,
              model_name,
              "pmv_comparison")
  
  # 1.4 Define prediction input file -------------------------------------------
  
  input_file <- 
    file.path(input_prediction_folder,
              "epred_mean_TSV.rds")
  
  
  # 1.5 Load posterior prediction results --------------------------------------
  
  epred_mean.prob <- readRDS(input_file)
  
  
  # 1.6 Filter prediction data -------------------------------------------------
  
  df_ta <- epred_mean.prob %>%
    filter(time.onset.h %in% onset_filter & rel.hum %in% rh_filter)
  
  
  # ============================================================================
  # 2. PMV Reference Layers
  # ============================================================================

  # 2.1 Create PMV layer for 1.0 met --------------------------------------------
  
  pmv_layer_1.0met <- 
    pmv_ta_layer(ta_values = unique(df_ta$air.temp), 
                 rh_values = unique(df_ta$rel.hum),
                 met = 1.0,     
                 linetype = "dotted",
                 colour = "PMV_1.0met", 
                 fill = "PMV_1.0met") 
  
  
  # 2.2 Create PMV layer for 1.1 met --------------------------------------------
  
  pmv_layer_1.1met <- 
    pmv_ta_layer(ta_values = unique(df_ta$air.temp), 
                 rh_values = unique(df_ta$rel.hum),
                 met = 1.1,     
                 linetype = "dotdash",
                 colour = "PMV_1.1met", 
                 fill = "PMV_1.1met")   
  
  # 2.3 Create PMV layer for 1.2 met --------------------------------------------
  
  pmv_layer_1.2met <- 
    pmv_ta_layer(ta_values = unique(df_ta$air.temp), 
                 rh_values = unique(df_ta$rel.hum),
                 met = 1.2,     
                 linetype = "dashed",
                 colour = "PMV_1.2met", 
                 fill = "PMV_1.2met")  
  
  
  # ============================================================================
  # 3. PMV Visualisation by Temperature and Sex
  # ============================================================================
  
  # 3.1 Loop over combinations of plotting conditions --------------------------
  
  for (i in seq_len(nrow(values_df_day))) {
    
    values <- values_df_day[i, , drop = FALSE]
    
    # 3.2 Create plotting condition data frame ---------------------------------
    
    new_df <- values_df_day[i, ]
    names(new_df) <- name_day_map[col_day]
    
    # 3.3 Create dynamic filter expressions ------------------------------------
    
    filter_expr <- lapply(names(values), function(col_name) {
      
      expr(!!sym(col_name) == !!values[[col_name]])
      
    })
    
    
    # 3.4 Combine filter expressions using AND condition -----------------------
    
    combined_filter <- Reduce(function(x) expr(!!x), filter_expr)
    
    
    # 3.5 Create PMV comparison plot -------------------------------------------
    
    plot.ta <-
      df_ta %>%
      filter(!!combined_filter) %>%
      ggplot(aes(x = air.temp, y = mean.vote)) +
  
      geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = sex, group = sex), 
                  alpha = 0.35) +
      
      geom_line(aes(color = sex, group = sex), linewidth = 0.75,
                linetype = "solid") +
      
      pmv_layer_1.0met$layer +
      pmv_layer_1.1met$layer +
      pmv_layer_1.2met$layer +
      
      scale_x_continuous(expression("Air temperature"~("°"*C)),
                         breaks = seq(0, 50, by = 1),
                         expand = c(0, 0)) +
      
      scale_y_continuous(expression("Thermal sensation vote"), #Posterior predictive mean vote
                         breaks = seq(-5, 5, by = 1),
                         limits = c(-3, 3)) +
      
      scale_colour_manual(name = "",
                          values = c(palette_sex, 
                                     "PMV_1.0met" = "black",
                                     "PMV_1.1met" = "black",
                                     "PMV_1.2met" = "black"),
                          labels = c(label_sex,
                                     "PMV_1.0met" = "PMV (1.0 met)",
                                     "PMV_1.1met" = "PMV (1.1 met)",
                                     "PMV_1.2met" = "PMV (1.2 met)")) +
      
      scale_fill_manual(name = "",
                        values = c(palette_sex,
                                   "PMV_1.0met" = NA,
                                   "PMV_1.1met" = NA,
                                   "PMV_1.2met" = NA),
                        labels = c(label_sex,
                                   "PMV_1.0met" = "PMV (1.0 met)",
                                   "PMV_1.1met" = "PMV (1.1 met)",
                                   "PMV_1.2met" = "PMV (1.2 met)")) +
      
      facet_grid(time.onset.h ~ rel.hum,
                 labeller = labeller(time.onset.h = onset_names,
                                     rel.hum = rh_names)) +
      
      theme(strip.background = element_rect(fill = NA, color = NA), 
            strip.text = element_text(size = 11, color = 'black'),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(colour = "black", fill = NA),
            legend.position = 'bottom',      
            legend.direction = 'horizontal', 
            legend.box.spacing = unit(0.2, 'cm'),
            plot.caption = element_text(size = 9, hjust = 1, face = 'italic', color = 'gray30')) 
    
    
    # 4.6 Save PMV comparison plot ---------------------------------------------
    
    ggsave(plot.ta, filename = file.path(output_figure_folder,
                                         paste0("Expected-TSV-by-sex_temp_fix-", values[[1]],  ".png")),
           width = 22,
           height = 15,
           units = "cm",
           dpi = 900)
    
    }
  
  # ============================================================================
  # 4. Clean Model Objects
  # ============================================================================
  
  rm(df_ta,
     plot.ta,
     epred_mean.prob)
  
  gc()
  
}


# ==============================================================================
# 5. Completion
# ==============================================================================

message("\nPMV visualisation completed")
message("----------------------------------------\n")
