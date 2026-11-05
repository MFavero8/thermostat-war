# ==============================================================================
# Script: 11a_1_visualisation_temperature.R
#
# Description: Visualisation of posterior UWTDI estimates across air temperature,
#              relative humidity, thermal onset time, sex and activity level
#
# Author: Matteo Favero
# ==============================================================================

message("----------------------------------------")
message("PPD visualisation in progress\n")


# ==============================================================================
# 0. Setup
# ==============================================================================

# 0.1 Load packages ------------------------------------------------------------

suppressPackageStartupMessages({
  library(patchwork)
})


# 0.3 Load visualisation settings ----------------------------------------------

source("src/scripts/11/visualisation_setting.R")


# 0.4 Define models to process -------------------------------------------------

models_to_process <- models_to_run


# ==============================================================================
# 1. Process Models
# ==============================================================================

for (model_name in models_to_process) {
  
  message("Processing: ",
          model_name)

  
# 1.1 Define input folder ------------------------------------------------------
  
  input_folder <- 
    file.path(prediction_folder,
              model_name,
              "cost_index")

  
# 1.2 Check input folder -------------------------------------------------------
  
  if (!dir.exists(input_folder)) {
    
    stop("\tPrediction folder missing: \n",
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
# 2. Load and Prepare UWTDI Estimates
# ==============================================================================
  
# 2.1 Select files corresponding to available cost schemes ---------------------
  
  file_names <- 
    cost_files[basename(cost_files) %in% paste0("cost.epred_", names(cost_schemes), ".rds")]
  
# 2.2 Load UWTDI estimates -----------------------------------------------------  
  
  df_list <- lapply(file_names, function(file) {
    
    data <- readRDS(file)
    
    # Add cost scheme identifier
    data$cost.type <- sub("cost\\.epred_(.*)\\.rds",
                          "\\1", 
                          basename(file))
    
    return(data)
  })

    
# 2.3 Combine cost schemes -----------------------------------------------------
  
  df_ta <- do.call(rbind, df_list) %>%
    filter(time.onset.h %in% onset_filter & rel.hum %in% rh_filter)
  
  
# ==============================================================================
# 3. Generate Temperature Visualisations
# ==============================================================================
  
# 3.1 Loop over temperature conditions -----------------------------------------  
  
  for (i in seq_len(nrow(values_df_day))) {
    
    values <- values_df_day[i, , drop = FALSE]
    
    new_df <- values_df_day[i, ]
    names(new_df) <- name_day_map[col_day]
    
    
# 3.2 Create dynamic filtering expressions -------------------------------------
    
    filter_expr <- lapply(names(values), function(col_name) {
    
      expr(!!sym(col_name) == !!values[[col_name]])
      
    })
    
# 3.3 Combine filtering conditions ---------------------------------------------
    
    combined_filter <- Reduce(function(x) expr(!!x), filter_expr)
    
    
# ==============================================================================
# 4. Define PPD Reference Layers
# ==============================================================================
    
# 4.1 PPD reference layer: 1.2 met ---------------------------------------------
    
    ppd_layer_1.2met <- 
      ppd_ta_layer(ta_values = unique(df_ta$air.temp), 
                   rh_values = unique(df_ta$rel.hum),
                   met = 1.2,
                   linetype = "dashed",
                   colour = "PPD_1.2met", 
                   fill = "PPD_1.2met") 
    
# 4.2 PPD reference layer: 1.1 met ---------------------------------------------    
    
    ppd_layer_1.1met <- 
      ppd_ta_layer(ta_values = unique(df_ta$air.temp),
                   rh_values = unique(df_ta$rel.hum),
                   met = 1.1,
                   linetype = "dotdash",
                   colour = "PPD_1.1met", 
                   fill = "PPD_1.1met") 
  
# 4.3 PPD reference layer: 1.0 met ---------------------------------------------
    
    ppd_layer_1.0met <- 
      ppd_ta_layer(ta_values = unique(df_ta$air.temp), 
                   rh_values = unique(df_ta$rel.hum),
                   met = 1.0,
                   linetype = "dotted",
                   colour = "PPD_1.0met", 
                   fill = "PPD_1.0met") 
    
# ==============================================================================
# 5. Fanger UWTDI Plot
# ==============================================================================
    
# 5.1 Generate Fanger plot -----------------------------------------------------
    
    plot.ta_Fanger <-
      df_ta %>%
      filter(cost.type %in% c("Fanger")) %>%
      filter(!!combined_filter) %>%
      ggplot(aes(x = air.temp, y = total.cost)) +
      
      geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = sex, group = sex), 
                  alpha = 0.35) +
      
      geom_line(aes(color = sex, group = sex), linewidth = 0.75,
                linetype = "solid") +
      
      ppd_layer_1.0met$layer +
      ppd_layer_1.1met$layer +
      ppd_layer_1.2met$layer +
      
      geom_hline(yintercept = 0.2, linewidth = 0.45, linetype = "dotted", color = "grey60") +
      
      scale_x_continuous(expression("Air temperature"~("°"*C)),
                         breaks = seq(0, 50, by = 1),
                         expand = c(0, 0)) +
      
      scale_y_continuous(expression("Posterior UWTDI "~("%")),
                         breaks = seq(0, 1, by = 0.2),
                         labels = function(x) x * 100,
                         limits = c(0, 1)) +
      
      scale_colour_manual(name = "",
                          values = c(palette_sex,  
                                     "PPD_1.0met" = "black",
                                     "PPD_1.1met" = "black",
                                     "PPD_1.2met" = "black"),
                          labels = c(label_sex,
                                     "PPD_1.0met" = "PPD (1.0 met)",
                                     "PPD_1.1met" = "PPD (1.1 met)",
                                     "PPD_1.2met" = "PPD (1.2 met)")) +
      
      scale_fill_manual(name = "",
                        values = c(palette_sex, 
                                   "PPD_1.0met" = NA,
                                   "PPD_1.1met" = NA,
                                   "PPD_1.2met" = NA),
                        labels = c(label_sex,
                                   "PPD_1.0met" = "PPD (1.0 met)",
                                   "PPD_1.1met" = "PPD (1.1 met)",
                                   "PPD_1.2met" = "PPD (1.2 met)")) +
      
      facet_grid(time.onset.h ~ rel.hum,
                 labeller = labeller(time.onset.h = onset_names,
                                     rel.hum = rh_names)) +
      ggtitle("Fanger") +
      
      guides(color = guide_legend(nrow = 2)) +
      
      theme(strip.background = element_rect(fill = NA, color = NA), 
            strip.text = element_text(size = 11, color = 'black'),
            plot.title = element_text(size = 12, 
                                      face = "bold", 
                                      margin = margin(0, 0, -5, -32)),
            
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            
            panel.border = element_rect(colour = "black", fill = NA),
            legend.position = 'bottom',      
            legend.direction = 'vertical',   
            
            legend.box.spacing = unit(0.2, 'cm'),
            plot.caption = element_text(size = 9, hjust = 1, face = 'italic', color = 'gray30')) 
    
    
# ==============================================================================
# 6. Comfort UWTDI Plot
# ==============================================================================
    
# 6.1 Generate Comfort plot ----------------------------------------------------    
    
    plot.ta_Comfort <-
      df_ta %>%
      filter(cost.type %in% c("Comfort")) %>%
      filter(!!combined_filter) %>%
      ggplot(aes(x = air.temp, y = total.cost)) +
      
      geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = sex, group = sex), 
                  alpha = 0.35) +
      
      geom_line(aes(color = sex, group = sex), linewidth = 0.75,
                linetype = "solid") +
      
      ppd_layer_1.0met$layer +
      ppd_layer_1.1met$layer +
      ppd_layer_1.2met$layer +
      
      geom_hline(yintercept = 0.2, linewidth = 0.45, linetype = "dotted", color = "grey60") +
      
      scale_x_continuous(expression("Air temperature"~("°"*C)),
                         breaks = seq(0, 50, by = 1),
                         expand = c(0, 0)) +
      
      scale_y_continuous(expression("Posterior UWTDI "~("%")),
                         breaks = seq(0, 1, by = 0.2),
                         labels = function(x) x * 100,
                         limits = c(0, 1)) +
      
      scale_colour_manual(name = "",
                          values = c(palette_sex,  
                                     "PPD_1.0met" = "black",
                                     "PPD_1.1met" = "black",
                                     "PPD_1.2met" = "black"),
                          labels = c(label_sex,
                                     "PPD_1.0met" = "PPD (1.0 met)",
                                     "PPD_1.1met" = "PPD (1.1 met)",
                                     "PPD_1.2met" = "PPD (1.2 met)")) +
      
      scale_fill_manual(name = "",
                        values = c(palette_sex,
                                   "PPD_1.0met" = NA,
                                   "PPD_1.1met" = NA,
                                   "PPD_1.2met" = NA),
                        labels = c(label_sex,
                                   "PPD_1.0met" = "PPD (1.0 met)",
                                   "PPD_1.1met" = "PPD (1.1 met)",
                                   "PPD_1.2met" = "PPD (1.2 met)")) +

      facet_grid(time.onset.h ~ rel.hum,
                 labeller = labeller(time.onset.h = onset_names,
                                     rel.hum = rh_names)) +
      
      ggtitle("Comfort") +
      
      guides(color = guide_legend(nrow = 2)) +
      
      theme(strip.background = element_rect(fill = NA, color = NA), 
            strip.text = element_text(size = 11, color = 'black'),
            plot.title = element_text(size = 12, 
                                      face = "bold", 
                                      margin = margin(0, 0, -5, -32)),
            
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            
            panel.border = element_rect(colour = "black", fill = NA),
            legend.position = 'bottom',      
            legend.direction = 'vertical',   
            
            legend.box.spacing = unit(0.2, 'cm'),
            plot.caption = element_text(size = 9, hjust = 1, face = 'italic', color = 'gray30'))     
    
    
# ==============================================================================
# 7. Symmetric UWTDI Plot
# ==============================================================================
    
# 7.1 Generate Symmetric plot --------------------------------------------------
    
    plot.ta_Symmetric <-
      df_ta %>%
      filter(cost.type %in% c("Symmetric")) %>% 
      filter(!!combined_filter) %>%
      ggplot(aes(x = air.temp, y = total.cost)) +
      
      geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = sex, group = sex), 
                  alpha = 0.35) +
      
      geom_line(aes(color = sex, group = sex), linewidth = 0.75,
                linetype = "solid") +
      
      ppd_layer_1.0met$layer +
      ppd_layer_1.1met$layer +
      ppd_layer_1.2met$layer +
      
      geom_hline(yintercept = 0.2, linewidth = 0.45, linetype = "dotted", color = "grey60") +
      
      scale_x_continuous(expression("Air temperature"~("°"*C)),
                         breaks = seq(0, 50, by = 1),
                         expand = c(0, 0)) +
      
      scale_y_continuous(expression("Posterior UWTDI "~("%")),
                         breaks = seq(0, 1, by = 0.2),
                         labels = function(x) x * 100,
                         limits = c(0, 1)) +
      
      scale_colour_manual(name = "",
                          values = c(palette_sex,  
                                     "PPD_1.0met" = "black",
                                     "PPD_1.1met" = "black",
                                     "PPD_1.2met" = "black"),
                          labels = c(label_sex,
                                     "PPD_1.0met" = "PPD (1.0 met)",
                                     "PPD_1.1met" = "PPD (1.1 met)",
                                     "PPD_1.2met" = "PPD (1.2 met)")) +
      
      scale_fill_manual(name = "",
                        values = c(palette_sex, 
                                   "PPD_1.0met" = NA,
                                   "PPD_1.1met" = NA,
                                   "PPD_1.2met" = NA),
                        labels = c(label_sex,
                                   "PPD_1.0met" = "PPD (1.0 met)",
                                   "PPD_1.1met" = "PPD (1.1 met)",
                                   "PPD_1.2met" = "PPD (1.2 met)")) +

      facet_grid(time.onset.h ~ rel.hum,
                 labeller = labeller(time.onset.h = onset_names,
                                     rel.hum = rh_names)) +
      
      ggtitle("Symmetric") +
      
      guides(color = guide_legend(nrow = 2)) +
      
      theme(strip.background = element_rect(fill = NA, color = NA), 
            strip.text = element_text(size = 11, color = 'black'),
            plot.title = element_text(size = 12, 
                                      face = "bold", 
                                      margin = margin(0, 0, -5, -32)),
            
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            
            panel.border = element_rect(colour = "black", fill = NA),
            legend.position = 'bottom',      
            legend.direction = 'vertical',  
            
            legend.box.spacing = unit(0.2, 'cm'),
            plot.caption = element_text(size = 9, hjust = 1, face = 'italic', color = 'gray30')) 
    
    
# ==============================================================================
# 8. Combine UWTDI Plots
# ==============================================================================
    
# 8.1 Define shared y-axis title -----------------------------------------------
    
    y_axis_plot <- ggplot() +
      theme_void() +
      labs(y = "Posterior UWTDI  (%)") +
      theme(axis.title.y = element_text(size = 11, angle = 90),
            plot.margin = margin(0, 0, 0, 0)
      )

      
# 8.2 Remove individual x-axis titles ------------------------------------------
    
    plot.ta_Fanger <- 
      plot.ta_Fanger +
      theme(axis.title.x = element_blank())
    
    plot.ta_Comfort <- 
      plot.ta_Comfort +
      theme(axis.title.x = element_blank())
            
    plot.ta_Symmetric <- plot.ta_Symmetric 

        
# 8.3 Define plot spacing ------------------------------------------------------    
    
    top_gap_theme <- 
      theme(plot.margin = margin(t = 12, r = 5, b = 5, l = 5, unit = "pt"))
    
    
# 8.4 Combine cost-scheme plots ------------------------------------------------    
    plot.ta <-
      (plot.ta_Fanger / (plot.ta_Comfort) / (plot.ta_Symmetric) +
         
         plot_layout(guides = "collect")) +
      
      plot_layout(widths = c(0.01, 1)) &
      theme(legend.position = "bottom",
            plot.tag.position = c(0.03, 1),
            legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
            legend.box.margin = margin(t = -15, r = 0, b = 0, l = 0, unit = "pt"))
    
  
# ==============================================================================
# 9. Save Temperature Visualisations
# ==============================================================================
    
# 9.1 Save combined plot -------------------------------------------------------
    
    ggsave(plot.ta, filename = file.path(output_figure_folder,
                                         paste0("All-schemes","_temp_vary-met-values","_fix-", values[[1]], ".png")),
           width = 12, 
           height = 30,
           units = "cm",
           dpi = 900)
    
    }

# 9.2 Clean model-specific objects ---------------------------------------------  
  
  rm(df_ta,
     plot.ta_Fanger,
     plot.ta_Comfort,
     plot.ta_Symmetric)
  
  gc()
  
}

# ==============================================================================
# 10. Completion
# ==============================================================================

message("\nPPD visualisation completed")
message("----------------------------------------\n")


# ==============================================================================
# 11. Unload Packages
# ==============================================================================

packages_to_unload <- 
  c("patchwork")

for (package in packages_to_unload) {
  
  if (paste0("package:", package) %in% search()) {
    
    detach(paste0("package:", package),
           unload = FALSE,
           character.only = TRUE)
    
  }
  
}
