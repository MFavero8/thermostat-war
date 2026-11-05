# ==============================================================================
# Script: 09a_1_visualisation_heatmap.R
#
# Author: Matteo Favero
# ==============================================================================

message("----------------------------------------")
message("Expected TSV visualisation in progress\n")


# ==============================================================================
# 0. Setup
# ==============================================================================

# 0.1 Load packages ------------------------------------------------------------

suppressPackageStartupMessages({
  library(comf)
  library(metR)
  library(patchwork)
})


# 0.2 Load configuration -------------------------------------------------------

source("src/scripts/09/visualisation_setting.R")


# ==============================================================================
# 1. Loop Over Models
# ==============================================================================

for (model_name in models_to_process) {
  
  message("Processing ",
          model_name)
  

  # 1.1 Define figure folder ---------------------------------------------------

  output_figure_folder <-
    file.path(figure_folder,
              model_name,
              "pmv_comparison")
  
  if (!dir.exists(output_figure_folder)) {
    
    dir.create(output_figure_folder,
               recursive = TRUE)
    
  }
  

  # 1.2 Define prediction folders ----------------------------------------------
  
  input_prediction_folder <-
    file.path(prediction_folder,
              model_name,
              "pmv_comparison")
  
  
  # 1.3 Load posterior expected TSV predictions -------------------------------
  
  input_file <- 
    file.path(input_prediction_folder,
              "epred_mean_TSV.rds")
  
  
  epred_mean.prob <- readRDS(input_file)

  
  # ============================================================================
  # 2. Define PMV Contour Layers
  # ============================================================================
  
  # 2.1 PMV contour layer at 1.0 met -------------------------------------------
  
  pmv_layer_1.0met <- 
    pmv_contour_layer(ta_range = range(epred_mean.prob$air.temp),
                      rh_range = range(epred_mean.prob$rel.hum),
                      met = 1.0,     
                      linetype = "dotted")
  
  
  # 2.2 PMV contour layer at 1.1 met -------------------------------------------
  
  pmv_layer_1.1met <-
    pmv_contour_layer(ta_range = range(epred_mean.prob$air.temp),
                      rh_range = range(epred_mean.prob$rel.hum),
                      met = 1.1,     
                      linetype = "dotdash")
  
  
  # 2.3 PMV contour layer at 1.2 met -------------------------------------------
  
  pmv_layer_1.2met <- 
    pmv_contour_layer(ta_range = range(epred_mean.prob$air.temp),
                      rh_range = range(epred_mean.prob$rel.hum),
                      met = 1.2,     
                      linetype = "dashed")
  
  
  # ============================================================================
  # 3. Generate Figures by Day
  # ============================================================================
  
  for (i in seq_len(nrow(values_df_day))) {
    
    values <- values_df_day[i, , drop = FALSE]
    
    new_df <- values_df_day[i, ]
    names(new_df) <- name_day_map[col_day]
    
    
    # 3.1 Construct facet/filter title components ------------------------------
    
    title_parts <- mapply(function(name, value) {
      
      value <- case_when(value == "evening" ~ "evening",
                         value == "afternoon" ~ "afternoon",
                         TRUE ~ as.character(value))
      
      paste0(name, " to ", value)
    },
    names(new_df),
    new_df[1],
    SIMPLIFY = TRUE)
    
    
    ## 3.2 Construct dynamic filter expressions --------------------------------
    
    filter_expr <- lapply(names(values), function(col_name) {
      
      expr(!!sym(col_name) == !!values[[col_name]])
      
    })
    
    
    # 3.3 Combine filter expressions -------------------------------------------
    
    combined_filter <- Reduce(function(x) expr(!!x), filter_expr)
    
    
    # ==========================================================================
    # 4. Plot Expected TSV at 1.2 met
    # ==========================================================================
    
    plot_1.2met <-
      epred_mean.prob %>%
      filter(time.onset.h %in% onset_filter) %>%
      filter(!!combined_filter) %>%
      ggplot(aes(x = air.temp, y = rel.hum, z = mean.vote)) +
      
      geom_raster(interpolate = FALSE, aes(fill = mean.vote)) + 
      
      geom_contour2(aes(label = after_stat(paste0(" ", ifelse(level < 0,
                                                              paste0("\u2212", sprintf("%.1f", abs(level))),  
                                                              sprintf("%.1f", level)),
                                                  " "))),
                    label_size = 2.7,
                    linewidth = 0.2,
                    colour = "white", 
                    linetype = "solid",
                    breaks = fixed_levels,
                    skip = 0, 
                    label.placer = label_placer_fraction(frac = 0.2)) +
      
      pmv_layer_1.2met$layer +
      
      scale_x_continuous(expression("Air temperature"~("°"*C)),
                         breaks = seq(0, 50, by = 1),
                         expand = c(0, 0)) +
      
      scale_y_continuous(expression("Relative humidity"~("%")),
                         breaks = seq(0, 100, by = 10),
                         expand = c(0, 0)) +
      
      scale_fill_gradientn(colors = palette_TSV,
                           values = scales::rescale(c(-3, -2, -1, 0, 1, 2, 3)),
                           limits = c(-3, 3),
                           breaks = seq(-3, 3, by = 0.5),
                           labels = function(x) {
                             ifelse(x < 0,
                                    paste0("\u2212", formatC(abs(x), format = "f", digits = 1)),
                                    formatC(x, format = "f", digits = 1))
                           }, 
                           guide = guide_colourbar(title = "Posterior expected TSV", 
                                                   title.position = "top",    
                                                   title.hjust = 0.5,    
                                                   barwidth = 20,
                                                   barheight = 1)) +
      
      facet_grid(time.onset.h ~ factor(sex, levels = c("male", "female")),
                 labeller = labeller(`factor(sex, levels = c("male", "female"))` = sex_names,
                                     time.onset.h = onset_names)) +
      
      ggtitle("1.2 met") +
      
      theme(strip.background = element_rect(fill = NA, color = NA), 
            strip.text = element_text(size = 11, color = "black"),
            plot.title = element_text(size = 12, 
                                      face = "bold", 
                                      margin = margin(0, 0, -5, -32)),
            legend.position = "bottom", 
            legend.direction = "horizontal", 
            legend.box.spacing = unit(0.2, "cm"),
            plot.caption = element_text(size = 9, hjust = 1, face = "italic", color = "gray30")) 
    
    
    # ==========================================================================
    # 5. Plot Expected TSV at 1.1 met
    # ==========================================================================
    
    plot_1.1met <-
      epred_mean.prob %>%
      filter(time.onset.h %in% onset_filter) %>%
      filter(!!combined_filter) %>%
      ggplot(aes(x = air.temp, y = rel.hum, z = mean.vote)) +
      
      geom_raster(interpolate = FALSE, aes(fill = mean.vote)) + 
      
      geom_contour2(aes(label = after_stat(paste0(" ", ifelse(level < 0,
                                                              paste0("\u2212", sprintf("%.1f", abs(level))),  
                                                              sprintf("%.1f", level)),
                                                  " "))),
                    label_size = 2.7,
                    linewidth = 0.2, 
                    colour = "white", 
                    linetype = "solid",
                    breaks = fixed_levels,
                    skip = 0, 
                    label.placer = label_placer_fraction(frac = 0.2)) +
      
      pmv_layer_1.1met$layer +
      
      scale_x_continuous(expression("Air temperature"~("°"*C)),
                         breaks = seq(0, 50, by = 1),
                         expand = c(0, 0)) +
      
      scale_y_continuous(expression("Relative humidity"~("%")),
                         breaks = seq(0, 100, by = 10),
                         expand = c(0, 0)) +
      
      scale_fill_gradientn(colors = palette_TSV,
                           values = scales::rescale(c(-3, -2, -1, 0, 1, 2, 3)),
                           limits = c(-3, 3),
                           breaks = seq(-3, 3, by = 0.5),
                           labels = function(x) {
                             ifelse(x < 0,
                                    paste0("\u2212", formatC(abs(x), format = "f", digits = 1)),
                                    formatC(x, format = "f", digits = 1))
                           }, 
                           guide = guide_colourbar(title = "Posterior expected TSV", 
                                                   title.position = "top",      
                                                   title.hjust = 0.5,          
                                                   barwidth = 20,   
                                                   barheight = 1)) +
      
      facet_grid(time.onset.h ~ factor(sex, levels = c("male", "female")),
                 labeller = labeller(`factor(sex, levels = c("male", "female"))` = sex_names,
                                     time.onset.h = onset_names)) +
      
      ggtitle("1.1 met") +
      
      theme(strip.background = element_rect(fill = NA, color = NA), 
            strip.text = element_text(size = 11, color = "black"),
            plot.title = element_text(size = 12, 
                                      face = "bold", 
                                      margin = margin(0, 0, -5, -32)),
            legend.position = "bottom",      
            legend.direction = "horizontal",  
            legend.box.spacing = unit(0.2, "cm"),
            plot.caption = element_text(size = 9, hjust = 1, face = "italic", color = "gray30")) 
    
    
    # ==========================================================================
    # 6. Plot Expected TSV at 1.0 met
    # ==========================================================================
    
    plot_1.0met <-
      epred_mean.prob %>%
      filter(time.onset.h %in% onset_filter) %>%
      filter(!!combined_filter) %>%
      ggplot(aes(x = air.temp, y = rel.hum, z = mean.vote)) +
      
      geom_raster(interpolate = FALSE, aes(fill = mean.vote)) + 
      
      geom_contour2(aes(label = after_stat(paste0(" ", ifelse(level < 0,
                                                              paste0("\u2212", sprintf("%.1f", abs(level))), 
                                                              sprintf("%.1f", level)),
                                                  " "))),
                    label_size = 2.7,
                    linewidth = 0.2,
                    colour = "white", 
                    linetype = "solid", 
                    breaks = fixed_levels,
                    skip = 0, 
                    label.placer = label_placer_fraction(frac = 0.2)) +
      
      pmv_layer_1.0met$layer +
      
      scale_x_continuous(expression("Air temperature"~("°"*C)),
                         breaks = seq(0, 50, by = 1),
                         expand = c(0, 0)) +
      
      scale_y_continuous(expression("Relative humidity"~("%")),
                         breaks = seq(0, 100, by = 10),
                         expand = c(0, 0)) +
      
      scale_fill_gradientn(colors = palette_TSV,
                           values = scales::rescale(c(-3, -2, -1, 0, 1, 2, 3)),
                           limits = c(-3, 3),
                           breaks = seq(-3, 3, by = 0.5),
                           labels = function(x) {
                             ifelse(x < 0,
                                    paste0("\u2212", formatC(abs(x), format = "f", digits = 1)),
                                    formatC(x, format = "f", digits = 1))
                           }, 
                           guide = guide_colourbar(title = "Posterior expected TSV", 
                                                   title.position = "top",      
                                                   title.hjust = 0.5,   
                                                   barwidth = 20,   
                                                   barheight = 1)) +

      facet_grid(time.onset.h ~ factor(sex, levels = c("male", "female")),
                 labeller = labeller(`factor(sex, levels = c("male", "female"))` = sex_names,
                                     time.onset.h = onset_names)) +
      
      ggtitle("1.0 met") +
      
      theme(strip.background = element_rect(fill = NA, color = NA), 
            strip.text = element_text(size = 11, color = "black"),
            plot.title = element_text(size = 12, 
                                      face = "bold", 
                                      margin = margin(0, 0, -5, -32)),
            legend.position = "bottom",  
            legend.direction = "horizontal",
            legend.box.spacing = unit(0.2, "cm"),
            plot.caption = element_text(size = 9, hjust = 1, face = "italic", color = "gray30")) 
    
    
    # ==========================================================================
    # 7. Combine Metabolic Rate Figures
    # ==========================================================================
    
    # 7.1 Remove repeated x-axis titles -----------------------------------------
    
    plot_1.2met <- 
      plot_1.2met +
      theme(axis.title.x = element_blank()) 
    
    plot_1.1met <- 
      plot_1.1met +
      theme(axis.title.x = element_blank()) 
    
    
    # 7.2 Stack figures vertically and collect legend ---------------------------
    
    combined_plot <-
      (plot_1.2met / plot_1.1met / plot_1.0met) +
      plot_layout(guides = "collect") &
      theme(legend.position = "bottom",
            legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
            legend.box.margin = margin(t = -5, r = 0, b = 0, l = 0, unit = "pt"))
    
    
    # ==========================================================================
    # 8. Save Combined Figure
    # ==========================================================================
    
    ggsave(combined_plot, 
           filename = file.path(output_figure_folder,
                                paste0("Expected-TSV-by-sex_fix-", values[[1]],  ".png")),
           width = 12,
           height = 30,
           units = "cm",
           dpi = 900)
    
  }
  
  # ============================================================================
  # 9. Clean Model Objects
  # ============================================================================
  
  rm(plot_1.2met,
     plot_1.1met,
     plot_1.0met,
     combined_plot,
     epred_mean.prob)
  
  gc()
  
}


# ==============================================================================
# 10. Completion
# ==============================================================================

message("\nExpected TSV visualisation completed")
message("----------------------------------------\n")
