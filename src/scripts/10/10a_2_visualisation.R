# ==============================================================================
# Script: 10a_2_visualisation.R
#
# Description: Risk prediction visualisation
#
# Author: Matteo Favero
# ==============================================================================

message("----------------------------------------")
message("Risk prediction visualisation in progress\n")


# ==============================================================================
# 0. Load Configuration
# ==============================================================================

source("src/scripts/10/visualisation_setting.R")


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
              "risk_prediction")
  
  if (!dir.exists(output_figure_folder)) {
    
    dir.create(output_figure_folder,
               recursive = TRUE)
    
  }
  
  
  # 1.2 Define prediction folder -----------------------------------------------
  
  input_prediction_folder <-
    file.path(prediction_folder,
              model_name,
              "intervention")
  
  
  # 1.3 Define prediction files ------------------------------------------------
  
  input_file_heating <- 
    file.path(input_prediction_folder,
              paste0("risk_pred_", 
                     contrast_name_h, 
                     ".rds"))
  
  input_file_cooling <- 
    file.path(input_prediction_folder,
              paste0("risk_pred_", 
                     contrast_name_c, 
                     ".rds"))
  
  
  # 1.4 Load risk predictions --------------------------------------------------
  
  risk.pred_heating <- readRDS(input_file_heating)
  risk.pred_cooling <- readRDS(input_file_cooling)
  
  
  # ============================================================================
  # 2. Figures by Sex
  # ============================================================================

  for (i in seq_len(nrow(values_df_day))) {
    
    values <- values_df_day[i, , drop = FALSE]
    
    
    # 2.1 Prepare facet title information --------------------------------------
    
    new_df <- values_df_day[i, ]
    names(new_df) <- name_day_map[col_day]
    
    title_parts <- mapply(function(name, value) {
      
      value <- case_when(value == "evening" ~ "evening",
                         value == "afternoon" ~ "afternoon",
                         TRUE ~ as.character(value))
      
      paste0(name, " to ", value)
    },
    names(new_df),
    new_df[1],
    SIMPLIFY = TRUE)
    
    
    # 2.2 Create filtering expressions -----------------------------------------
    

    filter_expr <- lapply(names(values), function(col_name) {

      expr(!!sym(col_name) == !!values[[col_name]])
      
    })
    
    
    # 2.3 Combine filtering expressions ----------------------------------------
    
    combined_filter <- Reduce(function(x) expr(!!x), filter_expr)
    
    
    # 2.4 Create heating risk prediction plot ----------------------------------
    
    plot_sex_heating <-
      risk.pred_heating %>%
      filter(!!combined_filter) %>%
      ggplot(aes(x = time.onset.h, y = prob, ymin = .lower, ymax = .upper)) +
      
      geom_errorbar(aes(colour = as.factor(vote)),
                    width = 0,
                    linewidth = 1.5,
                    alpha = 0.4,
                    position = position_dodge(width = 0.55)) +
      
      geom_point(aes(colour = as.factor(vote)),
                 size = 1.5,
                 alpha = 1,
                 position = position_dodge(width = 0.55)) + 
      
      scale_x_continuous("Time since onset (h)",
                         breaks = c(1.0, 2.0, 3.0), 
                         labels = c("1.0", "2.0", "3.0")) +
      
      scale_y_continuous(expression("Posterior predictive probability"~("%")),  
                         breaks = seq(0, 1, by = 0.2),
                         labels = function(x) x * 100,
                         limits = c(0, 1)) +
      
      scale_colour_manual("Thermal sensation vote",
                          values = palette_TSV,
                          labels = vote_names, 
                          guide = guide_legend(title.position = "top",    
                                               title.hjust = 0.5,  
                                               nrow = 1, 
                                               byrow = TRUE)) +
      
      facet_grid(factor(sex, levels = c("male", "female"), 
                        labels = c("Male", "Female")) ~ facet_setpoint) +
      
      theme(strip.background = element_rect(fill = NA, color = NA), 
            strip.text = element_text(size = 11, color = "black"),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(colour = "grey95"),
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(colour = "black", 
                                        fill = NA),
            legend.position = "bottom",   
            legend.direction = "horizontal", 
            legend.box.spacing = unit(0.2, "cm"),
            plot.caption = element_text(size = 9, hjust = 1, face = "italic", color = "gray30")) 
    
    
    # 2.5 Create cooling risk prediction plot ----------------------------------
    
    plot_sex_cooling <-
      risk.pred_cooling %>%
      filter(!!combined_filter) %>%
      ggplot(aes(x = time.onset.h, y = prob, ymin = .lower, ymax = .upper)) +
      
      geom_errorbar(aes(colour = as.factor(vote)),
                    width = 0,
                    linewidth = 1.5,
                    alpha = 0.4,
                    position = position_dodge(width = 0.55)) +
      
      geom_point(aes(colour = as.factor(vote)),
                 size = 1.5,
                 alpha = 1,
                 position = position_dodge(width = 0.55)) + 
      
      scale_x_continuous("Time since onset (h)",
                         breaks = c(1.0, 2.0, 3.0), 
                         labels = c("1.0", "2.0", "3.0")) +
      
      scale_y_continuous(expression("Posterior predictive probability"~("%")),
                         breaks = seq(0, 1, by = 0.2),
                         labels = function(x) x * 100,
                         limits = c(0, 1)) +
      
      scale_colour_manual("Thermal sensation vote",
                          values = palette_TSV, 
                          labels = vote_names, 
                          guide = guide_legend(title.position = "top",    
                                               title.hjust = 0.5,  
                                               nrow = 1, 
                                               byrow = TRUE)) +
      
      facet_grid(factor(sex, levels = c("male", "female"), 
                        labels = c("Male", "Female")) ~ facet_setpoint) +
      
      theme(strip.background = element_rect(fill = NA, color = NA), 
            strip.text = element_text(size = 11, color = "black"),
            panel.grid.major.x = element_blank(), 
            panel.grid.major.y = element_line(colour = "grey95"),
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(colour = "black", 
                                        fill = NA),
            legend.position = "bottom", 
            legend.direction = "horizontal", 
            legend.box.spacing = unit(0.2, "cm"),
            plot.caption = element_text(size = 9, hjust = 1, face = "italic", color = "gray30")) 
    
    
    # 2.6 Create shared y-axis title plot --------------------------------------
    
    y_axis_plot <- 
      ggplot() +
      theme_void() +
      labs(y = "Posterior predictive probability (%)") +
      theme(axis.title.y = element_text(size = 11, angle = 90),
            plot.margin = margin(0, 0, 0, 0))
    
    
    # 2.7 Remove individual y-axis titles --------------------------------------
    
    plot_sex_heating <- 
      plot_sex_heating +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      labs(tag = "a)")
    
    plot_sex_cooling <- 
      plot_sex_cooling +
      theme(axis.title.y = element_blank()) +
      labs(tag = "b)")
    
    
    # 2.8 Combine heating and cooling plots ------------------------------------
    
    combined_plot <-
      y_axis_plot +
      (plot_sex_heating / plot_sex_cooling +
         plot_layout(guides = "collect")) +
      plot_layout(widths = c(0.01, 1)) &
      theme(legend.position = "bottom",
            plot.tag.position = c(0.03, 1),
            plot.tag = element_text(size = 12,
                                    face = "bold"))
    
    
    # 2.9 Save combined plot ---------------------------------------------------
    
    ggsave(combined_plot, 
           filename = file.path(output_figure_folder,
                                paste0("Risk-prediction-by-sex_fix-", values[[1]],  ".png")),
           width = 18,
           height = 19,
           units = "cm",
           dpi = 900)
    
  }
  
  
  # 2.10 Remove sex-specific plotting objects ----------------------------------
  
  rm(plot_sex_heating,
     plot_sex_cooling,
     combined_plot)
  
  
  # ============================================================================
  # 3. Figures by Day
  # ============================================================================
  
  for (i in seq_len(nrow(values_df_sex))) {
    
    values <- values_df_sex[i, , drop = FALSE]
    
    
    # 3.1 Prepare facet title information --------------------------------------
    
    new_df <- values_df_sex[i, ]
    names(new_df) <- name_sex_map[col_sex]
    
    title_parts <- mapply(function(name, value) {
      
      value <- case_when(value == "male" ~ "male",
                         value == "female" ~ "female",
                         TRUE ~ as.character(value))
      
      paste0(name, " to ", value)
    },
    names(new_df),
    new_df[1],
    SIMPLIFY = TRUE)
    
    
    # 3.2 Create filtering expressions -----------------------------------------
    
    filter_expr <- lapply(names(values), function(col_name) {
     
      expr(!!sym(col_name) == !!values[[col_name]])
      
    })
    
    
    # 3.3 Combine filtering expressions ----------------------------------------
    
    combined_filter <- Reduce(function(x) expr(!!x), filter_expr)
    
    
    # 3.4 Create heating risk prediction plot ----------------------------------
    
    plot_day_heating <-
      risk.pred_heating %>%
      filter(!!combined_filter) %>%
      ggplot(aes(x = time.onset.h, y = prob, ymin = .lower, ymax = .upper)) +
      
      geom_errorbar(aes(colour = as.factor(vote)),
                    width = 0,
                    linewidth = 1.5,
                    alpha = 0.4,
                    position = position_dodge(width = 0.55)) +
      
      geom_point(aes(colour = as.factor(vote)),
                 size = 1.5,
                 alpha = 1,
                 position = position_dodge(width = 0.55)) + 
      
      scale_x_continuous("Time since onset (h)",
                         breaks = c(1.0, 2.0, 3.0), 
                         labels = c("1.0", "2.0", "3.0")) +
      
      scale_y_continuous(expression("Posterior predictive probability"~("%")),  
                         breaks = seq(0, 1, by = 0.2),
                         labels = function(x) x * 100,
                         limits = c(0, 1)) +
      
      scale_colour_manual("Thermal sensation vote",
                          values = palette_TSV, 
                          labels = vote_names, 
                          guide = guide_legend(title.position = "top",    
                                               title.hjust = 0.5,  
                                               nrow = 1, 
                                               byrow = TRUE)) +
      
      facet_grid(factor(time.day, levels = c("afternoon", "evening"), 
                        labels = c("Afternoon", "Evening")) ~ facet_setpoint) +
      
      theme(strip.background = element_rect(fill = NA, color = NA), 
            strip.text = element_text(size = 11, color = "black"),
            panel.grid.major.x = element_blank(), 
            panel.grid.major.y = element_line(colour = "grey95"),
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            
            panel.border = element_rect(colour = "black", 
                                        fill = NA),
            legend.position = "bottom",   
            legend.direction = "horizontal", 
            legend.box.spacing = unit(0.2, "cm"),
            plot.caption = element_text(size = 9, hjust = 1, face = "italic", color = "gray30")) 
    
    
    # 3.5 Create cooling risk prediction plot ----------------------------------
    
    plot_day_cooling <-
      risk.pred_cooling %>%
      filter(!!combined_filter) %>%
      ggplot(aes(x = time.onset.h, y = prob, ymin = .lower, ymax = .upper)) +
      
      geom_errorbar(aes(colour = as.factor(vote)),
                    width = 0,
                    linewidth = 1.5,
                    alpha = 0.4,
                    position = position_dodge(width = 0.55)) +
      
      geom_point(aes(colour = as.factor(vote)),
                 size = 1.5,
                 alpha = 1,
                 position = position_dodge(width = 0.55)) + 
      
      scale_x_continuous("Time since onset (h)",
                         breaks = c(1.0, 2.0, 3.0), 
                         labels = c("1.0", "2.0", "3.0")) +
      
      scale_y_continuous(expression("Posterior predictive probability"~("%")),
                         breaks = seq(0, 1, by = 0.2),
                         labels = function(x) x * 100,
                         limits = c(0, 1)) +
      
      scale_colour_manual("Thermal sensation vote",
                          values = palette_TSV, 
                          labels = vote_names, 
                          guide = guide_legend(title.position = "top",    
                                               title.hjust = 0.5,  
                                               nrow = 1, 
                                               byrow = TRUE)) +
      
      facet_grid(factor(time.day, levels = c("afternoon", "evening"), 
                        labels = c("Afternoon", "Evening")) ~ facet_setpoint) +
      
      theme(strip.background = element_rect(fill = NA, color = NA), 
            strip.text = element_text(size = 11, color = "black"),
            panel.grid.major.x = element_blank(), 
            panel.grid.major.y = element_line(colour = "grey95"),
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(colour = "black", 
                                        fill = NA),
            legend.position = "bottom", 
            legend.direction = "horizontal", 
            legend.box.spacing = unit(0.2, "cm"),
            plot.caption = element_text(size = 9, hjust = 1, face = "italic", color = "gray30")) 
    
    
    
    # 3.6 Create shared y-axis title plot --------------------------------------
    
    y_axis_plot <- 
      ggplot() +
      theme_void() +
      labs(y = "Posterior predictive probability (%)") +
      theme(axis.title.y = element_text(size = 11, angle = 90),
            plot.margin = margin(0, 0, 0, 0))
    
    
    # 3.7 Remove individual y-axis titles --------------------------------------
    
    plot_day_heating <- 
      plot_day_heating +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      labs(tag = "a)")
    
    plot_day_cooling <- 
      plot_day_cooling +
      theme(axis.title.y = element_blank()) +
      labs(tag = "b)")
    
    
    # 3.8 Combine heating and cooling plots ------------------------------------
    
    combined_plot <-
      y_axis_plot +
      (plot_day_heating / plot_day_cooling +
         plot_layout(guides = "collect")) +
      plot_layout(widths = c(0.01, 1)) &
      theme(legend.position = "bottom",
            plot.tag.position = c(0.03, 1),
            plot.tag = element_text(size = 12,
                                    face = "bold"))
    
    
    # 3.9 Save combined plot ---------------------------------------------------
    
    ggsave(combined_plot, 
           filename = file.path(output_figure_folder,
                                paste0("Risk-prediction-by-day_fix-", values[[1]],  ".png")),
           width = 18,
           height = 19,
           units = "cm",
           dpi = 900)
  }
  
  
  # 3.10 Remove day-specific plotting objects ----------------------------------
  
  rm(plot_day_heating,
     plot_day_cooling,
     combined_plot, 
     risk.pred_heating,
     risk.pred_cooling)

}


# ==============================================================================
# 4. Completion
# ==============================================================================

message("\nRisk prediction visualisation completed")
message("----------------------------------------\n")
