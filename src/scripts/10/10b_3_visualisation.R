# ==============================================================================
# Script: 10b_3_visualisation.R
#
# Description: Risk difference and effect modification visualisation
#              for fitted Bayesian models
#
# Author: Matteo Favero
# ==============================================================================

message("----------------------------------------")
message("Risk difference and effect modification visualisation in progress\n")


# ==============================================================================
# 1. Loop Over Models
# ==============================================================================

for (model_name in models_to_process) {
  
  message("Processing ",
          model_name)
  
  
  # 1.1 Define figure output folder --------------------------------------------
  
  output_figure_folder <-
    file.path(figure_folder,
              model_name,
              "risk_difference_and_effect_modification")
  
  if (!dir.exists(output_figure_folder)) {
    
    dir.create(output_figure_folder,
               recursive = TRUE)
    
  }

  
  # 1.2 Define prediction input folder -----------------------------------------
  
  input_prediction_folder <-
    file.path(prediction_folder,
              model_name,
              "intervention")
  
  
  # 1.3 Load risk difference predictions ---------------------------------------
  
  input_file_heating <- 
    file.path(input_prediction_folder,
              paste0("risk_diff_", 
                     contrast_name_h, 
                     ".rds"))
  
  input_file_cooling <- 
    file.path(input_prediction_folder,
              paste0("risk_diff_", 
                     contrast_name_c, 
                     ".rds"))
  
  risk.diff_heating <- readRDS(input_file_heating)
  risk.diff_cooling <- readRDS(input_file_cooling)
  

  # 1.4 Load sex effect modification predictions -------------------------------
  
  input_file_sex_heating <- 
    file.path(input_prediction_folder,
              paste0("eff_mod_sex_", 
                     contrast_name_h, 
                     ".rds"))
  
  input_file_sex_cooling <- 
    file.path(input_prediction_folder,
              paste0("eff_mod_sex_", 
                     contrast_name_c, 
                     ".rds"))
  
  # 1.5 Load day effect modification predictions -------------------------------
  
   input_file_day_heating <- 
    file.path(input_prediction_folder,
              paste0("eff_mod_day_", 
                     contrast_name_h, 
                     ".rds"))
  
  input_file_day_cooling <- 
    file.path(input_prediction_folder,
              paste0("eff_mod_day_", 
                     contrast_name_c, 
                     ".rds"))

  
  eff.mod_sex_heating <- readRDS(input_file_sex_heating)
  eff.mod_sex_cooling <- readRDS(input_file_sex_cooling)

  eff.mod_day_heating <- readRDS(input_file_day_heating)
  eff.mod_day_cooling <- readRDS(input_file_day_cooling)
  

  # ============================================================================
  # 2. Figures by Sex
  # ============================================================================
  
  for (i in seq_len(nrow(values_df_day))) {
    
    values <- values_df_day[i, , drop = FALSE]
    
    new_df <- values_df_day[i, ]
    names(new_df) <- name_day_map[col_day]
    
    
    # 2.1 Generate facet title components --------------------------------------
    
    title_parts <- mapply(function(name, value) {
      
      value <- case_when(value == "evening" ~ "evening",
                         value == "afternoon" ~ "afternoon",
                         TRUE ~ as.character(value))
      
      paste0(name, " to ", value)
    },
    names(new_df),
    new_df[1],
    SIMPLIFY = TRUE)
    
    
    # 2.2 Construct dynamic filtering expression -------------------------------
  
    filter_expr <- lapply(names(values), function(col_name) {

      expr(!!sym(col_name) == !!values[[col_name]])
      
    })
    
    
    # Combine all filter expressions using an AND condition
    
    combined_filter <- Reduce(function(x) expr(!!x), filter_expr)
    
    
    # 2.3 Risk difference plot: heating ----------------------------------------
    
    plot_risk.diff_heating <-
      risk.diff_heating %>%
      filter(!!combined_filter) %>%
      ggplot(aes(x = time.onset.h, y = .data[[contrast_name_h]], ymin = .lower, ymax = .upper)) +
      
      geom_errorbar(aes(colour = as.factor(vote)),
                    width = 0,
                    linewidth = 1.5,
                    alpha = 0.4,
                    position = position_dodge(width = 0.75)) +
      
      geom_point(aes(colour = as.factor(vote)),
                 size = 1.5,
                 alpha = 1,
                 position = position_dodge(width = 0.75)) +     
      
      geom_hline(yintercept = 0, color = "grey50", linetype = "dotted", linewidth = 0.35) +
      
      scale_x_continuous("Time since onset (h)",
                         breaks = c(1.0, 2.0, 3.0),
                         labels = c("1.0","2.0","3.0")) +
      
      scale_y_continuous(expression("Posterior category-specific CATE"~("%")),
                         breaks = seq(-1, 1, by = 0.2),
                         labels = function(x) x * 100,
                         limits = c(-0.4, 0.4)) +
      
      scale_colour_manual("Thermal sensation vote",
                          values = palette_TSV, 
                          labels = vote_names, 
                          guide = guide_legend(title.position = "top",  
                                               title.hjust = 0.5, 
                                               nrow = 1, 
                                               byrow = TRUE)) +
      
      facet_grid(. ~ factor(sex, levels = c("male", "female"), 
                            labels = c("Male", "Female"))) +
      
      theme(strip.background = element_rect(fill = NA, color = NA), 
            strip.text = element_text(size = 11, color = "black"),
            panel.grid.major.x = element_blank(), 
            panel.grid.major.y = element_line(colour = "grey95"),
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(colour = "black", fill = NA),
            legend.position = "bottom",    
            legend.direction = "horizontal",  
            legend.box.spacing = unit(0.2, "cm"),
            plot.caption = element_text(size = 9, hjust = 1, face = "italic", color = "gray30")) 
    
    
    # 2.4 Risk difference plot: cooling ----------------------------------------
    
    plot_risk.diff_cooling <-
      risk.diff_cooling %>%
      filter(!!combined_filter) %>%
      ggplot(aes(x = time.onset.h, y = .data[[contrast_name_c]], ymin = .lower, ymax = .upper)) +
      
      geom_errorbar(aes(colour = as.factor(vote)),
                    width = 0,
                    linewidth = 1.5,
                    alpha = 0.4,
                    position = position_dodge(width = 0.75)) +
      
      geom_point(aes(colour = as.factor(vote)),
                 size = 1.5,
                 alpha = 1,
                 position = position_dodge(width = 0.75)) +     
      
      geom_hline(yintercept = 0, color = "grey50", linetype = "dotted", linewidth = 0.35) +
      
      scale_x_continuous("Time since onset (h)",
                         breaks = c(1.0, 2.0, 3.0),
                         labels = c("1.0","2.0","3.0")) +
      
      scale_y_continuous(expression("Posterior category-specific CATE"~("%")),
                         breaks = seq(-1, 1, by = 0.2),
                         labels = function(x) x * 100,
                         limits = c(-0.4, 0.4)) +
      
      scale_colour_manual("Thermal sensation vote",
                          values = palette_TSV,
                          labels = vote_names, 
                          guide = guide_legend(title.position = "top",   
                                               title.hjust = 0.5,  
                                               nrow = 1, 
                                               byrow = TRUE)) +
      
      facet_grid(. ~ factor(sex, levels = c("male", "female"), 
                            labels = c("Male", "Female"))) +
      
      theme(strip.background = element_rect(fill = NA, color = NA), 
            strip.text = element_text(size = 11, color = "black"),
            panel.grid.major.x = element_blank(), 
            panel.grid.major.y = element_line(colour = "grey95"),
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(colour = "black", fill = NA),
            legend.position = "bottom",      
            legend.direction = "horizontal",
            legend.box.spacing = unit(0.2, "cm"),
            plot.caption = element_text(size = 9, hjust = 1, face = "italic", color = "gray30")) 
    
    
    # 2.5 Effect modification plot by sex: heating -----------------------------
    
    plot_eff.mod_sex_heating <-
      eff.mod_sex_heating %>%
      filter(!!combined_filter) %>%
      ggplot(aes(x = time.onset.h, y = EM.sex_M.F, ymin = .lower, ymax = .upper)) +
      
      geom_errorbar(aes(colour = as.factor(vote)),
                    width = 0,
                    linewidth = 1.5,
                    alpha = 0.4,
                    position = position_dodge(width = 0.75)) +
      
      geom_point(aes(colour = as.factor(vote)),
                 size = 1.5,
                 alpha = 1,
                 position = position_dodge(width = 0.75)) +     
      
      geom_hline(yintercept = 0, color = "grey50", linetype = "dotted", linewidth = 0.35) +
      
      scale_x_continuous("Time since onset (h)",
                         breaks = c(1.0, 2.0, 3.0),
                         labels = c("1.0","2.0","3.0")) +
      
      scale_y_continuous("Posterior difference in CATE (%)", 
                         breaks = seq(-1, 1, by = 0.2),
                         labels = function(x) x * 100,
                         limits = c(-0.4, 0.4)) +
      
      scale_colour_manual("Thermal sensation vote",
                          values = palette_TSV,
                          labels = vote_names, 
                          guide = guide_legend(title.position = "top",   
                                               title.hjust = 0.5,  
                                               nrow = 1,
                                               byrow = TRUE)) +
      
      facet_grid(setpoint ~ diff) +
      
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
    
    
    # 2.6 Effect modification plot by sex: cooling -----------------------------
    
    plot_eff.mod_sex_cooling <-
      eff.mod_sex_cooling %>%
      filter(!!combined_filter) %>%
      ggplot(aes(x = time.onset.h, y = EM.sex_M.F, ymin = .lower, ymax = .upper)) +
      
      geom_errorbar(aes(colour = as.factor(vote)),
                    width = 0,
                    linewidth = 1.5,
                    alpha = 0.4,
                    position = position_dodge(width = 0.75)) +
      
      geom_point(aes(colour = as.factor(vote)),
                 size = 1.5,
                 alpha = 1,
                 position = position_dodge(width = 0.75)) +     
      
      geom_hline(yintercept = 0, color = "grey50", linetype = "dotted", linewidth = 0.35) +
      
      scale_x_continuous("Time since onset (h)",
                         breaks = c(1.0, 2.0, 3.0),
                         labels = c("1.0","2.0","3.0")) +
      
      scale_y_continuous("Posterior difference in CATE (%)", 
                         breaks = seq(-1, 1, by = 0.2),
                         labels = function(x) x * 100,
                         limits = c(-0.4, 0.4)) +
      
      scale_colour_manual("Thermal sensation vote",
                          values = palette_TSV, 
                          labels = vote_names, 
                          guide = guide_legend(title.position = "top",
                                               title.hjust = 0.5,  
                                               nrow = 1, 
                                               byrow = TRUE)) +
      
      facet_grid(setpoint ~ diff) +
      
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
    
    
    
    # 2.7 Effect modification plot by sex: cooling -----------------------------
    
    y_axis_plot_left <- 
      ggplot() +
      theme_void() +
      labs(y = "Posterior category-specific CATE (%)") + 
      theme(axis.title.y = element_text(size = 11, angle = 90),
            plot.margin = margin(0, 0, 0, 0))
    
    y_axis_plot_right <- 
      ggplot() +
      theme_void() +
      labs(y = "Posterior difference in CATE (%)") +
      theme(axis.title.y = element_text(size = 11, angle = 90),
            plot.margin = margin(0, 0, 0, 0))
    
    
    # 2.8 Add panel labels and remove duplicated axis titles -------------------
    
    plot_risk.diff_heating <- 
      plot_risk.diff_heating +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      labs(tag = "a)")
    
    plot_risk.diff_cooling <- 
      plot_risk.diff_cooling +
      theme(axis.title.y = element_blank()) +
      labs(tag = "c)")
    
    
    plot_eff.mod_sex_heating <- 
      plot_eff.mod_sex_heating +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      labs(tag = "b)")
    
    plot_eff.mod_sex_cooling <- 
      plot_eff.mod_sex_cooling +
      theme(axis.title.y = element_blank()) +
      labs(tag = "d)")
    
    
    # 2.9 Assemble left and right panels ---------------------------------------
    
    left_panel <-
      y_axis_plot_left +
      (plot_risk.diff_heating / plot_risk.diff_cooling) +
      plot_layout(widths = c(0.01, 1))
    
    
    y_axis_plot_right <- y_axis_plot_right +
      theme(plot.margin = margin(0, 0, 0, 6))
    
    
    right_panel <-
      y_axis_plot_right +
      (plot_eff.mod_sex_heating / plot_eff.mod_sex_cooling) +
      plot_layout(widths = c(0.01, 1))
    
    
    # 2.10 Combine panels ------------------------------------------------------
    
    combined_plot <-
      (left_panel | right_panel) +
      plot_layout(guides = "collect", 
                  widths = c(1, 0.55)) &
      theme(legend.position = "bottom",
            plot.tag.position = c(0.03, 1),
            plot.tag = element_text(size = 12,
                                    face = "bold"))
    
    
    # 2.11 Save figure ---------------------------------------------------------
    
    ggsave(combined_plot, 
           filename = file.path(output_figure_folder,
                                paste0("Risk-diff-and-eff-mod-by-sex_fix-", values[[1]],  ".png")),
           width = 18,
           height = 15,
           units = "cm",
           dpi = 900)
    
  } 
  
  # 2.12 Remove sex-specific objects -------------------------------------------
  
  rm(plot_risk.diff_heating,
     plot_risk.diff_cooling,
     plot_eff.mod_sex_heating,
     plot_eff.mod_sex_cooling,
     combined_plot,
     eff.mod_sex_heating,
     eff.mod_sex_cooling)
  
  
  # ============================================================================
  # 3. Figures by Day
  # ============================================================================
  
  for (i in seq_len(nrow(values_df_sex))) {
    
    values <- values_df_sex[i, , drop = FALSE]
    
    new_df <- values_df_sex[i, ]
    names(new_df) <- name_sex_map[col_sex]
    
    
    # 3.1 Generate facet title components --------------------------------------
    
    title_parts <- mapply(function(name, value) {
      
      value <- case_when(value == "male" ~ "male",
                         value == "female" ~ "female",
                         TRUE ~ as.character(value))
      
      paste0(name, " to ", value)
    },
    names(new_df),
    new_df[1],
    SIMPLIFY = TRUE)
    
    
    # 3.2 Construct dynamic filtering expression -------------------------------
    
    filter_expr <- lapply(names(values), function(col_name) {

      expr(!!sym(col_name) == !!values[[col_name]])
      
    })
    
    
    # Combine all filter expressions using an AND condition
    
    combined_filter <- Reduce(function(x) expr(!!x), filter_expr)
    
    
    # 3.3 Risk difference plot: heating ----------------------------------------
    
    plot_risk.diff_heating <-
      risk.diff_heating %>%
      filter(!!combined_filter) %>%
      ggplot(aes(x = time.onset.h, y = .data[[contrast_name_h]], ymin = .lower, ymax = .upper)) +
      
      geom_errorbar(aes(colour = as.factor(vote)),
                    width = 0,
                    linewidth = 1.5,
                    alpha = 0.4,
                    position = position_dodge(width = 0.75)) +
      
      geom_point(aes(colour = as.factor(vote)),
                 size = 1.5,
                 alpha = 1,
                 position = position_dodge(width = 0.75)) +     
      
      geom_hline(yintercept = 0, color = "grey50", linetype = "dotted", linewidth = 0.35) +
      
      scale_x_continuous("Time since onset (h)",
                         breaks = c(1.0, 2.0, 3.0),
                         labels = c("1.0","2.0","3.0")) +
      
      scale_y_continuous(expression("Posterior category-specific CATE"~("%")),
                         breaks = seq(-1, 1, by = 0.2),
                         labels = function(x) x * 100,
                         limits = c(-0.4, 0.4)) +
      
      scale_colour_manual("Thermal sensation vote",
                          values = palette_TSV, 
                          labels = vote_names, 
                          guide = guide_legend(title.position = "top",  
                                               title.hjust = 0.5, 
                                               nrow = 1, 
                                               byrow = TRUE)) +
      
      facet_grid(. ~ factor(time.day, levels = c("afternoon", "evening"), 
                            labels = c("Afternoon", "Evening"))) +
      
      theme(strip.background = element_rect(fill = NA, color = NA), 
            strip.text = element_text(size = 11, color = "black"),
            panel.grid.major.x = element_blank(), 
            panel.grid.major.y = element_line(colour = "grey95"),
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(colour = "black", fill = NA),
            legend.position = "bottom",    
            legend.direction = "horizontal",  
            legend.box.spacing = unit(0.2, "cm"),
            plot.caption = element_text(size = 9, hjust = 1, face = "italic", color = "gray30")) 
    
    
    # 3.4 Risk difference plot: cooling ----------------------------------------
    
    plot_risk.diff_cooling <-
      risk.diff_cooling %>%
      filter(!!combined_filter) %>%
      ggplot(aes(x = time.onset.h, y = .data[[contrast_name_c]], ymin = .lower, ymax = .upper)) +
      
      geom_errorbar(aes(colour = as.factor(vote)),
                    width = 0,
                    linewidth = 1.5,
                    alpha = 0.4,
                    position = position_dodge(width = 0.75)) +
      
      geom_point(aes(colour = as.factor(vote)),
                 size = 1.5,
                 alpha = 1,
                 position = position_dodge(width = 0.75)) +     
      
      geom_hline(yintercept = 0, color = "grey50", linetype = "dotted", linewidth = 0.35) +
      
      scale_x_continuous("Time since onset (h)",
                         breaks = c(1.0, 2.0, 3.0),
                         labels = c("1.0","2.0","3.0")) +
      
      scale_y_continuous(expression("Posterior category-specific CATE"~("%")),
                         breaks = seq(-1, 1, by = 0.2),
                         labels = function(x) x * 100,
                         limits = c(-0.4, 0.4)) +
      
      scale_colour_manual("Thermal sensation vote",
                          values = palette_TSV,
                          labels = vote_names, 
                          guide = guide_legend(title.position = "top",   
                                               title.hjust = 0.5,  
                                               nrow = 1, 
                                               byrow = TRUE)) +
      
      facet_grid(. ~ factor(time.day, levels = c("afternoon", "evening"), 
                            labels = c("Afternoon", "Evening"))) +
      
      theme(strip.background = element_rect(fill = NA, color = NA), 
            strip.text = element_text(size = 11, color = "black"),
            panel.grid.major.x = element_blank(), 
            panel.grid.major.y = element_line(colour = "grey95"),
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            panel.border = element_rect(colour = "black", fill = NA),
            legend.position = "bottom",      
            legend.direction = "horizontal",
            legend.box.spacing = unit(0.2, "cm"),
            plot.caption = element_text(size = 9, hjust = 1, face = "italic", color = "gray30")) 
    
    
    # 3.5 Effect modification plot by day: heating -----------------------------
    
    plot_eff.mod_day_heating <-
      eff.mod_day_heating %>%
      filter(!!combined_filter) %>%
      ggplot(aes(x = time.onset.h, y = EM.day_A.E, ymin = .lower, ymax = .upper)) +
      
      geom_errorbar(aes(colour = as.factor(vote)),
                    width = 0,
                    linewidth = 1.5,
                    alpha = 0.4,
                    position = position_dodge(width = 0.75)) +
      
      geom_point(aes(colour = as.factor(vote)),
                 size = 1.5,
                 alpha = 1,
                 position = position_dodge(width = 0.75)) +     
      
      geom_hline(yintercept = 0, color = "grey50", linetype = "dotted", linewidth = 0.35) +
      
      scale_x_continuous("Time since onset (h)",
                         breaks = c(1.0, 2.0, 3.0),
                         labels = c("1.0","2.0","3.0")) +
      
      scale_y_continuous("Posterior difference in CATE (%)", 
                         breaks = seq(-1, 1, by = 0.2),
                         labels = function(x) x * 100,
                         limits = c(-0.4, 0.4)) +
      
      scale_colour_manual("Thermal sensation vote",
                          values = palette_TSV,
                          labels = vote_names, 
                          guide = guide_legend(title.position = "top",   
                                               title.hjust = 0.5,  
                                               nrow = 1,
                                               byrow = TRUE)) +
      
      facet_grid(setpoint ~ diff) +
      
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
    
    
    # 3.6 Effect modification plot by day: cooling -----------------------------
    
    plot_eff.mod_day_cooling <-
      eff.mod_day_cooling %>%
      filter(!!combined_filter) %>%
      ggplot(aes(x = time.onset.h, y = EM.day_A.E, ymin = .lower, ymax = .upper)) +
      
      geom_errorbar(aes(colour = as.factor(vote)),
                    width = 0,
                    linewidth = 1.5,
                    alpha = 0.4,
                    position = position_dodge(width = 0.75)) +
      
      geom_point(aes(colour = as.factor(vote)),
                 size = 1.5,
                 alpha = 1,
                 position = position_dodge(width = 0.75)) +     
      
      geom_hline(yintercept = 0, color = "grey50", linetype = "dotted", linewidth = 0.35) +
      
      scale_x_continuous("Time since onset (h)",
                         breaks = c(1.0, 2.0, 3.0),
                         labels = c("1.0","2.0","3.0")) +
      
      scale_y_continuous("Posterior difference in CATE (%)", 
                         breaks = seq(-1, 1, by = 0.2),
                         labels = function(x) x * 100,
                         limits = c(-0.4, 0.4)) +
      
      scale_colour_manual("Thermal sensation vote",
                          values = palette_TSV, 
                          labels = vote_names, 
                          guide = guide_legend(title.position = "top",
                                               title.hjust = 0.5,  
                                               nrow = 1, 
                                               byrow = TRUE)) +
      
      facet_grid(setpoint ~ diff) +
      
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
    
    
    # 3.7 Define shared y-axis labels ------------------------------------------
    
    y_axis_plot_left <- 
      ggplot() +
      theme_void() +
      labs(y = "Posterior category-specific CATE (%)") + 
      theme(axis.title.y = element_text(size = 11, angle = 90),
            plot.margin = margin(0, 0, 0, 0))
    
    y_axis_plot_right <- 
      ggplot() +
      theme_void() +
      labs(y = "Posterior difference in CATE (%)") +
      theme(axis.title.y = element_text(size = 11, angle = 90),
            plot.margin = margin(0, 0, 0, 0))
    
    
    # 3.8 Add panel labels and remove duplicated axis titles -------------------
    
    plot_risk.diff_heating <- 
      plot_risk.diff_heating +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      labs(tag = "a)")
    
    plot_risk.diff_cooling <- 
      plot_risk.diff_cooling +
      theme(axis.title.y = element_blank()) +
      labs(tag = "c)")
    
    
    plot_eff.mod_day_heating <- 
      plot_eff.mod_day_heating +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      labs(tag = "b)")
    
    plot_eff.mod_day_cooling <- 
      plot_eff.mod_day_cooling +
      theme(axis.title.y = element_blank()) +
      labs(tag = "d)")
    
    
    # 3.9 Assemble left and right panels ---------------------------------------
    
    left_panel <-
      y_axis_plot_left +
      (plot_risk.diff_heating / plot_risk.diff_cooling) +
      plot_layout(widths = c(0.01, 1))
    
    
    y_axis_plot_right <- y_axis_plot_right +
      theme(plot.margin = margin(0, 0, 0, 6))
    
    
    right_panel <-
      y_axis_plot_right +
      (plot_eff.mod_day_heating / plot_eff.mod_day_cooling) +
      plot_layout(widths = c(0.01, 1))
    
    
    # 3.10 Combine panels ------------------------------------------------------
    
    combined_plot <-
      (left_panel | right_panel) +
      plot_layout(guides = "collect", 
                  widths = c(1, 0.55)) &
      theme(legend.position = "bottom",
            plot.tag.position = c(0.03, 1),
            plot.tag = element_text(size = 12,
                                    face = "bold"))
    
  
    # 3.11 Save figure ---------------------------------------------------------
    
    ggsave(combined_plot, 
           filename = file.path(output_figure_folder,
                                paste0("Risk-diff-and-eff-mod-by-day_fix-", values[[1]],  ".png")),
           width = 18,
           height = 15,
           units = "cm",
           dpi = 900)
    
  }
  
  
  # 3.12 Remove day-specific objects -------------------------------------------
  
  rm(plot_risk.diff_heating,
     plot_risk.diff_cooling,
     plot_eff.mod_day_heating,
     plot_eff.mod_day_cooling,
     combined_plot,
     risk.diff_heating, 
     risk.diff_cooling,
     eff.mod_day_heating,
     eff.mod_day_cooling)  
  
  }

# ==============================================================================
# 4. Completion
# ==============================================================================

message("\nRisk difference and effect modification visualisation completed")
message("----------------------------------------\n")
