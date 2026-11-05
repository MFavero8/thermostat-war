# ==============================================================================
# Script: 07a_posterior_predictive_check.R
#
# Description: Generate posterior predictive check from fitted Bayesian models
#
#
# Author: Matteo Favero
# ==============================================================================


message("-------------------------------------------")
message("Posterior predictive checks\n")

# ==============================================================================
# 0. Setup
# ==============================================================================

before_section <- 
  ls(all.names = TRUE)


# 0.1 Check project root -------------------------------------------------------

if (!dir.exists("src/data")) {
  
  stop("Project root not detected. Please open the .Rproj file.")
  
}


# 0.2 Load packages ------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidybayes)
})


# 0.3 Load configuration -------------------------------------------------------

source("src/scripts/config.R")
source("src/scripts/parameters.R")
source("src/scripts/development_settings.R")


# ==============================================================================
# 1. Load Processed Dataset
# ==============================================================================

# 1.1 Load Nevins analysis dataset ---------------------------------------------

nevins_data <- 
  readRDS(file.path(processed_data_folder,
                    "nevins_aggregated_data.rds"))


# ==============================================================================
# 2. Select Models
# ==============================================================================

# 2.1 Select models to predict -------------------------------------------------

models_to_predict <- models_to_run


# ==============================================================================
# 3. Prepare Data for Predicts and Plot
# ==============================================================================

# 3.1 Define vote categories and numeric positions -----------------------------

vote_levels <- paste0("y", 1:7)

vote_positions <- 
  tibble(
    vote = vote_levels,
    vote_numeric = -3:3
  )

# 3.2 Select conditions to predict ---------------------------------------------

selected_conditions <- 
  nevins_data %>%
  filter(condition_id %in% c("h15.t66",
                             "h15.t74", 
                             "h15.t82", 
                             "h55.t66",
                             "h55.t74", 
                             "h55.t82", 
                             "h85.t66",
                             "h85.t74", 
                             "h85.t82")) %>%
  pull(condition_id) %>%
  as.character() %>%
  unique()


# 3.3 Create dataset for predictions -------------------------------------------

# All data, pooled across sex

data_all <- 
  nevins_data 
    

# All data, separated by sex
  
data_all_by_sex <- 
  nevins_data 
    

# Selected condition, pooled across sex

data_selected <-
  nevins_data %>%
  filter(
    condition_id %in% selected_conditions
  )

# Selected condition, separated by sex

data_selected_by_sex <- 
  nevins_data %>%
  filter(
    condition_id %in% selected_conditions
  )

# ==============================================================================
# 4. Generate Predictions
# ==============================================================================

# 4.1 Loop over fitted models --------------------------------------------------

for (model_name in models_to_predict) {
  
  
  message("\nPredicting: ",
          model_name)
  
  
  # 4.1.1 Load fitted model ----------------------------------------------------
  
  fit <- 
    readRDS(file.path(
      fitted_model_folder,
      paste0(model_name,
             ".rds")))
  
  
  # 4.1.2 Define output folder -------------------------------------------------
  
  output_figure_folder <-
    file.path(figure_folder,
              model_name,
              "posterior_pred_checks")
  
  if (!dir.exists(output_figure_folder)) {
    
    dir.create(output_figure_folder,
               recursive = TRUE)
    
  }
  
  # 4.1.3 Generate predictions -------------------------------------------------
  
  pred_draws <- 
    fit %>%
    predicted_draws(
      newdata = data_all,
      ndraws = 100,
      seed = 2026,
      re_formula = NULL
    )
  
  
  time_onset_values <- c(0, 1, 2)
  
  for (onset_time in time_onset_values) {
  
  
  # 4.1.4 Aggregate posterior predictions --------------------------------------
  
  prediction_long <- 
    pred_draws %>%
    ungroup() %>%
    filter(c_time.onset.h == onset_time) %>%
    mutate(category = as.character(.category)) %>%
    left_join(
      vote_positions %>%
        transmute(
          category = vote,
          vote_numeric
        ),
      by = "category"
    )
  
  # Helper to aggregate posterior draws and calculate 95% intervals
  summarise_predictions <- function(predictions, group_vars = character()) {
    
    draw_groups <- c(group_vars, ".draw", "vote_numeric")
    
    prediction_by_draw <- predictions %>%
      group_by(across(all_of(draw_groups))) %>%
      summarise(
        predicted_count = sum(.prediction, na.rm = TRUE),
        .groups = "drop"
      )
    
    summary_groups <- c(group_vars, "vote_numeric")
    
    prediction_by_draw %>%
      group_by(across(all_of(summary_groups))) %>%
      mean_qi(predicted_count, .width = 0.95) %>%
      ungroup()
  }
  
  
  # 4.1.5 Prepare observed counts for plotting ---------------------------------
  
  observed_long <- function(data, group_vars = character()) {
    
    vote_matrix <- as.data.frame(
      matrix(
        unlist(data$vote),
        ncol = 7,
        byrow = FALSE
      )
    )
    
    names(vote_matrix) <- vote_levels
    
    bind_cols(
      data %>% select(all_of(group_vars)),
      vote_matrix
    ) %>%
      pivot_longer(
        cols = all_of(vote_levels),
        names_to = "vote",
        values_to = "count"
      ) %>%
      left_join(vote_positions, by = "vote") %>%
      group_by(across(all_of(c(group_vars, "vote_numeric")))) %>%
      summarise(count = sum(count), .groups = "drop")
  }
  
  # 4.1.6 Prepare data for all four plots --------------------------------------
  
  # All data, pooled across sex
  pred_all <- 
    summarise_predictions(
      prediction_long %>% filter(c_time.onset.h == onset_time)
  )
  
  obs_all <- 
    observed_long(data_all %>% filter(c_time.onset.h == onset_time)) %>%
    group_by(vote_numeric) %>%
    summarise(count = sum(count), 
              .groups = "drop"
              )
  
  # All data, separated by sex
  pred_all_sex <- 
    summarise_predictions(
      prediction_long %>% 
        filter(c_time.onset.h == onset_time),
      group_vars = "sex"
  )
  
  obs_all_sex <- 
    observed_long(data_all %>% filter(c_time.onset.h == onset_time), "sex") %>%
    group_by(sex, vote_numeric) %>%
    summarise(count = sum(count), 
              .groups = "drop"
              )
  
  # Selected conditions, pooled across sex
  
  pred_selected <- 
    summarise_predictions(
      prediction_long %>%
        filter(condition_id %in% selected_conditions,
               c_time.onset.h == onset_time),
      group_vars = "condition_id"
    )
  
  obs_selected <- observed_long(data_selected %>% filter(c_time.onset.h == onset_time), "condition_id")

  
  # Selected conditions, separated by sex
  
  pred_selected_sex <- 
    summarise_predictions(
      prediction_long %>%
        filter(condition_id %in% selected_conditions,
               c_time.onset.h == onset_time),
      group_vars = c("condition_id", "sex")
  )
  
  obs_selected_sex <- 
    observed_long(
      data_selected %>% filter(c_time.onset.h == onset_time),
      c("condition_id", "sex")
  )
  
  
  # 4.1.4 Plot posterior predictive check --------------------------------------
  
  label_sex <- c("male" = "Male", "female" = "Female")
  
  label_condition <-
    setNames(
      paste0(
        sprintf(
          "%.1f",
          (as.numeric(stringr::str_extract(
            selected_conditions, "(?<=\\.t)\\d+"
          )) - 32) * 5/9
        ),
        "°C, ",
        stringr::str_extract(
          selected_conditions, "(?<=h)\\d+"
        ),
        "%rh"
      ),
      selected_conditions
    )
  
  # Plot 1: All data, pooled across sex
  
  pred_step <- 
    pred_all %>%
    arrange(across(any_of(c("condition_id", "sex"))), vote_numeric) %>%
    group_by(across(any_of(c("condition_id", "sex")))) %>%
    reframe(
      x = as.vector(rbind(vote_numeric - 0.5, vote_numeric + 0.5)),
      estimate = rep(predicted_count, each = 2),
      lower = rep(.lower, each = 2),
      upper = rep(.upper, each = 2)
      )
  
  obs_step <-
    obs_all %>%
    arrange(across(any_of(c("condition_id", "sex"))), vote_numeric) %>%
    group_by(across(any_of(c("condition_id", "sex")))) %>%
    reframe(
      x = as.vector(rbind(vote_numeric - 0.5, vote_numeric + 0.5)),
      count_step = rep(count, each = 2)
      )
  
  p1 <-
    ggplot(pred_step, aes(x = x, y = estimate)) +
    
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = "Predicted data"), alpha = 0.3)+
    
    geom_line(aes(colour = "Predicted data", linetype = "Predicted data"), linewidth = 0.6) +
    
    geom_step(data = obs_step, aes(x = x, y = count_step, 
                                   colour = "black", fill = "black",
                                   linetype = "black"), linewidth = 0.6) +  
    scale_colour_manual("",
                        values = c("#5AB4AC", "black"), 
                        breaks = c("Predicted data", "black"),
                        labels = c("Prediction", "Observed data")) +
    
    scale_fill_manual("",
                      values = c("#5AB4AC", "black"), 
                      breaks = c("Predicted data", "black"),
                      labels = c("Prediction", "Observed data")) +
    
    scale_linetype_manual("",
                          values = c("solid", "dashed"), 
                          breaks = c("Predicted data", "black"),
                          labels = c("Prediction", "Observed data")) +
    
    scale_x_continuous(expression("Thermal sensation vote"),
                       breaks = seq(-3, 3, by = 1),
                       limits = c(-3.5, 3.5)) +
    
    scale_y_continuous(expression("Count"),
                       breaks = seq(0, 720, by = 50),
                       limits = c(0, 300)) +
    
    theme(strip.background = element_rect(fill = NA, colour = NA),
          strip.text = element_text(size = 11, colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box.spacing = unit(0.2, "cm"),
          legend.key = element_blank(),
          plot.caption = element_text(size = 9, hjust = 1, face = "italic", colour = "gray30"))
  
  
  # Plot 2: All data, separated by sex
  
  pred_step <- 
    pred_all_sex %>%
    arrange(across(any_of(c("condition_id", "sex"))), vote_numeric) %>%
    group_by(across(any_of(c("condition_id", "sex")))) %>%
    reframe(
      x = as.vector(rbind(vote_numeric - 0.5, vote_numeric + 0.5)),
      estimate = rep(predicted_count, each = 2),
      lower = rep(.lower, each = 2),
      upper = rep(.upper, each = 2)
      )
  
  obs_step <-
    obs_all_sex %>%
    arrange(across(any_of(c("condition_id", "sex"))), vote_numeric) %>%
    group_by(across(any_of(c("condition_id", "sex")))) %>%
    reframe(
      x = as.vector(rbind(vote_numeric - 0.5, vote_numeric + 0.5)),
      count_step = rep(count, each = 2)
    )
  
  
  p2 <-
    ggplot(pred_step, aes(x = x, y = estimate)) +
    
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = as.factor(sex)), alpha = 0.3)+
    
    geom_line(aes(colour = as.factor(sex), linetype = as.factor(sex)), linewidth = 0.6) +
    
    geom_step(data = obs_step, aes(x = x, y = count_step, 
                                   colour = "black", fill = "black",
                                   linetype = "black"), linewidth = 0.6) + 
    
    scale_colour_manual("",
                        values = c("#FDB863", "#B2ABD2", "black"), 
                        breaks = c("male", "female", "black"),
                        labels = c("Male", "Female", "Observed data")) +
    
    scale_fill_manual("",
                      values = c("#FDB863", "#B2ABD2", "black"), 
                      breaks = c("male", "female", "black"),
                      labels = c("Male", "Female", "Observed data")) +
    
    scale_linetype_manual("",
                          values = c("solid", "solid", "dashed"), 
                          breaks = c("male", "female", "black"),
                          labels = c("Male", "Female", "Observed data")) +
    
    scale_x_continuous(expression("Thermal sensation vote"),
                       breaks = seq(-3, 3, by = 1),
                       limits = c(-3.5, 3.5)) +
    
    scale_y_continuous(expression("Count"),
                       breaks = seq(0, 720, by = 50),
                       limits = c(0, 200)) +
    
    facet_wrap(. ~ sex, 
               nrow = 2,
               labeller = labeller(sex = label_sex)) +
    
    theme(strip.background = element_rect(fill = NA, colour = NA),
          strip.text = element_text(size = 11, colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box.spacing = unit(0.2, "cm"),
          legend.key = element_blank(),
          plot.caption = element_text(size = 9, hjust = 1, face = "italic", colour = "gray30"))
  
  
  # Plot 3: Selected conditions, pooled across sex
  
  pred_step <- 
    pred_selected %>%
    arrange(across(any_of(c("condition_id", "sex"))), vote_numeric) %>%
    group_by(across(any_of(c("condition_id", "sex")))) %>%
    reframe(
      x = as.vector(rbind(vote_numeric - 0.5, vote_numeric + 0.5)),
      estimate = rep(predicted_count, each = 2),
      lower = rep(.lower, each = 2),
      upper = rep(.upper, each = 2)
    )
  
  obs_step <-
    obs_selected %>%
    arrange(across(any_of(c("condition_id", "sex"))), vote_numeric) %>%
    group_by(across(any_of(c("condition_id", "sex")))) %>%
    reframe(
      x = as.vector(rbind(vote_numeric - 0.5, vote_numeric + 0.5)),
      count_step = rep(count, each = 2)
    )
    
  
  p3 <-
    ggplot(pred_step, aes(x = x, y = estimate)) +
    
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = "Predicted data"), alpha = 0.3)+
    
    geom_line(aes(colour = "Predicted data", linetype = "Predicted data"), linewidth = 0.6) +
    
    geom_step(data = obs_step, aes(x = x, y = count_step, 
                                   colour = "black", fill = "black",
                                   linetype = "black"), linewidth = 0.6) + 
    
    scale_colour_manual("",
                        values = c("#5AB4AC", "black"), 
                        breaks = c("Predicted data", "black"),
                        labels = c("Prediction", "Observed data")) +
    
    scale_fill_manual("",
                      values = c("#5AB4AC", "black"), 
                      breaks = c("Predicted data", "black"),
                      labels = c("Prediction", "Observed data")) +
    
    scale_linetype_manual("",
                          values = c("solid", "dashed"), 
                          breaks = c("Predicted data", "black"),
                          labels = c("Prediction", "Observed data")) +
    
    scale_x_continuous(expression("Thermal sensation vote"),
                       breaks = seq(-3, 3, by = 1),
                       limits = c(-3.5, 3.5)) +
    
    scale_y_continuous(expression("Count"),
                       breaks = seq(0, 10, by = 2),
                       limits = c(0, 10)) +
    
    facet_wrap(condition_id ~ .,
               labeller = labeller(condition_id = label_condition)) +
    
    theme(strip.background = element_rect(fill = NA, colour = NA),
          strip.text = element_text(size = 11, colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box.spacing = unit(0.2, "cm"),
          legend.key = element_blank(),
          plot.caption = element_text(size = 9, hjust = 1, face = "italic", colour = "gray30"))
  
  
  # Plot 4: Selected conditions, separated by sex
  
  pred_step <- 
    pred_selected_sex %>%
    arrange(across(any_of(c("condition_id", "sex"))), vote_numeric) %>%
    group_by(across(any_of(c("condition_id", "sex")))) %>%
    reframe(
      x = as.vector(rbind(vote_numeric - 0.5, vote_numeric + 0.5)),
      estimate = rep(predicted_count, each = 2),
      lower = rep(.lower, each = 2),
      upper = rep(.upper, each = 2)
    )
  
  obs_step <-
    obs_selected_sex %>%
    arrange(across(any_of(c("condition_id", "sex"))), vote_numeric) %>%
    group_by(across(any_of(c("condition_id", "sex")))) %>%
    reframe(
      x = as.vector(rbind(vote_numeric - 0.5, vote_numeric + 0.5)),
      count_step = rep(count, each = 2)
    )
  
  condition_levels <- unique(as.character(pred_step$condition_id))
  
  facet_levels <- c(
    paste(condition_levels, "male", sep = "_"),
    paste(condition_levels, "female", sep = "_")
  )
  
  pred_step <- pred_step %>%
    mutate(
      condition_sex = factor(
        paste(condition_id, tolower(as.character(sex)), sep = "_"),
        levels = facet_levels
      )
    )
  
  obs_step <- obs_step %>%
    mutate(
      condition_sex = factor(
        paste(condition_id, tolower(as.character(sex)), sep = "_"),
        levels = facet_levels
      )
    )
  
  p4 <-
    ggplot(pred_step, aes(x = x, y = estimate)) +
    
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = as.factor(sex)), alpha = 0.3)+
    
    geom_line(aes(colour = as.factor(sex), linetype = as.factor(sex)), linewidth = 0.6) +
    
    geom_step(data = obs_step, aes(x = x, y = count_step, 
                                   colour = "black", fill = "black",
                                   linetype = "black"), linewidth = 0.6) + 
    
    scale_colour_manual("",
                        values = c("#FDB863", "#B2ABD2", "black"), 
                        breaks = c("male", "female", "black"),
                        labels = c("Male", "Female", "Observed data")) +
    
    scale_fill_manual("",
                      values = c("#FDB863", "#B2ABD2", "black"), 
                      breaks = c("male", "female", "black"),
                      labels = c("Male", "Female", "Observed data")) +
    
    scale_linetype_manual("",
                          values = c("solid", "solid", "dashed"), 
                          breaks = c("male", "female", "black"),
                          labels = c("Male", "Female", "Observed data")) +
    
    scale_x_continuous(expression("Thermal sensation vote"),
                       breaks = seq(-3, 3, by = 1),
                       limits = c(-3.5, 3.5)) +
    
    scale_y_continuous(expression("Count"),
                       breaks = seq(0, 5, by = 1),
                       limits = c(0, 5)) +
    
    facet_wrap(. ~ condition_sex,
               ncol = 3,
               labeller = labeller(
                 condition_sex = function(x) {
                   ids <- sub("_(male|female)$", "", as.character(x))
                   unname(label_condition[ids])
        }
      )
    ) +
    
    theme(strip.background = element_rect(fill = NA, colour = NA),
          strip.text = element_text(size = 11, colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box.spacing = unit(0.2, "cm"),
          legend.key = element_blank(),
          plot.caption = element_text(size = 9, hjust = 1, face = "italic", colour = "gray30"))
  
  
  
  # 4.1.5 Save plots -----------------------------------------------------------
      
  ggsave(p1, 
         filename = file.path(output_figure_folder,
                              paste0("posterior-check_all-data_onset-", onset_time+1, "h", ".png")),
         width = 10,
         height = 10,
         units = "cm",
         dpi = 900)
  
  ggsave(p2, 
         filename = file.path(output_figure_folder,
                              paste0("posterior-check_all-data-by-sex_onset-", onset_time+1, "h", ".png")),
         width = 10,
         height = 16,
         units = "cm",
         dpi = 900)
  
  ggsave(p3, 
         filename = file.path(output_figure_folder,
                              paste0("posterior-check_selected-conditions_onset-", onset_time+1, "h", ".png")),
         width = 15,
         height = 15,
         units = "cm",
         dpi = 900)
  
  ggsave(p4, 
         filename = file.path(output_figure_folder,
                              paste0("posterior-check_selected-conditions-by-sex_onset-", onset_time+1, "h", ".png")),
         width = 15,
         height = 30,
         units = "cm",
         dpi = 900)

#    # Release memory -----------------------------------------------------------
#   
  }
     rm(pred_draws, 
        prediction_long, 
        pred_step,
        obs_step,
        p1,
        p2,
        p3,
        p4
        )
#    gc()
#    
  
  # Release fitted model from memory -------------------------------------------
  
  rm(fit)
  gc()
  
}



# ==============================================================================
# 6. Completion
# ==============================================================================

message("\nPosterior predictive checks completed successfully.")
message("----------------------------------------\n")


# ==============================================================================
# 11. Unload Packages
# ==============================================================================

packages_to_unload <- 
  c("tidyverse",
    "tidybayes")

for (package in packages_to_unload) {
  
  if (paste0("package:", package) %in% search()) {
    
    detach(paste0("package:", package),
           unload = FALSE,
           character.only = TRUE)
    
  }
  
}


# ==============================================================================
# 7. Clean Workspace
# ==============================================================================

after_section <- 
  ls(all.names = TRUE)

new_objects <- 
  setdiff(after_section,
          c(before_section,
            "before_section"))

rm(list = new_objects)

rm(before_section,
   after_section,
   new_objects)

gc()

