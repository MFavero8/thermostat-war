# ==============================================================================
# Script: config.R
#
# Description: Define project folders and analysis structure.
#
# Author: Matteo Favero 
# ==============================================================================


# ==============================================================================
# 1. Model analysis settings
# ==============================================================================

# Prior scenario
# Change this to "prior_weak" to run prior sensitivity analyses

prior <- "prior_select"
#prior <- "prior_weak"


# ==============================================================================
# 2. Setting folders 
# ==============================================================================

# 2.1 Data folders -------------------------------------------------------------

raw_data_folder <- "src/data/raw"
processed_data_folder <- "src/data/processed"


# 2.2 Main folders -------------------------------------------------------------

model_folder <- "src/models"
table_folder <- "src/tables"
figure_folder <- "src/figures"
prior_folder <- "src/scripts/priors"

# 2.3 Output folders -----------------------------------------------------------

fitted_model_folder <- 
  file.path(model_folder,
            "fitted",
            prior)

diagnostic_folder <- 
  file.path(model_folder,
            "diagnostics",
            prior)

diagnostic_traceplot_folder <- 
  file.path(model_folder,
            "diagnostics",
            "traceplots",
            prior)

comparison_folder <- 
  file.path(model_folder,
            "comparison",
            prior)

cross_validation_folder <- 
  file.path(model_folder,
            "cross_validation",
            prior)

prediction_folder <- 
  file.path(model_folder,
            "predictions",
            prior)

table_folder <- 
  file.path(table_folder,
            prior)

figure_folder <- 
  file.path(figure_folder,
            prior)

prior_folder <- 
  file.path(prior_folder,
            prior)


# 2.4 Create folders -----------------------------------------------------------

folders <- 
  c(fitted_model_folder,
    diagnostic_folder,
    comparison_folder,
    cross_validation_folder,
    prediction_folder,
    table_folder,
    figure_folder)

for (folder in folders) {
  
  if (!dir.exists(folder)) {
    
    dir.create(folder,
               recursive = TRUE)
    
  }
}

