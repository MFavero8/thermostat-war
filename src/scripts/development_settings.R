# ==============================================================================
# Script: development_settings.R
#
#
# Description: Control model selection and workflow execution.
#              
#              final_model_only:
#              Controls the number of models analysed.
#
#              run_* switches:
#              Control which analysis steps are executed.
#
# Author: Matteo Favero
# ==============================================================================


# ==============================================================================
# 1. Final model mode
# ==============================================================================

# TRUE:
#   Run only selected development models.
#
# FALSE:
#   Run complete model set.


final_model_only <- TRUE
#final_model_only <- FALSE


# 1.1 Model selection ----------------------------------------------------------

if (final_model_only) {
  
  models_to_run <- 
    c("mod_01")
  
} else {
  
  models_to_run <- 
    paste0("mod_",
           sprintf("%02d", 0:7))
  
}


# ============================================================
# Workflow execution control for script run_all
#
# TRUE  = execute step
# FALSE = skip step
#
# ============================================================

# ------------------------------------------------------------
# Data preparation
# ------------------------------------------------------------

run_data_preparation <- FALSE


# ------------------------------------------------------------
# Model definition, prior setup and Bayesian model fitting
# ------------------------------------------------------------

run_model_setup_and_model_fitting <- FALSE


# ------------------------------------------------------------
# Model diagnostics
# ------------------------------------------------------------

run_diagnostics <- FALSE


# ------------------------------------------------------------
# Predictive model comparison using LOO
# ------------------------------------------------------------

run_loo_comparison <- FALSE


# ------------------------------------------------------------
# K-fold cross validation
# ------------------------------------------------------------

run_kfold <- FALSE


# ------------------------------------------------------------
# Posterior predictive checks
# ------------------------------------------------------------

run_posterior_check <- TRUE


# ------------------------------------------------------------
# Posterior predictions
# ------------------------------------------------------------

run_predictions <- TRUE


# ------------------------------------------------------------
# Average thermal sensation
# ------------------------------------------------------------

run_average_thermal_sensation <- TRUE


# ------------------------------------------------------------
# Risk prediction, risk difference and effect_modification
# ------------------------------------------------------------

run_risk_prediction_risk_difference_effect_modification <- TRUE


# ------------------------------------------------------------
# Utility-weighted outcome index
# ------------------------------------------------------------

run_utility_weighted_outcome_index <- TRUE


