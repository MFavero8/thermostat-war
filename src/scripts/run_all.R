# ==============================================================================
# Script: run_all.R
#
# Purpose:
#   Execute the complete Bayesian thermal comfort workflow.
#
#   During development, the number of fitted models is controlled
#   through scripts/development_settings.R.
#
# ==============================================================================


# ============================================================
# Load project settings
# ============================================================

source("src/scripts/config.R")
source("src/scripts/parameters.R")
source("src/scripts/development_settings.R")


# ------------------------------------------------------------
# 1. Data preparation
#
# Reads the raw Nevins dataset, creates derived variables,
# aggregates thermal sensation votes, and saves the dataset
# used for Bayesian modelling.
# ------------------------------------------------------------

if (run_data_preparation) {
  
  message("=============================")
  message("Running 01_data_preparation.R")
  message("=============================")
  
  source("src/scripts/01_data_preparation.R")
  
}


if (run_model_setup_and_model_fitting) {
  
  
  # ------------------------------------------------------------
  # 2. Model setup
  #
  # Defines the Bayesian model formulas, priors, likelihood,
  # and computational settings used for model fitting.
  # ------------------------------------------------------------
  
  message("========================")
  message("Running 02_model_setup.R")
  message("========================")
  
  source("src/scripts/02_model_setup.R")
  
  
  # ------------------------------------------------------------
  # 3. Model fitting
  #
  # Fits the selected Bayesian multinomial models using brms.
  #
  # The selected models are controlled by:
  # scripts/development_settings.R
  # ------------------------------------------------------------
  
  message("=======================")
  message("Running 03_fit_models.R")
  message("=======================")
  
  source("src/scripts/03_fit_models.R")
  
}


# ------------------------------------------------------------
# 4. Model diagnostics
#
# Evaluates convergence, effective sample sizes, R-hat values,
# divergent transitions, and Hamiltonian Monte Carlo diagnostics.
# ------------------------------------------------------------

if (run_diagnostics) {
  
  message("==============================")
  message("Running 04_model_diagnostics.R")
  message("==============================")
  
  source("src/scripts/04_model_diagnostics.R")
  
}


# ------------------------------------------------------------
# 5. Model comparison
#
# Performs LOO cross-validation, checks Pareto-k diagnostics,
# and compares predictive performance between models.
#
# Requires at least two fitted models.
# Run only after fitting the complete model set.
# ------------------------------------------------------------

if (run_loo_comparison) {
  
  message("=============================")
  message("Running 05_model_comparison.R")
  message("=============================")
  
  source("src/scripts/05_model_comparison.R")
  
}


# ------------------------------------------------------------
# 6. K-fold cross-validation
#
# Performs K-fold cross-validation as a sensitivity analysis
# of model predictive performance and comparison with LOO rankings
#
# Recommended: 
# - run only after model diagnostics are OK (computationally expensive).
# - run with final_model_only = FALSE.
#
# See development_settings.R
# ------------------------------------------------------------

if (run_kfold) {
  
  message("===================================")
  message("Running 06_kfold_cross_validation.R")
  message("===================================")
  
  source("src/scripts/06_kfold_cross_validation.R")
  
}


# ------------------------------------------------------------
# 7. Posterior predictions
#
# Generate posterior predictive check from fitted Bayesian models
# 
# ------------------------------------------------------------

if (run_posterior_check) {
  
  message("=======================================")
  message("Running 07_posterior_predictive_check.R")
  message("=======================================")
  
  source("src/scripts/07_posterior_predictive_check.R")

  
}

# ------------------------------------------------------------
# 8. Posterior predictions
# 
# Generates posterior expected predictions from fitted models.
# Predictions are saved in chunks to avoid memory limitations.
# Existing chunks are detected and skipped, allowing the script
# to resume after interruption.
# ------------------------------------------------------------

if (run_predictions) {
  
  message("========================")
  message("Running 08_predictions.R")
  message("========================")
  
  source("src/scripts/08_predictions.R")
  
}


# ------------------------------------------------------------
# 9. Average thermal sensation
#
# Converts posterior expected probabilities into average
# thermal sensation values.
# ------------------------------------------------------------

if (run_average_thermal_sensation) {
  
  message("======================================")
  message("Running 09_average_thermal_sensation.R")
  message("======================================")
  
  source("src/scripts/09/09_average_thermal_sensation.R")
  
}


# ------------------------------------------------------------
# 10. Risk prediction, risk difference and effect modification
#
# Calculates risk prediction, risk difference and effect modification   
# for the heating and cooling intervention of interest
# ------------------------------------------------------------

if (run_risk_prediction_risk_difference_effect_modification) {
  
  message("================================================================")
  message("Running 10_risk_prediction_risk_difference_effect_modification.R")
  message("================================================================")
  
  source("src/scripts/10/10_risk_prediction_risk_difference_effect_modification.R")
  
}


# ------------------------------------------------------------
# 11.Utility-Weighted Outcome Index
#
# Calculates the Utility-Weighted Thermal Discomfort Index (UWTDI)
# ------------------------------------------------------------

if (run_utility_weighted_outcome_index) {
  
  message("===========================================")
  message("Running 11_Utility-weighted_outcome_index.R")
  message("===========================================\n")
  
  source("src/scripts/11/11_utility_weighted_outcome_index.R")
  
}
