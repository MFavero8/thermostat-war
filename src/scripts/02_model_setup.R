# ==============================================================================
# Script: 02_model_setup.R
#
# Description: Definition of Bayesian model formulas, priors, and fitting
#              settings for the Nevins thermal comfort analysis
#
#              This script does not fit models.
#              It creates the objects required by 03_fit_models.R.
#
# Author: Matteo Favero
# ==============================================================================

message("-------------------------------------------")
message("Model setup\n")

# ==============================================================================
# 0. Setup
# ==============================================================================

before_section <- 
  ls(all.names = TRUE)


# 0.1 Check project root --------------------------------------------------------

if (!dir.exists("src/data")) {
  stop("Project root not detected. Please open the .Rproj file.")
}


# 0.2 Load packages -------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(brms)
})


# 0.3 Load project configuration ------------------------------------------------

source("src/scripts/config.R")
source("src/scripts/parameters.R")

# 0.4 Report prior specification ------------------------------------------------

message("Using prior specification: ",
        prior)

# ==============================================================================
# 1. Load Processed Dataset
# ==============================================================================

# 1.1 Load Nevins analysis dataset ----------------------------------------------

nevins_analysis <- 
  readRDS(file.path(processed_data_folder,
                    "nevins_aggregated_data.rds"))

message("\nLoading Nevins analysis dataset: ", 
        nrow(nevins_analysis),
        " aggregated observations loaded")


# ==============================================================================
# 2. Define Model Formulas
# ==============================================================================

# 2.1 Model specification -------------------------------------------------------

# All models use:
#
# - multinomial thermal sensation vote
# - y4 (comfortable) as reference category
# - varying effects by experimental condition
#
# Models differ in the inclusion of:
#
# - sex × air temperature interaction
# - sex × relative humidity interaction
# - air temperature × relative humidity interaction


# 2.2 Baseline model ------------------------------------------------------------

formula_mod_00 <- 
  bf(vote | trials(count) ~ 0 + sex + time.day + mo(c_time.onset.h) + c_air.temp + 
       c_rel.hum  + (0 + sex + mo(c_time.onset.h) | i | condition_id)) 


# 2.3 Air temperature × relative humidity interaction --------------------------

formula_mod_01 <- 
  bf(vote | trials(count) ~ 0 + sex + time.day + mo(c_time.onset.h) + c_air.temp + 
       c_rel.hum + c_air.temp:c_rel.hum + (0 + sex + mo(c_time.onset.h) | i | condition_id)) 


# 2.4 Sex × air temperature interaction -----------------------------------------

formula_mod_02 <- 
  bf(vote | trials(count) ~ 0 + sex + time.day + mo(c_time.onset.h) + sex:c_air.temp + 
       c_rel.hum + (0 + sex + mo(c_time.onset.h) | i | condition_id))  


# 2.5 Sex × relative humidity interaction ---------------------------------------

formula_mod_03 <- 
  bf(vote | trials(count) ~ 0 + sex + time.day + mo(c_time.onset.h) + c_air.temp + 
       sex:c_rel.hum + (0 + sex + mo(c_time.onset.h) | i | condition_id)) 


# 2.6 Sex × air temperature and air temperature × relative humidity interactions

formula_mod_04 <- 
  bf(vote | trials(count) ~ 0 + sex + time.day + mo(c_time.onset.h) + sex:c_air.temp + 
       c_rel.hum + c_air.temp:c_rel.hum + (0 + sex + mo(c_time.onset.h) | i | condition_id)) 


# 2.7 Sex × relative humidity and air temperature × relative humidity interactions

formula_mod_05 <- 
  bf(vote | trials(count) ~ 0 + sex + time.day + mo(c_time.onset.h) + c_air.temp +
       sex:c_rel.hum + c_air.temp:c_rel.hum + (0 + sex + mo(c_time.onset.h) | i | condition_id)) 


# 2.8 Sex × air temperature and sex × relative humidity interactions -----------

formula_mod_06 <- 
  bf(vote | trials(count) ~ 0 + sex + time.day + mo(c_time.onset.h) + sex:c_air.temp + 
       sex:c_rel.hum + (0 + sex + mo(c_time.onset.h) | i | condition_id)) 


# 2.9 Full interaction model ----------------------------------------------------

formula_mod_07 <- 
  bf(vote | trials(count) ~ 0 + sex + time.day + mo(c_time.onset.h) + sex:c_air.temp + 
       sex:c_rel.hum + c_air.temp:c_rel.hum + (0 + sex + mo(c_time.onset.h) | i | condition_id)) 


# 2.10 Store model formulas -----------------------------------------------------

model_formulas <- 
  list(mod_00 = formula_mod_00,
       mod_01 = formula_mod_01,
       mod_02 = formula_mod_02,
       mod_03 = formula_mod_03,
       mod_04 = formula_mod_04,
       mod_05 = formula_mod_05,
       mod_06 = formula_mod_06,
       mod_07 = formula_mod_07)


# ==============================================================================
# 3. Load Model-Specific Priors
# ==============================================================================

# 3.1 Load priors for each model ------------------------------------------------

source(file.path(prior_folder,
                 "prior_mod_00.R"))

source(file.path(prior_folder,
                 "prior_mod_01.R"))

source(file.path(prior_folder,
                 "prior_mod_02.R"))

source(file.path(prior_folder,
                 "prior_mod_03.R"))

source(file.path(prior_folder,
                 "prior_mod_04.R"))

source(file.path(prior_folder,
                 "prior_mod_05.R"))

source(file.path(prior_folder,
                 "prior_mod_06.R"))

source(file.path(prior_folder,
                 "prior_mod_07.R"))


# 3.2 Store model-specific priors ------------------------------------------------

model_priors <- 
  list(mod_00 = prior_mod_00,
       mod_01 = prior_mod_01,
       mod_02 = prior_mod_02,
       mod_03 = prior_mod_03,
       mod_04 = prior_mod_04,
       mod_05 = prior_mod_05,
       mod_06 = prior_mod_06,
       mod_07 = prior_mod_07)


# ==============================================================================
# 4. Model Settings
# ==============================================================================

# 4.1 Define model family -------------------------------------------------------

model_family <- 
  multinomial(refcat = "y4",
              link = "logit")


# 4.2 Define model fitting settings ---------------------------------------------

model_control <-
  list(iter = 4000,
       warmup = 1000,
       chains = 4,
       cores = 4,
       seed = 2026,
       control = list(adapt_delta = 0.95))

# ==============================================================================
# 5. Completion
# ==============================================================================

message("\nModel setup completed")
message("-------------------------------------------\n")
