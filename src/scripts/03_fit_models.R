# ==============================================================================
# Script: 03_fit_models.R
#
# Description: Fit Bayesian multinomial models for the Nevins thermal comfort
#              dataset using the formulas and priors defined in
#              02_model_setup.R.
#
# Author: Matteo Favero
# ==============================================================================

message("-------------------------------------------")
message("Model fitting\n")

# ==============================================================================
# 0. Setup
# ==============================================================================

# 0.1 Check project root --------------------------------------------------------

if (!dir.exists("src/data")) {
  stop("Project root not detected. Please open the .Rproj file.")
}


# 0.2 Load project configuration ------------------------------------------------

source("src/scripts/development_settings.R")


# 0.3 Define output folder ------------------------------------------------------

output_model_folder  <- fitted_model_folder

if (!dir.exists(output_model_folder )) {
  dir.create(output_model_folder ,
             recursive = TRUE)
}


# ==============================================================================
# 1. Select Models
# ==============================================================================

# 1.1 Define models to fit ------------------------------------------------------

models_to_fit <- models_to_run


# 1.2 Verify model names --------------------------------------------------------

if (!all(models_to_fit %in% names(model_formulas))) {
  
  stop("Unknown model(s): ",
       paste(setdiff(models_to_fit,
                     names(model_formulas)),
             collapse = ", "))
  
}

# ==============================================================================
# 2. Fit Bayesian Models
# ==============================================================================

# 2.1 Fit each selected model --------------------------------------------------

for (model_name in models_to_fit) {    
  
  
  message("Fitting model: ",
          model_name)
  
  
  # Fit Bayesian model ---------------------------------------------------------
  
  fitted_model <- 
    brm(formula = model_formulas[[model_name]],
        data = nevins_analysis,
        family = model_family,
        prior = model_priors[[model_name]],
        sample_prior = TRUE,
        save_pars = save_pars(all = TRUE),
        iter = model_control$iter,
        warmup = model_control$warmup,
        chains = model_control$chains,
        cores = model_control$cores,
        seed = model_control$seed,
        control = model_control$control,
        file = file.path(output_model_folder ,
                         model_name))
  
  # 2.1 Add LOO criterion ------------------------------------------------------
  
  # Stores LOO estimates and Pareto-k diagnostics inside the
  # brmsfit object.
  
  message("Calculating LOO for ",
          model_name, "\n")
  
  fitted_model <- 
    add_criterion(fitted_model,
                  criterion = "loo")

}


# ==============================================================================
# 3. Completion
# ==============================================================================


message("Model fitting completed")
message("All models fitted successfully.")
message("----------------------------------------\n")


# ==============================================================================
# 4. Unload Packages
# ==============================================================================

packages_to_unload <- 
  c("tidyverse",
    "brms")

for (package in packages_to_unload) {
  
  if (paste0("package:", package) %in% search()) {
    
    detach(paste0("package:", package),
           unload = FALSE,
           character.only = TRUE)
    
  }
  
}


# ==============================================================================
# 5. Clean Workspace
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