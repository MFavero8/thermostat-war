# ==============================================================================
# Script: 05_model_comparison.R
#
# Description: Compare fitted Bayesian models using stored LOO estimates,
#              evaluate Pareto-k diagnostics, and rank models based on
#              predictive performance
#
# Author: Matteo Favero
# ==============================================================================

message("-------------------------------------------")
message("Model comparison\n")

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
  library(loo)
})


# 0.3 Load project configuration ------------------------------------------------

source("src/scripts/config.R")
source("src/scripts/development_settings.R")


# 0.4 Define folders ------------------------------------------------------------

input_model_folder <- fitted_model_folder

#comparison_folder <- comparison_folder

if (!dir.exists(comparison_folder)) {
  
  dir.create(comparison_folder,
             recursive = TRUE)
  
}


# ==============================================================================
# 1. Select Models
# ==============================================================================

# 1.1 Select models to compare --------------------------------------------------

models_to_compare <- models_to_run


# 1.2 Verify fitted model files -------------------------------------------------

model_files <- 
  file.path(input_model_folder,
            paste0(models_to_compare,
                   ".rds"))

if (!all(file.exists(model_files))) {
  
  stop("Missing fitted model files:\n",
       paste(model_files[!file.exists(model_files)],
             collapse = "\n"))
  
}

# 1.3 Verify sufficient number of models ---------------------------------------

if (length(models_to_compare) < 2) {
  
  on.exit({
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
    
    message("Model comparison not executed.")
    message("----------------------------------------\n")
  })
  
  stop("LOO comparison requires at least two fitted models. ",
       "Currently selected: ",
       paste(models_to_compare,
             collapse = ", "),
       call. = FALSE)
  
}


# ==============================================================================
# 2. Load Fitted Models
# ==============================================================================

# 2.1 Load models ----------------------------------------------------------------

message("Loading fitted models")

models <- lapply(model_files,
                 readRDS)

names(models) <- models_to_compare


# ==============================================================================
# 3. Extract Stored LOO Objects
# ==============================================================================

# 3.1 Extract LOO estimates -----------------------------------------------------

message("Extracting stored LOO estimates")

loo_results <- lapply(
  seq_along(models),
  function(i) {
    
    # Check that LOO estimates are available
    
    if (is.null(models[[i]]$criteria$loo)) {
      
      stop("LOO criterion not found in one or more models.")
      
    }
    
    
    # Extract stored LOO object
    
    loo_object <- models[[i]]$criteria$loo
    
    
    # Assign model name for identification
    
    attr(loo_object, "model_name") <- models_to_compare[i]
    
    loo_object
    
  }
)

names(loo_results) <- models_to_compare


# ==============================================================================
# 4. Pareto-k Diagnostics
# ==============================================================================

# 4.1 Extract Pareto-k diagnostics ---------------------------------------------

message("Checking Pareto-k diagnostics")

pareto_summary <- lapply(names(loo_results),
  function(model_name) {
    
    pareto_k <- loo_results[[model_name]]$diagnostics$pareto_k
    
    
    # Summarise observations with elevated Pareto-k values
    
    tibble(model = model_name,
           max_pareto_k = max(pareto_k),
           k_gt_0.7 = sum(pareto_k > 0.7),
           k_gt_1 = sum(pareto_k > 1))
  }
) %>%
  bind_rows()


# 4.2 Print Pareto-k summary ----------------------------------------------------

print(pareto_summary)


# 4.3 Save Pareto-k summary -----------------------------------------------------

write.csv(pareto_summary,
          file.path(comparison_folder,
                    "pareto_k_summary.csv"),
          row.names = FALSE)


# ==============================================================================
# 5. LOO Model Comparison
# ==============================================================================

# 5.1 Compare models using LOO --------------------------------------------------

message("Comparing models")

loo_comparison <- 
  do.call(loo_compare,
          c(list(x = loo_results[[1]]),
            loo_results[-1]))


# 5.2 Print LOO comparison ------------------------------------------------------

print(loo_comparison)


# 5.3 Convert comparison to data frame -----------------------------------------

loo_comparison_df <- as.data.frame(loo_comparison)


# 5.4 Add model names -----------------------------------------------------------

# loo_compare returns models in ranked order.
# Recover the corresponding model names from the LOO estimates.

loo_comparison_df$model <-
  names(loo_results)[order(sapply(loo_results,
      function(x) x$estimates["elpd_loo", "Estimate"],
      simplify = TRUE),
      decreasing = TRUE)]


# 5.5 Reorder output columns ----------------------------------------------------

loo_comparison_df <-
  loo_comparison_df %>%
  relocate(model)


# 5.6 Save LOO comparison -------------------------------------------------------

write.csv(loo_comparison_df,
          file.path(comparison_folder,
                    "loo_comparison.csv"),
          row.names = FALSE)

saveRDS(loo_comparison_df,
        file.path(comparison_folder,
                  "loo_comparison.rds"))


# ==============================================================================
# 6. Completion
# ==============================================================================

message("\nModel comparison completed successfully.")
message("----------------------------------------\n")


# ==============================================================================
# 11. Unload Packages
# ==============================================================================

packages_to_unload <- 
  c("tidyverse",
    "brms",
    "loo")

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
