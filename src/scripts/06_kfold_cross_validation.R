# ==============================================================================
# Script: 06_kfold_cross_validation.R
#
# Description: Stratified K-fold cross-validation as a sensitivity analysis
#              of model predictive performance and comparison with LOO rankings
#
# Author: Matteo Favero
# ==============================================================================

message("-------------------------------------------")
message("K-fold cross-validation\n")


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
  library(posterior)
})


# 0.3 Load configuration --------------------------------------------------------

source("src/scripts/config.R")
source("src/scripts/development_settings.R")


# 0.4 Define cross-validation settings ------------------------------------------

K <- 10

seed <- 1234

set.seed(seed)


# ==============================================================================
# 1. Load Analysis Dataset
# ==============================================================================

# 1.1 Load analysis dataset -----------------------------------------------------

message("Loading analysis dataset")

nevins_analysis <- 
  readRDS(file.path(processed_data_folder,
                    "nevins_aggregated_data.rds"))

df <- droplevels(nevins_analysis)


# ==============================================================================
# 2. Select and Load Fitted Models
# ==============================================================================

# 2.1 Select models for cross-validation ----------------------------------------

models_to_cv <- models_to_run


# 2.2 Define fitted model folder ------------------------------------------------

input_model_folder <- fitted_model_folder


# 2.3 Verify sufficient number of models ---------------------------------------

if (length(models_to_cv) < 2) {
  
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
    
    message("K-fold cross-validation not executed.")
    message("----------------------------------------\n")
  })
  
  stop("LOO comparison requires at least two fitted models. ",
       "Currently selected: ",
       paste(models_to_cv,
             collapse = ", "),
       call. = FALSE)
  
}

# 2.4 Check that all fitted models exist -----------------------------------------

model_files <- 
  file.path(input_model_folder,
            paste0(models_to_cv,
                   ".rds"))

if (!all(file.exists(model_files))) {
  
  stop("Missing fitted models:\n",
       paste(model_files[!file.exists(model_files)],
             collapse="\n"))
  
}


# 2.5 Load fitted models --------------------------------------------------------

models <- 
  lapply(model_files,
         readRDS)

names(models) <- models_to_cv


# ==============================================================================
# 3. Create Stratified K-folds
# ==============================================================================

# 3.1 Create stratified folds ---------------------------------------------------

message("Creating folds")

folds <- 
  kfold_split_stratified(K = K,
                         x = df$condition_id)


# ==============================================================================
# 4. Configure Output and Computational Settings
# ==============================================================================

# 4.1 Define output folder ------------------------------------------------------

cv_folder <- cross_validation_folder

if (!dir.exists(cv_folder)) {
  
  dir.create(cv_folder,
             recursive = TRUE)
  
}


# 4.2 Increase future global memory limit ---------------------------------------

options(future.globals.maxSize = 1024 * 1024^2)


# ==============================================================================
# 5. Run K-fold Cross-validation
# ==============================================================================

# 5.1 Run cross-validation for each model ---------------------------------------

for (model_name in names(models)) {
  
  message("\nCross-validation: ",
          model_name)
  
  
  # Define progress and final output files
  
  progress_file <- 
    file.path(cv_folder,
              paste0("K",
                     K,
                     "_CV_progress_",
                     model_name,
                     ".rds"))
  
  final_file <- 
    file.path(cv_folder,
              paste0("K",
                     K,
                     "_CV_final_",
                     model_name,
                     ".rds"))
  
  
  # 5.1.1 Skip models with completed results -----------------------------------
  
  if (file.exists(final_file)) {
    
    message("Final result already exists. Skipping ",
            model_name)
    
    next
    
  }
  
  # 5.1.2 Initialise model-specific cross-validation objects -------------------
  
  base_fit <- models[[model_name]]
  
  N <- nrow(df)
  
  ndraws <- posterior::ndraws(base_fit)
  
  log_lik_mat <- 
    matrix(NA,
           nrow = ndraws,
           ncol = N)
  
  start_fold <- 1
  
  
  # 5.1.3 Resume previous progress if available -------------------------------
  
  if (file.exists(progress_file)) {
    
    message("Loading previous progress")
    
    progress <- readRDS(progress_file)
    
    
    # Safety check: verify model identity
    
    if (progress$model != model_name) {
      
      stop("Progress file belongs to ",
           progress$model,
           " but current model is ",
           model_name)
      
    }
    
    
    # Safety check: verify number of folds
    
    if (progress$K != K) {
      
      stop("Progress file was created with K = ",
           progress$K,
           " but current K = ",
           K)
      
    }
    
    
    # Safety check: verify random seed
    
    if (progress$seed != seed) {
      
      stop("Progress file was created with seed = ",
           progress$seed,
           " but current seed = ",
           seed)
      
    }
    
    
    # Restore saved cross-validation state
    
    log_lik_mat <- progress$log_lik_mat
    
    folds <- progress$folds
    
    start_fold <- progress$fold_completed + 1
    
    message("Resuming from fold ",
            start_fold)
    
  }
  
  
  # 5.1.4 Run individual folds -------------------------------------------------
  
  for (k in start_fold:K) {
    
    message("Running fold ",
            k,
            " / ",
            K)
    
    
    # Identify training and test observations
    
    train_idx <- which(folds != k)
    test_idx <- which(folds == k)
    
    
    # Create training and test datasets
    
    train_df <- df[train_idx, ]
    test_df <- df[test_idx, ]
    
    
    # Refit the model using the training data
    
    fit_k <- 
      update(base_fit,
             newdata = train_df,
             chains = 4,
             cores = 4,
             recompile = FALSE,
             seed = 2026 + k,
             control = list(adapt_delta = 0.95))
    
    
    # Calculate pointwise log-likelihood for the test data
    
    ll_k <- 
      log_lik(fit_k,
              newdata = test_df)
    
    
    # Store test-set log-likelihoods in their original observation positions
    
    log_lik_mat[, test_idx] <- ll_k
    
    
    # Save progress after each completed fold
    
    saveRDS(list(log_lik_mat = log_lik_mat,
                 fold_completed = k,
                 folds = folds,
                 model = model_name,
                 K = K,
                 seed = seed),
            progress_file)
    
    message("Saved progress after fold ",
            k)
    
  }
  
  
  # ============================================================================
  # 5.2 Compute and save final ELPD object
  # ============================================================================
  
  # Identify observations for which log-likelihood values were obtained
  
  complete_idx <- 
    which(!apply(is.na(log_lik_mat),
                 2,
                               all))
  
  
  # Remove observations without cross-validation results
  
  log_lik_mat <- log_lik_mat[ , complete_idx, drop = FALSE]
  
  
  # Compute expected log predictive density
  
  cv_result <- elpd(log_lik_mat)
  
  
  # Save final cross-validation result
  
  saveRDS(cv_result,
          final_file)
  
  message("Saved final CV result: ",
          final_file)
  
  
  # Remove progress file after successful completion
  
  if (file.exists(progress_file)) {
    
    file.remove(progress_file)
    
  }
  
}


# ==============================================================================
# 6. Compare K-fold Cross-validation Results
# ==============================================================================

# 6.1 Check that all final cross-validation results exist -----------------------

message("\nComparing K-fold results")

cv_files <- 
  file.path(cv_folder,
            paste0("K",
                   K,
                   "_CV_final_",
                   models_to_cv,
                   ".rds"))

if (!all(file.exists(cv_files))) {
  
  stop("Missing K-fold results:\n",
       paste(cv_files[!file.exists(cv_files)],
             collapse = "\n"))
  
}


# 6.2 Load cross-validation results --------------------------------------------

cv_results <- 
  lapply(cv_files,
         readRDS)

names(cv_results) <- models_to_cv


# 6.3 Compare model predictive performance -------------------------------------

cv_comparison <- loo_compare(cv_results)

# 6.4 Extract model comparison results -----------------------------------------

cv_comparison_df <-
  as.data.frame(cv_comparison) %>%
  select(model, elpd_diff, se_diff, elpd, se_elpd)


# 6.5 Save comparison table ----------------------------------------------------

write.csv(cv_comparison_df,
          file.path(cv_folder,
                    "Kfold_model_comparison.csv"),
          row.names = FALSE)

saveRDS(cv_comparison_df,
        file.path(cv_folder,
                  "Kfold_model_comparison.rds"))

message("K-fold cross-validation completed.")


# ==============================================================================
# 7. Compare K-fold and LOO Rankings
# ==============================================================================

# 7.1 Load LOO model comparison -------------------------------------------------

message("\nComparing K-fold and LOO rankings")

loo_comparison <- 
  readRDS(file.path(comparison_folder,
                    "loo_comparison.rds"))


# 7.2 Create LOO model rankings -------------------------------------------------

loo_rank <-
  loo_comparison %>%
  mutate(rank_loo = row_number()) %>%
  select(model,
         rank_loo)


# 7.3 Create K-fold model rankings ----------------------------------------------

cv_rank <-
  cv_comparison %>%
  mutate(rank_cv = row_number()) %>%
  select(model,
         rank_cv)


# 7.4 Merge LOO and K-fold rankings ---------------------------------------------

ranking_comparison <-
  inner_join(loo_rank,
             cv_rank,
             by = "model")

# 7.5 Verify that model sets are identical --------------------------------------

if (nrow(ranking_comparison) != length(models_to_cv)) {
  stop("Model names do not match between LOO and K-fold results.")
}


# 7.6 Calculate Spearman rank correlation ---------------------------------------

ranking_correlation <-
  cor.test(ranking_comparison$rank_loo,
           ranking_comparison$rank_cv,
           method = "spearman",
           exact = FALSE)

print(ranking_correlation)

# 7.7 Save ranking comparison results -------------------------------------------

write.csv(ranking_comparison,
          file.path(cv_folder,
                    "ranking_comparison.csv"),
          row.names = FALSE)

saveRDS(ranking_correlation,
        file.path(cv_folder,
                  "ranking_correlation.rds"))

# 7.8 Print Spearman correlation -----------------------------------------------

message("Spearman's rho = ",
        round(unname(ranking_correlation$estimate), 3))

# ==============================================================================
# 8. Completion
# ==============================================================================

message("\nK-fold cross-validation completed")
message("----------------------------------------\n")


# ==============================================================================
# 11. Unload Packages
# ==============================================================================

packages_to_unload <- 
  c("tidyverse",
    "brms",
    "loo", 
    "posterior")

for (package in packages_to_unload) {
  
  if (paste0("package:", package) %in% search()) {
    
    detach(paste0("package:", package),
           unload = FALSE,
           character.only = TRUE)
    
  }
  
}


# ==============================================================================
# 9. Clean Workspace
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
