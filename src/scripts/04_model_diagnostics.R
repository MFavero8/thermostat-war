# ==============================================================================
# Script: 04_model_diagnostics.R
#
# Description: Convergence and Hamiltonian Monte Carlo (HMC) diagnostics
#              for fitted Bayesian models
#
# Author: Matteo Favero
# ==============================================================================

message("-------------------------------------------")
message("Model diagnostics\n")


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
  library(brms)
  library(rstan)
  library(posterior)
})


# 0.3 Diagnostic thresholds ----------------------------------------------------

rhat_threshold <- 1.01
bulk_ess_threshold <- 400
tail_ess_threshold <- 400
ebfmi_threshold <- 0.3


# 0.4 Load development settings ------------------------------------------------

source("src/scripts/config.R")
source("src/scripts/development_settings.R")


# 0.5 Define folders -----------------------------------------------------------

input_model_folder <- fitted_model_folder

if (!dir.exists(diagnostic_folder)) {
  
  dir.create(diagnostic_folder, recursive = TRUE)
  
}


# ==============================================================================
# 1. Load Fitted Models
# ==============================================================================

# 1.1 Select models to check ---------------------------------------------------

models_to_check <- models_to_run

existing_models <-
  models_to_check[file.exists(file.path(input_model_folder, 
                                        paste0(models_to_check,".rds")))]


if (length(existing_models) == 0) {
  
  stop("No fitted models found.")
  
}


# 1.2 Load fitted models -------------------------------------------------------

models <-
  lapply(existing_models,
    
    function(model_name) {
      
      readRDS(file.path(input_model_folder,
                        paste0(model_name, ".rds")))
      
    }
  )

names(models) <- existing_models


# 1.3 Verify backend -----------------------------------------------------------

if (!inherits(models[[1]]$fit, "stanfit")) {
  
  stop(paste("The fitted models do not appear to use the rstan backend.",
             "Check class(models[[1]]$fit)."))
  
}

# ==============================================================================
# 2. Parameter-Level Diagnostics
# ==============================================================================

# 2.1 Extract parameter diagnostics --------------------------------------------

extract_parameter_diagnostics <- 
  function(fit, model_name) {
    
    # Extract posterior draws
  
    draws <-
    posterior::as_draws_array(fit$fit)
  
    
    # Calculate posterior diagnostics
  
    diagnostics <-
      posterior::summarise_draws(draws)
  
    
    # Check required diagnostic columns
    
    required_columns <- 
      c("variable",
        "rhat",
        "ess_bulk",
        "ess_tail")
  
    missing_columns <-
      setdiff(required_columns,
              names(diagnostics))
    
    if (length(missing_columns) > 0) {
      
      stop(paste("Missing posterior diagnostic columns:",
                 paste(missing_columns, collapse = ", ")))
      
    }
  
  
    # Rename diagnostic columns
    
    diagnostics <-
      diagnostics %>%
      rename(parameter = variable,
             Rhat = rhat,
             Bulk_ESS = ess_bulk,
             Tail_ESS = ess_tail)
  
    
    # Extract Monte Carlo standard error
  
    mcse_results <-
      posterior::summarise_draws(draws, posterior::mcse_mean)
  
    names(mcse_results)[1] <- "parameter"
    
    names(mcse_results)[2] <- "MCSE"
    
    diagnostics <-
      diagnostics %>%
      select(parameter, Rhat, Bulk_ESS, Tail_ESS) %>%
      left_join(mcse_results %>%
                  select(parameter, MCSE),
                by = "parameter")
  
    
    # Add model identifier
    
    diagnostics$model <-
      model_name
  
    
    # Flag parameters exceeding diagnostic thresholds
    
    diagnostics <-
      diagnostics %>%
      mutate(rhat_flag = !is.na(Rhat) &  Rhat >= rhat_threshold,
             bulk_ess_flag = !is.na(Bulk_ESS) & Bulk_ESS <= bulk_ess_threshold,
             tail_ess_flag = !is.na(Tail_ESS) & Tail_ESS <= tail_ess_threshold,
             diagnostic_flag = rhat_flag | bulk_ess_flag | tail_ess_flag)
  
  
    # Order output columns
    
    diagnostics <-
      diagnostics %>%
      select(model, parameter, Rhat, Bulk_ESS, Tail_ESS, MCSE, everything())
    
    diagnostics
  
}


# 2.2 Extract parameter diagnostics for all models -----------------------------

message("Checking parameter convergence:")

parameter_diagnostics_list <- list()

for (model_name in names(models)) {
  
  message("  ", model_name)
  
  parameter_diagnostics_list[[model_name]] <-
    extract_parameter_diagnostics(models[[model_name]],
                                  model_name)
  
}

parameter_diagnostics <-
  bind_rows(parameter_diagnostics_list)


# 2.3 Save parameter diagnostics -----------------------------------------------

write.csv(parameter_diagnostics,
          file.path(diagnostic_folder,
                    "parameter_diagnostics.csv"),
          row.names = FALSE)


# ==============================================================================
# 3. HMC / Sampler Diagnostics
# ==============================================================================

# 3.1 Extract HMC diagnostics --------------------------------------------------

extract_hmc_diagnostics <- function(fit, model_name) {
  
  sampler_params <-
    rstan::get_sampler_params(fit, inc_warmup = FALSE)
  
  diagnostics <-
    lapply(seq_along(sampler_params),
      
      function(chain) {
        
        sp <-
          sampler_params[[chain]]
        
        
        # Calculate E-BFMI from the sampler energy
        
        energy <-
          sp[, "energy__"]
        
        ebfmi <-
          mean(diff(energy)^2) / var(energy)
        
        
        # Maximum treedepth reached by the chain
        
        max_treedepth <-
          max(sp[, "treedepth__"], na.rm = TRUE)
        
        
        # Number of transitions reaching treedepth 10
        
        num_max_treedepth <-
          sum(sp[, "treedepth__"] >= 10, na.rm = TRUE)
        
        
        # Leapfrog step diagnostics
        
        max_n_leapfrog <-
          max(sp[, "n_leapfrog__"], na.rm = TRUE)
        
        
        mean_n_leapfrog <-
          mean(sp[, "n_leapfrog__"],  na.rm = TRUE)
        
    
        # Return diagnostics
        
        tibble(model = model_name,
               chain = chain,
               iterations = nrow(sp),
               divergent_transitions = sum(sp[, "divergent__"] == 1, na.rm = TRUE),
               max_treedepth = max_treedepth,
               num_max_treedepth = num_max_treedepth,
               mean_treedepth = mean(sp[ , "treedepth__"],  na.rm = TRUE),
               max_n_leapfrog = max_n_leapfrog,
               mean_n_leapfrog = mean_n_leapfrog,
               mean_acceptance_stat = mean(sp[, "accept_stat__"], na.rm = TRUE),
               mean_stepsize = mean(sp[,"stepsize__"], na.rm = TRUE),
               ebfmi = ebfmi)
        
      }
    )
  
 
  
  bind_rows(diagnostics)
  
}

# 3.2 Extract HMC diagnostics for all models -----------------------------------

message("\nChecking HMC diagnostics:")

hmc_results_list <- list()

for (model_name in names(models)) {
  
  message("  ", model_name)
  
  hmc_results_list[[model_name]] <-
    extract_hmc_diagnostics(models[[model_name]]$fit,
                            model_name)
  
}


hmc_results <-
  bind_rows(hmc_results_list)


# 3.3 Save HMC diagnostics -----------------------------------------------------

write.csv(hmc_results,
          file.path(diagnostic_folder,
                    "hmc_diagnostics.csv"),
          row.names = FALSE)


# ==============================================================================
# 4. Treedepth Diagnostics
# ==============================================================================

# 4.1 Extract treedepth distribution -------------------------------------------

extract_treedepth_distribution <- 
  function(fit,  model_name) {
    
    sampler_params <-
      rstan::get_sampler_params(fit, inc_warmup = FALSE)
  
    results <-
      lapply(seq_along(sampler_params),
        function(chain) {
          tibble(model = model_name,
                 chain = chain,
                 treedepth = sampler_params[[chain]][, "treedepth__"])
        }
      )
    
    bind_rows(results) %>%
      count(model,
            chain,
            treedepth,
            name = "iterations")
  
}

# 4.2 Extract treedepth distributions for all models ---------------------------

message("\nChecking treedepth distribution:")

treedepth_results_list <- list()

for (model_name in names(models)) {
  
  message("  ", model_name)
  
  treedepth_results_list[[model_name]] <-
    extract_treedepth_distribution(models[[model_name]]$fit, model_name)
  
}

treedepth_results <- bind_rows(treedepth_results_list)


# 4.3 Save treedepth diagnostics -----------------------------------------------

write.csv(treedepth_results,
          file.path(diagnostic_folder,
                    "treedepth_distribution.csv"),
          row.names = FALSE)


# ==============================================================================
# 5. Model-Level Diagnostic Summaries
# ==============================================================================

# 5.1 Model-level convergence summary ------------------------------------------

convergence_results <-
  parameter_diagnostics %>%
  group_by(model) %>%
  summarise(n_parameters = n(),
            max_rhat = max(Rhat, na.rm = TRUE),
            min_bulk_ess = min(Bulk_ESS, na.rm = TRUE),
            min_tail_ess = min(Tail_ESS, na.rm = TRUE),
            n_rhat_problems = sum(rhat_flag, na.rm = TRUE),
            n_bulk_ess_problems = sum(bulk_ess_flag, na.rm = TRUE),
            n_tail_ess_problems = sum(tail_ess_flag, na.rm = TRUE),
            .groups = "drop")


# 5.2 Model-level HMC summary --------------------------------------------------

hmc_summary <-
  hmc_results %>%
  group_by(model) %>%
  summarise(total_divergent_transitions = sum(divergent_transitions, na.rm = TRUE),
            max_treedepth = max(max_treedepth, na.rm = TRUE),
            total_max_treedepth_transitions = sum(num_max_treedepth, na.rm = TRUE),
            mean_treedepth =mean(mean_treedepth, na.rm = TRUE),
            max_n_leapfrog = max(max_n_leapfrog, na.rm = TRUE),
            mean_n_leapfrog = mean(mean_n_leapfrog, na.rm = TRUE),
            min_ebfmi = min(ebfmi, na.rm = TRUE),
            min_acceptance_stat = min(mean_acceptance_stat, na.rm = TRUE),
            max_acceptance_stat = max(mean_acceptance_stat, na.rm = TRUE),
            .groups = "drop")


# 5.3 Overall diagnostic summary -----------------------------------------------

diagnostic_summary <-
  convergence_results %>%
  left_join(hmc_summary, by = "model") %>%
  mutate(rhat_ok = max_rhat < rhat_threshold,
         bulk_ess_ok = min_bulk_ess > bulk_ess_threshold,
         tail_ess_ok = min_tail_ess > tail_ess_threshold,
         divergences_ok = total_divergent_transitions == 0,
         treedepth_ok = total_max_treedepth_transitions == 0,
         ebfmi_ok = min_ebfmi >= ebfmi_threshold,
         overall_ok = rhat_ok & bulk_ess_ok & tail_ess_ok & divergences_ok & ebfmi_ok)


# 5.4 Save diagnostic summary --------------------------------------------------

write.csv(diagnostic_summary,
          file.path(diagnostic_folder,
                    "diagnostic_summary.csv"),
          row.names = FALSE)


# ==============================================================================
# 6. Diagnostic Summary Output
# ==============================================================================

# 6.1 Print diagnostic summary -------------------------------------------------

message("\n.........................................")

message("\nDiagnostic summary")



for (i in seq_len(nrow(diagnostic_summary))) {
  
  x <-
    diagnostic_summary[i, ]
  
  message("\nModel ", x$model)
  
  message("  Parameters: ", x$n_parameters)
  
  message("  Maximum R-hat: ", round(x$max_rhat, 4))

  message("  Minimum Bulk ESS: ", round(x$min_bulk_ess, 1))
  
  message("  Minimum Tail ESS: ", round(x$min_tail_ess, 1))
  
  message("  Divergent transitions: ", x$total_divergent_transitions)
  
  message("  Maximum treedepth: ", x$max_treedepth)
  
  message("  Transitions at treedepth 10: ", x$total_max_treedepth_transitions)
  
  message("  Minimum E-BFMI: ", round(x$min_ebfmi, 3))
  
  message("  Mean treedepth: ", round(x$mean_treedepth, 2))
  
  message("  Mean leapfrog steps: ", round(x$mean_n_leapfrog, 1))
  
  message("  Acceptance statistic range: ", round(x$min_acceptance_stat, 3), " - ", round(x$max_acceptance_stat, 3))
  

  
  if (x$overall_ok) {
    
    message("  STATUS: OK")
    
  } else {
    
    message("  STATUS: CHECK REQUIRED")
    
  }
  
}


message("\n.........................................")


# ==============================================================================
# 7. Diagnostic Warnings
# ==============================================================================

# 7.1 R-hat warnings -----------------------------------------------------------

message("\nChecking R-hat:")

rhat_problems <-
  parameter_diagnostics %>%
  filter(rhat_flag) %>% 
  select(model, parameter, Rhat)

if (nrow(rhat_problems) == 0) {
  
  message("  ✓ No parameters with R-hat >= 1.01")
  
} else {
  
  warning(paste0(nrow(rhat_problems), " parameters have R-hat >= 1.01."))
  
  print(rhat_problems)
  
}


# 7.2 Bulk ESS warnings --------------------------------------------------------

message("\nChecking Bulk ESS:")

bulk_ess_problems <-
  parameter_diagnostics %>%
  filter(bulk_ess_flag) %>%
  select(model, parameter, Bulk_ESS)

if (nrow(bulk_ess_problems) == 0) {
  
  message("  ✓ No parameters with Bulk ESS <= 400")
  
} else {
  
  warning(paste0(nrow(bulk_ess_problems), " parameters have Bulk ESS <= 400."))
  
  print(bulk_ess_problems)
  
}


# 7.3 Tail ESS warnings --------------------------------------------------------

message("\nChecking Tail ESS:")

tail_ess_problems <-
  parameter_diagnostics %>%
  filter(tail_ess_flag) %>%
  select(model, parameter, Tail_ESS)

if (nrow(tail_ess_problems) == 0) {
  
  message("  ✓ No parameters with Tail ESS <= 400")
  
} else {
  
  warning(paste0(nrow(tail_ess_problems), " parameters have Tail ESS <= 400.")
  )
  
  print(tail_ess_problems)
  
}


# 7.4 Divergence warnings ------------------------------------------------------

message("\nChecking divergent transitions:")

if (all(hmc_results$divergent_transitions == 0, na.rm = TRUE)) {
  
  message("  ✓ No divergent transitions")
  
} else {
  
  warning("  Some chains contain divergent transitions.")
  
}


# 7.5 Treedepth warnings -------------------------------------------------------

message("\nChecking maximum treedepth:")

if (all(hmc_results$num_max_treedepth == 0, na.rm = TRUE)) {
  
  message("  ✓ No transitions reached treedepth 10")
  
} else {
  
  warning(paste0(sum(hmc_results$num_max_treedepth, na.rm = TRUE),
                 " transitions reached treedepth 10."))
  
}


# 7.6 E-BFMI warnings ----------------------------------------------------------

message("\nChecking E-BFMI:")

if (all(hmc_results$ebfmi >= ebfmi_threshold, na.rm = TRUE)) {
  
  message("  ✓ E-BFMI >= ", ebfmi_threshold, " for all chains")
  
} else {
  
  warning("  Some chains have E-BFMI < 0.3.")
  
}


# ==============================================================================
# 8. Parameters with Weakest Diagnostics
# ==============================================================================

# 8.1 Exclude log posterior ----------------------------------------------------

parameter_diagnostics_substantive <-
  parameter_diagnostics %>%
  filter(parameter != "lp__")


# 8.2 Identify weakest parameters ----------------------------------------------

worst_parameters <-
  parameter_diagnostics_substantive %>%
  arrange(desc(Rhat), Bulk_ESS, Tail_ESS) %>%
  group_by(model) %>%
  slice_head(n = 20) %>%
  ungroup()


# 8.3 Save weakest parameter diagnostics ---------------------------------------

write.csv(worst_parameters,
          file.path(diagnostic_folder,
                    "worst_parameters.csv"), 
          row.names = FALSE)


# ==============================================================================
# 9. Detailed Sampler Diagnostics
# ==============================================================================

message("\n.........................................")
message("\nStan sample diagnostics")


# 9.1 Diagnostics by model and chain -------------------------------------------

for (model_name in names(models)) {
  
  message("\nModel ", model_name)
  
  sampler_params <-
    rstan::get_sampler_params(models[[model_name]]$fit, inc_warmup = FALSE)
  
  
  for (chain in seq_along(sampler_params)) {
    
    sp <-
      sampler_params[[chain]]
    
    energy <-
      sp[, "energy__"]
    
    
    ebfmi <- 
      mean(diff(energy)^2) / var(energy)
    
    message("  Chain ", chain, ": ", 
            nrow(sp), " post-warmup iterations; ",
            sum(sp[ , "divergent__"], na.rm = TRUE), " divergences; ",
            "max treedepth = ", max(sp[ ,"treedepth__"], na.rm = TRUE), ";", 
            "max leapfrog = ", max(sp[ , "n_leapfrog__"], na.rm = TRUE ), ";", 
            "mean acceptance = ", round(mean(sp[, "accept_stat__"], na.rm = TRUE), 3), ";",
            "mean stepsize = ", round(mean(sp[ , "stepsize__"], na.rm = TRUE), 5), ";", 
            "E-BFMI = ", round(ebfmi, 3))
    
  }
  
}


# ==============================================================================
# 10. Completion
# ==============================================================================


message("\nModel diagnostics completed")


if (all(diagnostic_summary$overall_ok, na.rm = TRUE)) {
  
  message("All models passed automatic diagnostics")
  
} else {
  
  warning("One or more models require diagnostic inspection.")
  
}

message("----------------------------------------\n")


# ==============================================================================
# 11. Unload Packages
# ==============================================================================

packages_to_unload <- 
  c("tidyverse",
    "brms",
    "rstan",
    "posterior")

for (package in packages_to_unload) {
  
  if (paste0("package:", package) %in% search()) {
    
    detach(paste0("package:", package),
           unload = FALSE,
           character.only = TRUE)
    
  }
  
}


# ==============================================================================
# 12. Clean Workspace
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