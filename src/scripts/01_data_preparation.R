# ==============================================================================
# Script: 01_data_preparation.R
#
# Description: Preparation of the Nevins thermal comfort dataset for statistical
#              analysis, including variable transformation, centering,
#              aggregation of thermal sensation votes, and preparation of the
#              multinomial response for Bayesian modelling
#
# Author: Matteo Favero
# ==============================================================================

message("-------------------------------------------")
message("Data preparation\n")


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
})


# 0.3 Load project configuration ------------------------------------------------

source("src/scripts/config.R")
source("src/scripts/parameters.R")


# 0.4 Define folders ------------------------------------------------------------

input_data_file <- 
  file.path(raw_data_folder,
            "nevins_all.csv")

output_data_folder <- 
  processed_data_folder

if (!dir.exists(output_data_folder)) {
  dir.create(output_data_folder,
             recursive = TRUE)
}


# ==============================================================================
# 1. Read Raw Dataset
# ==============================================================================

# 1.1 Check input dataset -------------------------------------------------------

if (!file.exists(input_data_file)) {
  
  stop("Input dataset not found:\n",
       input_data_file)
  
}

# 1.2 Read dataset --------------------------------------------------------------

nevins <- 
  read.csv(input_data_file,
           header = TRUE,
           quote = "\"",
           strip.white = TRUE)

message("Reading Nevins dataset: ", 
        nrow(nevins),
        " rows loaded")


# ==============================================================================
# 2. Create Analysis Variables
# ==============================================================================

message("\nCreating analysis variables")

# 2.1 Create derived and transformed variables ----------------------------------

nevins_analysis <- 
  nevins %>%
  mutate(condition_id = as.factor(condition_id), # experimental condition identifier
         

         # Temperature conversion (Fahrenheit to Celsius)

         air.temp = (as.numeric(air.temp) - 32) * 5/9,
         

         # Centered environmental variables

         c_air.temp = air.temp - air.temp_reference,
         c_rel.hum = (rel.hum / rh_step) - (rh_reference / rh_step),
         

         # Exposure duration

         time.onset.h = unname(onset_time_values[onset.time]),
         c_time.onset.h = time.onset.h - onset_time_reference,
         

         # Thermal sensation vote coding

         vote = unname(thermal_vote_labels[vote])) %>%
  select(-c(onset.time, 
            time.year))


# ==============================================================================
# 3. Aggregate Thermal Sensation Votes
# ==============================================================================
#
# Convert individual thermal sensation votes into multinomial count format
# for Bayesian aggregate-level modelling.
#
# Each row represents one experimental condition and contains the number
# of votes in each thermal sensation category.

message("\nAggregating thermal sensation votes")

# 3.1 Aggregate observations by experimental condition -------------------------

nevins_analysis <- 
  nevins_analysis %>%
  group_by(condition_id,
           rel.hum,
           c_rel.hum,
           air.temp,
           c_air.temp,
           sex,
           time.day,
           time.onset.h,
           c_time.onset.h, 
           vote) %>%
  summarise(rel.count = n(),
            .groups = "drop") %>%
  pivot_wider(names_from = vote,
              values_from = rel.count) %>%
  replace(is.na(.), 0) %>%
  mutate(count =  y1 + y2 + y3 + y4 + y5 + y6 + y7)


# ==============================================================================
# 4. Prepare Dataset for Bayesian Modelling
# ==============================================================================
#
# Create the multinomial response matrix and define factor variables
# required by brms.


# 4.1 Create multinomial response matrix ---------------------------------------

nevins_analysis$vote <- 
  with(nevins_analysis,
       cbind(y1, y2, y3, y4, y5, y6, y7))


# 4.2 Verify multinomial response counts ----------------------------------------

stopifnot(all(nevins_analysis$count == rowSums(nevins_analysis$vote)))


# 4.3 Remove individual vote count columns -------------------------------------

nevins_analysis <- 
  nevins_analysis %>%
  select(-c(y1, y2, y3, y4, y5, y6, y7))


# 4.4 Convert sex to factor -----------------------------------------------------

nevins_analysis$sex <- 
  factor(nevins_analysis$sex)

# 4.5 Set reference category for sex --------------------------------------------

nevins_analysis$sex <- 
  relevel(nevins_analysis$sex, 
          ref = "male")

# 4.6 Convert time of day to factor ---------------------------------------------

nevins_analysis$time.day <- 
  as.factor(nevins_analysis$time.day)

# 4.7 Set reference category for time of day ------------------------------------

nevins_analysis$time.day <- 
  relevel(nevins_analysis$time.day, 
          ref = "afternoon" )


# 4.8 Define ordered exposure duration ------------------------------------------
#
# Keep the same centred exposure-duration variable used previously.
# The specified levels preserve the intended temporal ordering.

nevins_analysis$c_time.onset.h <- 
  factor(nevins_analysis$c_time.onset.h, 
         ordered = TRUE,
         levels = c("0", "0.5", "1", "1.5", "2"))

# 4.9 Select final analysis variables -------------------------------------------
  
nevins_analysis <-
  nevins_analysis %>%
  select(condition_id,
         rel.hum,
         c_rel.hum,
         air.temp,
         c_air.temp,
         sex,
         time.day,
         time.onset.h,
         c_time.onset.h,
         count,
         vote)


# ==============================================================================
# 5. Save Processed Dataset
# ==============================================================================

# 5.1 Define output file --------------------------------------------------------

output_data_file <- 
  file.path(output_data_folder,
            "nevins_aggregated_data.rds")

# 5.2 Save processed dataset ----------------------------------------------------

saveRDS(nevins_analysis,
        output_data_file)

message("\nProcessed dataset saved:")
message("  ", output_data_file)

# ==============================================================================
# 6. Completion
# ==============================================================================


message("\nData preparation completed successfully.")
message("----------------------------------------\n")


# ==============================================================================
# 7. Unload Packages
# ==============================================================================

packages_to_unload <- 
  c("tidyverse")

for (package in packages_to_unload) {
  
  if (paste0("package:", package) %in% search()) {
    
    detach(paste0("package:", package),
           unload = FALSE,
           character.only = TRUE)
    
  }
  
}


# ==============================================================================
# 8. Clean Workspace
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