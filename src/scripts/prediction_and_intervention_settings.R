# ==============================================================================
# Script: prediction_and_intervention_settings.R
#
# Description: prediction
#              define settings used for posterior prediction.
#
#              intervention
#              define settings used for intervention-based predictions.
#              These settings are used by 07_predictions.R and all 
#              subsequent scripts that rely on these predictions.
#
#              this script specifies:
#               - prediction type
#               - metabolic rate scenario
#               - heating interventions
#               - cooling interventions
#
# Author: Matteo Favero 
# ==============================================================================


# ==============================================================================
# 1. Prediction
# ==============================================================================

# 1.1 Number of intervention points per prediction chunk -----------------------

# A chunk contains complete combinations of:
# temperature × humidity and never splits them.
#
# The actual number of rows per chunk is calculated
# automatically in 07_predictions.R.

interventions_per_chunk <- 20


# 1.2 Prediction seed ----------------------------------------------------------

prediction_seed <- 2026


# ==============================================================================
# 2. Intervention
# ==============================================================================

# 2.1 Metabolic rate scenario --------------------------------------------------

# Supported values:
#   1.05
#   1.10

#metabolic_rate <- 1.05
metabolic_rate <- 1.10


# 2.2 Heating interventions ----------------------------------------------------

heating_interventions <-
  list(met_1.05 = list(temperature = c(initial = 25.4,
                                       intervention = 23.9),
                       humidity = c(initial = 35,
                                    intervention = 35)),
       met_1.10 = list(temperature = c(initial = 25.0,
                                       intervention = 23.5),
                       humidity = c(initial = 35,
                                    intervention = 35)))


# 2.3 Cooling interventions ----------------------------------------------------

cooling_interventions <-
  list(met_1.05 = list(temperature = c(initial = 24.9,
                                       intervention = 26.4),
                       humidity = c(initial = 55,
                                    intervention = 55)),
       met_1.10 = list(temperature = c(initial = 24.5,
                                       intervention = 26.0),
                       humidity = c(initial = 55,
                                    intervention = 55)))

# 2.3 Intervention seed --------------------------------------------------------

intervention_seed <- 2026

