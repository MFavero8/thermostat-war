# ============================================================
# Script: parameters.R
#
# Description: Define project-wide parameters used during data 
#              preparation and analysis.
#
# Author: Matteo Favero
# ============================================================


# ============================================================
# Thermal sensation vote labels
# ============================================================

thermal_vote_labels <- 
  c(cold = "y1",
    cool = "y2",
    slightly.cool = "y3",
    comfortable = "y4",
    slightly.warm = "y5",
    warm = "y6",
    hot = "y7")


# ============================================================
# Thermal sensation scores
# ============================================================

thermal_vote_scores <- 
  c(y1 = -3,
    y2 = -2,
    y3 = -1,
    y4 = 0,
    y5 = 1,
    y6 = 2,
    y7 = 3)


# ============================================================
# Temperature centering
# ============================================================

air.temp_reference <- 25.5
# °C reference temperature used for centering
# Corresponds approximately to PMV = 0 assumptions:
# MRT = 25.5°C
# air velocity = 0.1 m/s
# RH = 50%
# metabolic rate = 1 met
# clothing = 0.6 clo


# ============================================================
# Relative humidity centering
# ============================================================

rh_reference <- 50
rh_step <- 5
# Centering:
# (RH / 5) - 10
# 0 corresponds to 50% RH


# ============================================================
# Exposure onset times
# ============================================================

onset_time_values <- 
  c("1.0h" = 1,
    "1.5h" = 1.5,
    "2.0h" = 2,
    "2.5h" = 2.5,
    "3.0h" = 3)


# ============================================================
# Exposure centering
# ============================================================

onset_time_reference <- 1
