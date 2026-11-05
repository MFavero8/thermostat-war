# ============================================================
# Script: cost_schemes.R
#
# Description: Define cost schemes
#
# Author: Matteo Favero
# ============================================================

cost_schemes <- 
  list(Fanger = c(y1 = 1, y2 = 1, y3 = 0, y4 = 0, y5 = 0, y6 = 1, y7 = 1),
       Comfort = c(y1 = 1, y2 = 1, y3 = 1, y4 = 0, y5 = 1, y6 = 1, y7 = 1),
       Symmetric = c(y1 = 1, y2 = 0.75, y3 = 0.25, y4 = 0, y5 = 0.25, y6 = 0.75, y7 = 1))
