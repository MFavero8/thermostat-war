# ============================================================
# ============================================================
# Script: visualisation_setting.R
#
# Description: Define visualisation settings
#
# Author: Matteo Favero
# ============================================================


palette_TSV1 <-
  c("#3B76AF", #cold
    "#5E98C7",
    "#87C3D0",
    "#A7D76D", #comfortable 
    "#D6C85A",
    "#DD8A4E",
    "#E65540") #hot


#c("#37ACD9",  # -3  cold
#  "#2CC5C5",  # -2
#  "#6BD3A3",  # -1
#  "#A7D76D",  #  0  comfortable
#  "#D1BA59",  # +1
#  "#E39C58",  # +2
#  "#DF8071"   # +3  hot
#  )

palette_TSV <-
 c("#0072B2",  # coldest blue
   "#4FA3C7",
   "#7CC6C0",
   "#A7D76D",  # comfortable — fixed
   "#E8B500",
   "#E9782A",
   "#C83E2C"   # hottest red
   )

 
#c("#5382A8",  # -3 cold
#  "#6F959C",  # -2
#  "#88AE8C",  # -1
#  "#A7D76D",  #  0 comfortable
#  "#C5B25D",  # +1
#  "#D49452",  # +2
#  "#E07147"   # +3 hot
#)




col_day <- c("time.day")


values_df_day <- 
  expand.grid(time.day = c("afternoon", "evening"),
              stringsAsFactors = FALSE)


name_day_map <- c("time.day" = "time of day")

fixed_levels <- c(-3, -2.5, -2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5, 3)


sex_names <- c("male" = "Male",
               "female" = "Female")

onset_filter <- c(1, 3)

onset_names <- c("1" = "1.0 h",
                 "2" = "2.0 h",
                 "3" = "3.0 h")

rh_filter <- c(30, 70)

rh_names <- c("30" = "30 %rh", 
              "70" = "70 %rh")



pmv_contour_layer <- 
  
  function(ta_range, # vector of min/max air temp (°C)
           rh_range, # vector of min/max RH (%)
           vel = 0.05, # air velocity (m/s)
           clo = 0.6, # clothing
           met = 1.2, # metabolic rate
           step_t = 0.05, # temp setp for smooth contours
           step_rh = 0.2, # rh step for smooth contours
           linetype = "dashed"
  ) {
    # 1) Create fine grid
    ta_seq <- seq(ta_range[1], ta_range[2], by = step_t)
    rh_seq <- seq(rh_range[1], rh_range[2], by = step_rh)
    grid <- expand.grid(ta = ta_seq, rh = rh_seq)
    
    # 2) Compute PMV
    grid$pmv <- vapply(seq_len(nrow(grid)), function(i) {
      as.numeric(calcPMVPPD(ta = grid$ta[i],
                            tr = grid$ta[i],
                            vel = vel,
                            rh = grid$rh[i],
                            clo = clo,
                            met = met)["pmv"])
    }, numeric(1))
    
    
    levels <- c(-0.5, 0.0, 0.5)
    
    labels <- c(paste0( "PMV ","\u2212","0.5"),
                paste0( "PMV 0.0"),
                paste0( "PMV 0.5")) 
    
    list(layer = geom_contour2(data = grid,
                               aes(x = ta,
                                   y = rh,
                                   z = pmv,
                                   label = after_stat(paste0( " ",
                                                              factor(cut(level,
                                                                         breaks = c(-Inf, levels, Inf),  
                                                                         labels = c(labels, NA),
                                                                         right = TRUE)), 
                                                              " "))),
                               breaks = levels,
                               colour = "black",
                               linetype = linetype,
                               linewidth = 0.3,
                               label.placer = label_placer_fraction(0.78),
                               skip = 0,
                               label_size = 2.7))
    
  }




pmv_ta_layer <- function(
    ta_values, # vector of  air temp (°C)
    rh_values, # vector of  RH (%)
    vel = 0.05, # air velocity (m/s)
    clo = 0.6, # clothing
    met = 1.2, # metabolic rate
    linetype,
    colour = "PPD", 
    fill = "PPD"
) {
  # 1) Create fine grid
  ta_seq <- ta_values 
  rh_seq <- rh_values 
  grid <- expand.grid(air.temp = ta_seq, rel.hum = rh_seq)
  
  # 2) Compute PMV
  grid$pmv <- vapply(seq_len(nrow(grid)), function(i) {
    as.numeric(calcPMVPPD(ta = grid$air.temp[i], 
                          tr = grid$air.temp[i],
                          vel = vel, 
                          rh = grid$rel.hum[i], 
                          clo = clo, 
                          met = met)["pmv"])
    }, numeric(1))
  
  list(layer = suppressWarnings(geom_line(data = grid, 
                                          aes(x = air.temp, 
                                              y = pmv, 
                                              group = rel.hum, 
                                              colour = colour, 
                                              fill = fill), 
                                              #colour = paste(sprintf('%.1f', met), "met"), 
                                              #fill = paste(sprintf('%.1f', met), "met")), 
                                          linetype = linetype, 
                                          linewidth = 0.4)))
  
  }



palette_sex <-  c("male" = "#FDB863", "female" = "#B2ABD2") # for 8a.2

label_sex <- c("male" = "Male", "female" = "Female") # for 8a.2


