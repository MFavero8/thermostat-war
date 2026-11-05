# ============================================================
# Script: visualisation_setting.R
#
# Description: Define visualisation settings
#
# Author: Matteo Favero
# ============================================================


col_day <- c("time.day")
col_sex <- c("sex")


values_df_day <- 
  expand.grid(time.day = c("afternoon", "evening"),
              stringsAsFactors = FALSE)
values_df_sex <- 
  expand.grid(sex = c("male", "female"),
              stringsAsFactors = FALSE)


name_day_map <- c("time.day" = "time of day")
name_sex_map <- c("sex" = "sex")


sex_names <- c("male" = "Male",
               "female" = "Female")

day_names <- c("afternoon" = "Afternoon",
               "evening" = "Evening")





fixed_levels <- seq(0, 1, by = 0.05)

onset_filter <- c(1, 3)

onset_names <- c("1" = "1.0 h",
                 "2" = "2.0 h",
                 "3" = "3.0 h")

rh_filter <- c(30, 70)

rh_names <- c("30" = "30 %rh", 
              "70" = "70 %rh")



ppd_ta_layer <- 
  function(ta_values,      # vector of min/max air temp (°C)
           rh_values,      # vector of min/max RH (%)
           vel = 0.05,       # air velocity (m/s)
           clo = 0.6,     # clothing
           met = 1.2,     # metabolic rate
           linetype = "dashed",
           colour = "PPD", 
           fill = "PPD"
           ) {
    # 1) Create fine grid
    ta_seq <- ta_values #seq(ta_range[1], ta_range[2], by = step_t)
    rh_seq <- rh_values #seq(rh_range[1], rh_range[2], by = step_rh)
    grid <- expand.grid(air.temp = ta_seq, rel.hum = rh_seq)
    
    # 2) Compute PPD
    grid$ppd <- 
      vapply(seq_len(nrow(grid)), function(i) {
        as.numeric(calcPMVPPD(ta = grid$air.temp[i], tr = grid$air.temp[i],
                              vel = vel, rh = grid$rel.hum[i], clo = clo, met = met)["ppd"])
        }, numeric(1))
    
    list(layer = suppressWarnings(geom_line(data = grid, 
                                            aes(x = air.temp, 
                                                y = ppd/100, 
                                                group = rel.hum, 
                                                colour = colour, 
                                                fill = fill), 
                                            linetype = linetype)))
    
    }




palette_sex <- c("male" = "#FDB863", "female" = "#B2ABD2") # for 10a.1 and 10b.1

palette_heating_setpoint <- "#E41A1C" # for 10a.2

palette_cooling_setpoint <- "#377EB8" # for 10a.2

palette_UWTDI <- c("#86BC7A", "#FFD700") # for 10a.2
  
label_sex <- c("male" = "Male", "female" = "Female") # for 10b.1

