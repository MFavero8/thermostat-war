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


vote_names <-  c("y7" = "3", 
                 "y6" = "2", 
                 "y5" = "1", 
                 "y4" = "0", 
                 "y3" = paste0("\u2212","1"), 
                 "y2" = paste0("\u2212","2"),
                 "y1" = paste0("\u2212","3")) 

