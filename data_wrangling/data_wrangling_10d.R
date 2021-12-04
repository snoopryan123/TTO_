########################################################################
# row_idx === row number of original dataset, to preserve the order when arranging temporarily
# remove 
#     WOBA_AVG_PIT, WOBA_AVG_BAT, WOBA_FINAL_BAT, WOBA_FINAL_PIT, 
#     WOBA_AVG_PIT_19, WOBA_AVG_BAT_19, WOBA_FINAL_BAT_19, WOBA_FINAL_PIT_19, 
#     NUM_WOBA_APP_BAT, NUM_WOBA_APP_PIT, NUM_WOBA_APP_FINAL_BAT, NUM_WOBA_APP_FINAL_PIT
########################################################################

library(tidyverse)
input_filename = "../data/retro_final_PA_1990-2020c.csv"
output_filename = "../data/retro_final_PA_1990-2020d.csv"
E <- read_csv(input_filename)

# row_idx
E1 = E %>% mutate(row_idx = row_number()) %>% relocate(row_idx, .before=GAME_ID)
# remove the columns we are replacing
E2 = E1 %>% select(-c("WOBA_AVG_PIT","WOBA_AVG_BAT","WOBA_FINAL_BAT","WOBA_FINAL_PIT",
                      "WOBA_AVG_PIT_19","WOBA_AVG_BAT_19","WOBA_FINAL_BAT_19","WOBA_FINAL_PIT_19",
                      "NUM_WOBA_APP_BAT","NUM_WOBA_APP_PIT","NUM_WOBA_APP_FINAL_BAT","NUM_WOBA_APP_FINAL_PIT")) %>%
  relocate(EVENT_WOBA, .before = EVENT_WOBA_19)

##############################

R = E2
write_csv(R, output_filename)

##############################

