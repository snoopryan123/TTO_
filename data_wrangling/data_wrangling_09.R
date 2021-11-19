library(tidyverse)

########################################################################
# BASE_STATE === {000,100,010,001,110,101,011,111} 
# fix EVENT_RUNS, ER
########################################################################

################################
########### THE CODE ###########
################################

input_filename = "../data/retro_final_PA_1990-2020b.csv"
output_filename = "retro_final_PA_1990-2020c.csv"
E <- read_csv(input_filename)
E0 <- E %>% filter(YEAR %in% c(2019,2020))

# BASE_STATE
E1 <- E0 %>% select(GAME_ID,INNING,BAT_HOME_IND,EVENT_TX,OUTS_CT,EVENT_RUNS,EVENT_ER_CT)
# initialize
E1$MAN_ON_1ST = 0
E1$MAN_ON_2ND = 0
E1$MAN_ON_3RD = 0
#View(E1)
E11 <- E1 %>% filter(GAME_ID == "ANA201904040", INNING == 5, BAT_HOME_IND == 0)


perform_base_progressions <- function(EE) {
  # make sure EE is grouped by GAME_ID, INNING, BAT_HOME_ID
  for (i in 1:nrow(EE)) {
    # current event
    e_curr = EE[i,]
    event_tx = e_curr$EVENT_TX

    if (i > 1) {
      # account for men on base from the previous event
      e_prev = EE[i-1,]
      mo1 = e_prev$MAN_ON_1ST
      mo2 = e_prev$MAN_ON_2ND 
      mo3 = e_prev$MAN_ON_3RD;
    } else {
      # initialize men on base for the first event of the half inning
      mo1 = 0; mo2 = 0; mo3 = 0;
    }
    
    # move the baserunners
    mtb = move_the_baserunners(event_tx,mo1,mo2,mo3)
    mo1 = mtb[1]
    mo2 = mtb[2]
    mo3 = mtb[3]
    # account for the main result of this event (e.g.,single,walk,...)
    gmbe = get_main_base_event(event_tx)
    mo1 = ifelse(gmbe==1, 1, mo1)
    mo2 = ifelse(gmbe==2, 1, mo2)
    mo3 = ifelse(gmbe==3, 1, mo3)
    # store result
    EE[i,]$MAN_ON_1ST = mo1
    EE[i,]$MAN_ON_2ND = mo2
    EE[i,]$MAN_ON_3RD = mo3
  }
  EE
}

move_the_baserunners <- function(event_tx,mo1,mo2,mo3) {
  b = str_split_fixed(event_tx, "\\.", 2)[,2]
  
  b1 = str_split_fixed(b, "\\;", 3)[,1]
  b2 = str_split_fixed(b, "\\;", 3)[,2]
  b3 = str_split_fixed(b, "\\;", 3)[,3]
  
  c1 = str_split_fixed(b1, "\\-", 2)
  c2 = str_split_fixed(b2, "\\-", 2)
  c3 = str_split_fixed(b3, "\\-", 2)
  
  # remove 1st baserunner from starting base
  mo1 = ifelse(c1[,1] == "1", 0, mo1)
  mo2 = ifelse(c1[,1] == "2", 0, mo2)
  mo3 = ifelse(c1[,1] == "3", 0, mo3)
  # add 1st baserunner to destination base
  mo1 = ifelse(c1[,2] == "1", 1, mo1)
  mo2 = ifelse(c1[,2] == "2", 1, mo2)
  mo3 = ifelse(c1[,2] == "3", 1, mo3)
  # remove 2nd baserunner from starting base
  mo1 = ifelse(c2[,1] == "1", 0, mo1)
  mo2 = ifelse(c2[,1] == "2", 0, mo2)
  mo3 = ifelse(c2[,1] == "3", 0, mo3)
  # add 2nd baserunner to destination base
  mo1 = ifelse(c2[,2] == "1", 1, mo1)
  mo2 = ifelse(c2[,2] == "2", 1, mo2)
  mo3 = ifelse(c2[,2] == "3", 1, mo3)
  # remove 3rd baserunner from starting base
  mo1 = ifelse(c3[,1] == "1", 0, mo1)
  mo2 = ifelse(c3[,1] == "2", 0, mo2)
  mo3 = ifelse(c3[,1] == "3", 0, mo3)
  # add 3rd baserunner to destination base
  mo1 = ifelse(c3[,2] == "1", 1, mo1)
  mo2 = ifelse(c3[,2] == "2", 1, mo2)
  mo3 = ifelse(c3[,2] == "3", 1, mo3)
  
  c(mo1,mo2,mo3)
}

get_main_base_event <- function(event_tx) {
  a = str_split_fixed(event_tx, "\\.", 2)[,1]
  
  mo1 = (str_detect(a, "^S") & !str_detect(a, "^SB")) | 
    str_detect(a, "^W") | str_detect(a, "^IW") |
    str_detect(a, "^WP") | str_detect(a, "^PB") | str_detect(a, "^HP")
  
  mo2 = str_detect(a, "^D") & !str_detect(a, "^DI")
  
  mo3 = str_detect(a, "^T")
  
  ifelse(mo1, 1, ifelse(mo2, 2, ifelse(mo3, 3, 0)))
}
# E1$EVENT_TX[str_detect(E1$EVENT_TX, "^HP")]
# E1$EVENT_TX[!str_detect(E1$EVENT_TX, "^[0-9]") & 
#               !str_detect(E1$EVENT_TX, "^K") &
#               !str_detect(E1$EVENT_TX, "^S[0-9]") &
#               !str_detect(E1$EVENT_TX, "^D[0-9]") &
#               !str_detect(E1$EVENT_TX, "^T[0-9]") &
#               !str_detect(E1$EVENT_TX, "^W") &
#               !str_detect(E1$EVENT_TX, "^HR")]


E22 <- perform_base_progressions(E11)


E2 <- E1 %>% group_by(GAME_ID,INNING,BAT_HOME_IND) %>% perform_base_progressions()
  


##############################

R = E3
write_csv(R, output_filename)


