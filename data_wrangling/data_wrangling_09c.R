########################################################################
# BASE_STATE === {000,100,010,001,110,101,011,111} 
# note that EVENT_ER_CT, EVENT_RBI_CT need to be fixed, but EVENT_RUNS is ok,
#       so delete the first 2 of these columns and fix late... 
#       check unique(E1$EVENT_RUNS)
########################################################################

################################
########## BASE_STATE ##########
################################

library(tidyverse)
input_filename = "../data/retro_final_PA_1990-2020b.csv"
output_filename = "../data/retro_base_states_1990-2020c.csv"
E <- read_csv(input_filename)
E1 <- E #%>% filter(YEAR %in% c(2019,2020))  %>% filter(row_number() <= 10000) 
E1 <- E1 %>% select(GAME_ID,INNING,BAT_HOME_IND,BAT_ID,WOBA_APP,EVENT_TX,)
rm(E)

# initialize BASE_STATE
E1$MAN_ON_1ST = 0
E1$MAN_ON_2ND = 0
E1$MAN_ON_3RD = 0
#View(E1)
#E11 <- E1 %>% filter(GAME_ID == "ANA201905240", INNING == 7, BAT_HOME_IND == 0)
counter = 0
perform_base_progressions <- function(EE) {
  # make sure EE is grouped by GAME_ID, INNING, BAT_HOME_ID
  for (i in 1:nrow(EE)) {
    print(counter)
    assign("a", "new", envir = .GlobalEnv)
    counter <<- counter+1
    # current event
    e_curr = EE[i,]
    event_tx = e_curr$EVENT_TX

    if (i == nrow(EE)) {
      # the last event of the inning -> no baserunners after that event
      EE[i,]$MAN_ON_1ST = NA
      EE[i,]$MAN_ON_2ND = NA
      EE[i,]$MAN_ON_3RD = NA
      return(EE)
    } else if (i > 1) {
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
    if (e_curr$WOBA_APP) {
      gmbe = get_main_base_event(event_tx)
      mo1 = ifelse(gmbe==1, 1, mo1)
      mo2 = ifelse(gmbe==2, 1, mo2)
      mo3 = ifelse(gmbe==3, 1, mo3)
    }
    # store result
    EE[i,]$MAN_ON_1ST = mo1
    EE[i,]$MAN_ON_2ND = mo2
    EE[i,]$MAN_ON_3RD = mo3
  }
  EE
}

move_the_baserunners <- function(event_tx,mo1,mo2,mo3) {
  b00 = str_split_fixed(event_tx, "\\.", 2)[,2]
  
  # remove both the 2X3 and the error. ex: b00="2X3(5E4);3XH(E)"
  b001 = str_remove_all(b00, "[B123]X[123H]\\(.?E.?\\)\\;?")
  b002 = ifelse(str_sub(b001,-1) == ";", str_sub(b001,1,-2), b001)
  
  bb1 = str_extract_all(b00, "[B123]X[123H]\\(.?E.?\\)")[[1]]
  bb2 = str_remove_all(bb1, "(\\(.+?\\))+")
  bb3 = str_replace_all(bb2,"X","\\-")
  bb4 = paste(bb3,collapse=";")
  b0 = ifelse(bb4 == "", b002,
              ifelse(b002 == "", bb4,
                     paste(c(b002,bb4),collapse=";")))

  # now remove all errors; for example, "S8/L+.3-H(UR)(E8);2-H;B-2(E8)"
  b = str_remove_all(b0, "(\\(.+?\\))+")
  
  a = str_split_fixed(event_tx, "\\.", 2)[,1]
  # stolen base SB
  if (str_detect(a, "SB2") & !str_detect(b, "1\\-") & !str_detect(b, "1X")) {
    b = paste0(b,"1-2")
  } else if (str_detect(a, "SB3") & !str_detect(b, "2\\-") & !str_detect(b, "2X")) {
    b = paste0(b,"2-3")
  } else if (str_detect(a, "SBH") & !str_detect(b, "3\\-") & !str_detect(b, "3X")) {
    b = paste0(b,"3-H")
  }
  # caught stealing CS & POCS
  a = str_replace(a,"POCS","CS")
  if (str_detect(a, "CS2") & !str_detect(b, "1\\-") & !str_detect(b, "1X")) {
    b = paste0(b,"1X2")
  } else if (str_detect(a, "CS3") & !str_detect(b, "2\\-") & !str_detect(b, "2X")) {
    b = paste0(b,"2X3")
  } else if (str_detect(a, "CSH") & !str_detect(b, "3\\-") & !str_detect(b, "3X")) {
    b = paste0(b,"3XH")
  }
  # picked off PO 
  a = str_remove(a, "PO[123]\\(.?E.?\\)")
  #(account for errors)
  if (str_detect(a, "PO1") & !str_detect(b, "1\\-") & !str_detect(b, "1X")) {
    b = paste0(b,"1X1")
  } else if (str_detect(a, "PO2") & !str_detect(b, "2\\-") & !str_detect(b, "2X")) {
    b = paste0(b,"2X2")
  } else if (str_detect(a, "PO3") & !str_detect(b, "3\\-") & !str_detect(b, "3X")) {
    b = paste0(b,"3X3")
  } 
  

  b1 = str_split_fixed(b, "\\;", 3)[,1]
  b2 = str_split_fixed(b, "\\;", 3)[,2]
  b3 = str_split_fixed(b, "\\;", 3)[,3]
  
  # baserunner who do not get thrown out
  c1 = str_split_fixed(b1, "\\-", 2)
  c2 = str_split_fixed(b2, "\\-", 2)
  c3 = str_split_fixed(b3, "\\-", 2)
  
  # baserunner who are attempted to get thrown out
  d1 = str_split_fixed(b1, "X", 2)
  d2 = str_split_fixed(b2, "X", 2)
  d3 = str_split_fixed(b3, "X", 2)
  
  # remove 1st baserunner from starting base
  mo1 = ifelse(c1[,1] == "1" | d1[,1] == "1", 0, mo1)
  mo2 = ifelse(c1[,1] == "2" | d1[,1] == "2", 0, mo2)
  mo3 = ifelse(c1[,1] == "3" | d1[,1] == "3", 0, mo3)
  # add 1st baserunner to destination base
  mo1 = ifelse(c1[,2] == "1", 1, mo1)
  mo2 = ifelse(c1[,2] == "2", 1, mo2)
  mo3 = ifelse(c1[,2] == "3", 1, mo3)
  # remove 2nd baserunner from starting base
  mo1 = ifelse(c2[,1] == "1" | d2[,1] == "1", 0, mo1)
  mo2 = ifelse(c2[,1] == "2" | d2[,1] == "2", 0, mo2)
  mo3 = ifelse(c2[,1] == "3" | d2[,1] == "3", 0, mo3)
  # add 2nd baserunner to destination base
  mo1 = ifelse(c2[,2] == "1", 1, mo1)
  mo2 = ifelse(c2[,2] == "2", 1, mo2)
  mo3 = ifelse(c2[,2] == "3", 1, mo3)
  # remove 3rd baserunner from starting base
  mo1 = ifelse(c3[,1] == "1" | d3[,1] == "1", 0, mo1)
  mo2 = ifelse(c3[,1] == "2" | d3[,1] == "2", 0, mo2)
  mo3 = ifelse(c3[,1] == "3" | d3[,1] == "3", 0, mo3)
  # add 3rd baserunner to destination base
  mo1 = ifelse(c3[,2] == "1", 1, mo1)
  mo2 = ifelse(c3[,2] == "2", 1, mo2)
  mo3 = ifelse(c3[,2] == "3", 1, mo3)
  
  c(mo1,mo2,mo3)
}

get_main_base_event <- function(event_tx) {
  a = str_split_fixed(event_tx, "\\.", 2)[,1]
  b = str_split_fixed(event_tx, "\\.", 2)[,2]
  #b = str_remove_all(b0, "(\\(.+?\\))+") # remove the errors
  # if the baserunners involve the batter, ignore this function!
  if (str_detect(b, "B\\-") | str_detect(b, "BX")) {
    return(0)
  }
  
  mo1 = (str_detect(a, "^S") & !str_detect(a, "^SB")) | 
    str_detect(a, "^W") | str_detect(a, "^IW") |
    str_detect(a, "^WP") | str_detect(a, "^PB") | str_detect(a, "^HP") |
    str_detect(a, "^E") | str_detect(a, "^FC") 
  
  mo2 = str_detect(a, "^D") & !str_detect(a, "^DI")
  
  mo3 = str_detect(a, "^T")
  
  ifelse(mo1, 1, ifelse(mo2, 2, ifelse(mo3, 3, 0)))
}

E2 <- E1 %>% group_by(GAME_ID,INNING,BAT_HOME_IND) %>% 
  group_modify(~ perform_base_progressions(.x)) %>%
  mutate(MAN_ON_1ST = lag(MAN_ON_1ST, default=0),
         MAN_ON_2ND = lag(MAN_ON_2ND, default=0),
         MAN_ON_3RD = lag(MAN_ON_3RD, default=0)) %>%
  ungroup() %>% mutate(
    BASE_STATE = paste0(as.character(MAN_ON_1ST),as.character(MAN_ON_2ND),as.character(MAN_ON_3RD))
  )
#View(E2)

# # remove EVENT_ER_CT, EVENT_RBI_CT (to be fixed later)... notice unique(E1$EVENT_ER_CT) has a -1...
# E3 <- E2 %>% select(-c(EVENT_ER_CT, EVENT_RBI_CT))

##############################

R = E2
write_csv(R, output_filename)

##############################

# checks...
# E1$EVENT_TX[str_detect(E1$EVENT_TX, "^HP")]
# E1$EVENT_TX[!str_detect(E1$EVENT_TX, "^[0-9]") & 
#               !str_detect(E1$EVENT_TX, "^K") &
#               !str_detect(E1$EVENT_TX, "^S[0-9]") &
#               !str_detect(E1$EVENT_TX, "^D[0-9]") &
#               !str_detect(E1$EVENT_TX, "^T[0-9]") &
#               !str_detect(E1$EVENT_TX, "^W") &
#               !str_detect(E1$EVENT_TX, "^HR")]
# View(E1[str_detect(E1$EVENT_TX,"E"),])
# View(E1[str_detect(str_split_fixed(E1$EVENT_TX, "\\.", 2)[,1],"E"),])
#View(E2[str_detect(E2$EVENT_TX, "X"),])
#E2 %>% filter(GAME_ID=="ANA201904040",INNING==3,BAT_HOME_IND==0)
#E2 %>% filter(GAME_ID=="ANA201905050",INNING==3,BAT_HOME_IND==1)
#E2 %>% filter(GAME_ID=="ANA201905180",INNING==6,BAT_HOME_IND==0)
#E2 %>% filter(GAME_ID=="ANA201906050",INNING==8,BAT_HOME_IND==0)
# View(E2[str_detect(E2$EVENT_TX, "FC"),])
# E2 %>% filter(GAME_ID=="ANA201907250",INNING==4,BAT_HOME_IND==0) ####
# E2 %>% filter(GAME_ID=="ANA201906280",INNING==8,BAT_HOME_IND==1)
# E2 %>% filter(GAME_ID=="ANA201906290",INNING==4,BAT_HOME_IND==0)
# E2 %>% filter(GAME_ID=="ANA201907180",INNING==6,BAT_HOME_IND==1)
# View(E2[str_detect(E2$EVENT_TX, "SB"),])
# E2 %>% filter(GAME_ID=="ANA201904180",INNING==4,BAT_HOME_IND==1)
# E2 %>% filter(GAME_ID=="ANA201904060",INNING==8,BAT_HOME_IND==1)
# E2 %>% filter(GAME_ID=="ANA201904200",INNING==9,BAT_HOME_IND==0)
# View(E2[str_detect(E2$EVENT_TX, "X") & str_detect(E2$EVENT_TX, "E"),])
# E2 %>% filter(GAME_ID=="ANA201907250",INNING==4,BAT_HOME_IND==0)
# View(E2[str_detect(E2$EVENT_TX, "CS"),])
# E2 %>% filter(GAME_ID=="ANA201904070",INNING==1,BAT_HOME_IND==0)
# E2 %>% filter(GAME_ID=="ANA201904080",INNING==3,BAT_HOME_IND==1)
# E2 %>% filter(GAME_ID=="ANA201904100",INNING==2,BAT_HOME_IND==0)
# E2 %>% filter(GAME_ID=="ANA201908300",INNING==14,BAT_HOME_IND==0)
# E2 %>% filter(GAME_ID=="ARI201904130",INNING==7,BAT_HOME_IND==1)
# E2 %>% filter(GAME_ID=="ANA201906090",INNING==5,BAT_HOME_IND==0)
# View(E2[str_detect(E2$EVENT_TX, "PO"),])
# E2 %>% filter(GAME_ID=="ANA201904060",INNING==7,BAT_HOME_IND==1)
# E2 %>% filter(GAME_ID=="ANA201907250",INNING==15,BAT_HOME_IND==0)
# E2 %>% filter(GAME_ID=="ANA201906260",INNING==8,BAT_HOME_IND==1)


#######################################################
########## MERGE BASE_STATE WITH BIG DATASET ##########
#######################################################

library(tidyverse)
input_filename0 = "../data/retro_base_states_1990-2020c.csv"
input_filename = "../data/retro_final_PA_1990-2020b.csv"
output_filename = "../data/retro_final_PA_1990-2020c.csv"
B <- R #read_csv(input_filename0) #R
C <- read_csv(input_filename)

C$BASE_STATE = B$BASE_STATE
C = C %>% select(-c(EVENT_ER_CT, EVENT_RBI_CT)) # slight errors discovered in these columns (-1's)
#View(C %>% filter(row_number() <= 200) %>% select(GAME_ID,INNING,BAT_HOME_IND,EVENT_TX,BASE_STATE))

write_csv(C, output_filename)

