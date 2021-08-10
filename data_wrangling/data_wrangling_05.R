library(tidyverse)
library(stringr)

# create new columns
#######################################################
# EVENT_OUTS_CT === number of outs recorded at that event [*****]
# OUTS_CT === number of outs at that time [*****]
#######################################################

################################
########### THE CODE ###########
################################

input_filename = "retro04_PA_1990-2020.csv"
output_filename = "retro05_PA_1990-2020.csv"
D <- read_csv(input_filename)
D00 <- D %>% filter(YEAR %in% 1996:1996)
  
################################

compute_A <- function(a) {
    x1 = str_detect(a, "^C/E") | str_detect(a, "^S") | str_detect(a, "^D") | str_detect(a, "^T") | str_detect(a, "^E") | 
         str_detect(a, "^FC") | # ignore fielder's choice. looks at the baserunner activity
         str_detect(a, "^FLE") | str_detect(a, "^HP") | str_detect(a, "^HR") | str_detect(a, "^H") |
         str_detect(a, "^NP") | str_detect(a, "^W\\+SB") | str_detect(a, "^IW\\\\+SB") | str_detect(a, "^I\\+SB") |
         str_detect(a, "^W\\+PB") | str_detect(a, "^IW\\+PB") | str_detect(a, "^I\\+PB") | 
         str_detect(a, "^W\\+WP") | str_detect(a, "^IW\\+WP") | str_detect(a, "^I\\+WP") | 
         str_detect(a, "^W\\+E") | str_detect(a, "^IW\\+E") | str_detect(a, "^I\\+E") | 
         str_detect(a, "^W\\+OA") | str_detect(a, "^BK") | str_detect(a, "DI") | str_detect(a, "^OA") |
         str_detect(a, "^PB") | str_detect(a, "^WP") | str_detect(a, "^SB") |
         str_detect(a, "^K\\+E") | str_detect(a, "^K[0-9]*\\+OA")  # sometimes 0, sometimes 1 --> look at baserunner advances
         # return(0)
    
    x2 = (str_detect(a, "^W") & !str_detect(a, "^W\\+")) |
         (str_detect(a, "^IW") & !str_detect(a, "^IW\\+")) | 
         (str_detect(a, "^I") & !str_detect(a, "^I\\+")) 
         # return(0)
    
    x3 = str_detect(a, "^W\\+CS") | str_detect(a, "^IW\\+CS") | str_detect(a, "^I\\+CS") | 
         str_detect(a, "^W\\+PO") | str_detect(a, "^IW\\+PO") | str_detect(a, "^I\\+PO")
         # return(1)
    
    R = ifelse(x1 | x2, 0,
        ifelse(x3, 1, 
        NA # forgot a case?
        ))
  
    return(R)
}
#X = result %>% filter(str_detect(EVENT_TX, "^K\\+PO")) %>% select(EVENT_TX,EVENT_OUTS_CT)
#View(X)
compute_K <- function(a,b) {
  BBB = str_detect(b, "B-") | str_detect(b, "BX") 
  x4 = str_detect(a, "^K[0-9]*\\+PB") | str_detect(a, "^K[0-9]*\\+WP") | str_detect(a, "^K[0-9]*\\+SB") |
       str_detect(a, "^K\\/FO") 
  xxx.b = str_count(b, "BX") - str_count(b, "BX[123H]*\\([0-9]*E") #str_count(b, "B-") - str_count(b, "B-[123H]*\\([0-9]*E") +
  xxx.nob = str_count(b, "[123H]X[123BH]") - str_count(b, "[123H]X[123BH]\\([0-9]*E")
  
  x5 = str_detect(a, "^K[0-9]*\\+CS") | str_detect(a, "^K[0-9]*\\+PO")
  x6 = str_detect(a, "^K") & !x4 & !x5
  x4 = x4 | x6
  
  AAA = str_detect(a, "\\([0-9]*E")

  R = ifelse(x4 & BBB, 0 + xxx.b + xxx.nob,
      ifelse(x4 & !BBB, 1 + xxx.nob, 
      ifelse(x5 & AAA, 1 + xxx.nob,
      ifelse(x5 & !AAA, 2, 
             NA))))
  
  return(R)
 
  # # return(0)
  # 
  # x6 = str_detect(a, "^K[0-9]*\\+CS") | str_detect(a, "^K[0-9]*\\+PO")
  # # return(2)
  # 
  # x7 = str_detect(a, "^K") & !x4 & !x5 & !x6
  # # return(1)
  # 
  # x8 = !x4 & !x5 & !x6 & !x7
}

################################

{
    # STILL a problem with DUPLICATE ROWS somehow...
    # ex. see inning 6 Bryce Harper HR (shows up twice) in
    #View(E %>% filter(GAME_ID == "BOS202008180") %>% select(GAME_ID, INNING, BAT_HOME_IND, BAT_ID, PIT_ID, PITCH_SEQ_TX, EVENT_TX, BATTER_SEQ_NUM))
    #View(E %>% filter(GAME_ID == "BOS202008180"))
    D0 = D00 %>% distinct(across(c(GAME_ID, INNING, BAT_HOME_IND, BAT_ID, PIT_ID, PITCH_SEQ_TX, EVENT_TX, BATTER_SEQ_NUM)), .keep_all = TRUE)
    #View(E0 %>% filter(GAME_ID == "BOS202008180") %>% select(GAME_ID, INNING, BAT_HOME_IND, BAT_ID, PIT_ID, PITCH_SEQ_TX, EVENT_TX, BATTER_SEQ_NUM))
    
    
    # EVENT_OUTS_CT === number of outs recorded at that event
    A0 = D0 %>% mutate(
      ################################
      a = str_split_fixed(EVENT_TX, "\\.", 2)[,1], # string, description of the basic play
      b = str_split_fixed(EVENT_TX, "\\.", 2)[,2], # string, the advancement of any runners
      starts.with.num = str_count(a, "^[0-9]"),
      num.outs.startsWithNum = str_count(a, "^[0-9]*") - str_count(a, "^[0-9]*E"), # 63/G --> 1 out, 6E3/G --> 0 outs
      num.outs.cs = str_count(a, "CS[23H]") - str_count(a, "CS[23H]\\([0-9]*E"), # CS2(E1/TH).3-H(NR);1-3 --> 0 outs
      num.outs.po = ifelse(str_detect(a, "^POCS"), 
                           str_count(a, "POCS[H123]") - str_count(a, "POCS[H123]\\([0-9]*E"),
                           str_count(a, "PO[H123]") - str_count(a, "PO[H123]\\([0-9]*E")),
      A = ifelse(starts.with.num, num.outs.startsWithNum,
          ifelse(str_detect(a, "CS") & !str_detect(a, "^K+CS"), num.outs.cs, # str_detect(a, "^CS")
          ifelse(str_detect(a, "PO") & !str_detect(a, "^K+PO"), num.outs.po, # includes "POCS" # str_detect(a, "^PO")
                 compute_A(a)  
      ))),
      ################################
      # S9/L.2-3;1X3(936) --> 1 baserunner out
      # FC4.1X2(4E6);B-1 --> 0 baserunner outs
      # FC6/G.2XH(NR)(6E5)(UR);B-2\n --> 0 outs (remove the (NR), (UR))   to get FC6/G.2XH(6E5);B-2
      # D9/G+.1-H;BX3(E9)(95/TH) --> 1 out   WRONG???
      # S8/G6M.1XH(E8)(862) --> 1 out
      # S6/L6D.2-H;BXH(TH)(E2/TH)(8E2)(NR)(UR) --> 0 outs
      # S9/F9S.2-H;BX3(E9/THH)(13) --> 1 out
      b = str_remove_all(b, "\\(NR\\)"),
      b = str_remove_all(b, "\\(UR\\)"),
      b = str_remove_all(b, "THH"),
      b = str_remove_all(b, "TH"),
      b = str_remove_all(b, "\\/"),
      b = str_remove_all(b, "\\(\\)"),
      num.baserunner.outs = str_count(b, "[123BH]X[123BH]") - 
                            str_count(b, "[123BH]X[123BH]\\([0-9]*E") +
                            str_count(b, "[123BH]X[123BH]\\([0-9]*E[0-9]*\\)\\([0-9]*\\)"),
      B = num.baserunner.outs,
      ################################
      dp = str_detect(EVENT_TX, "DP"),
      tp = str_detect(EVENT_TX, "TP"),
      K = compute_K(a,b),
      EVENT_OUTS_CT = ifelse(dp, 2, ifelse(tp, 3, ifelse(is.na(K), A+B, K)))
    ) %>% select( c(names(D0),EVENT_OUTS_CT) )
    D1 = left_join(D0, A0)
    print("D1")  
    
    ################################
    
    # OUTS_CT === number of outs PRIOR TO the play
    A1 = D1 %>% group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
                mutate(OUTS_CT_after = cumsum(EVENT_OUTS_CT)) %>%
                ungroup() %>%
                mutate(OUTS_CT = OUTS_CT_after - EVENT_OUTS_CT) %>%
                select(!c(OUTS_CT_after)) 
    D2 = left_join(D1, A1)
    # rm(A2)
    # rm(D1)
    print("D2")
    
    ################################
    
    result = D2
    #write_csv(result, output_filename)
}

########################################################
########### SANITY CHECKS FOR EVENT_OUTS_CT ############
########################################################

{
  # outs check: make sue all OUTS_CT are in c(0,1,2)
  # outs check: make sure the inning ends with 3 outs (and drop the last inning)
  # CHECK this tibble is empty
  t1 = result %>% filter(OUTS_CT >= 3) %>%
        select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT)
  t2 = result %>% group_by(GAME_ID) %>% filter(INNING < max(INNING)) %>% ungroup() %>%
    group_by(GAME_ID, BAT_HOME_IND, INNING) %>% slice_tail() %>% 
    filter(replace_na(EVENT_OUTS_CT + OUTS_CT != 3, TRUE)) %>%  ungroup() %>%
    select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT)
  t3 = bind_rows(t1,t2) %>% distinct()
  View(t3)
}

# specific game and inning check
game = "LAN199608200" 
inning = 3
View(result %>% filter(GAME_ID == game, INNING == inning) %>%
       select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT))

# # specific full game check
# game = "WAS202009270"
# View(result %>% filter(GAME_ID == game) %>%
#        select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT))








