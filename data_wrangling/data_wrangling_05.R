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


input_filename = "retro04_PA_2020.csv"
output_filename = "retro05_PA_2020.csv"
D <- read_csv(input_filename)




    
# EVENT_OUTS_CT === number of outs recorded at that event
D1 = D %>% mutate(
  num.outs.cs = str_count(EVENT_TX, "CS[23H]") - str_count(EVENT_TX, "CS[23H]\\([0-9]*E"), # CS2(E1/TH).3-H(NR);1-3 --> 0 outs
  num.outs.po = ifelse(str_detect(EVENT_TX, "^POCS"), 
                       str_count(EVENT_TX, "POCS[H123]") - str_count(EVENT_TX, "POCS[H123]\\([0-9]*E"),
                       str_count(EVENT_TX, "PO[H123]") - str_count(EVENT_TX, "PO[H123]\\([0-9]*E")),
  num.outs.startsWithNum = str_count(EVENT_TX, "^[0-9]*") - str_count(EVENT_TX, "^[0-9]*E"), # 63/G --> 1 out, 6E3/G --> 0 outs
  
  # S9/L.2-3;1X3(936) --> 1 baserunner out
  # FC4.1X2(4E6);B-1 --> 0 baserunner outs
  # FC6/G.2XH(NR)(6E5)(UR);B-2\n --> 0 outs (remove the (NR), (UR))
  starts.with.num = str_count(s.urnr, "^[0-9]"),
  s = str_split_fixed(EVENT_TX, "\\.", 2),
  a = s[1], # string, description of the basic play
  
  
  b = s[2], # string, the advancement of any runners
  b.nr = str_remove_all(b, "\\(NR\\)"),
  b.urnr = str_remove_all(b, "\\(UR\\)"),
  num.baserunner.outs = str_count(b.urnr, "[123BH]X[123BH]") - str_count(b.urnr, "[123BH]X[123BH]\\([0-9]*E"),
  B = num.baserunner.outs,
  dp = str_detect(EVENT_TX, "DP"),
  tp = str_detect(EVENT_TX, "TP"),
  EVENT_OUTS_CT = ifelse(dp, 2, ifelse(tp, 3, A+B))
)
    
    
    compute_event_outs_ct <- function(event_tx) {
      A = 0
      # set the value of A
      {
        if (starts.with.num(a)) {
          # 63/G --> 1 out
          # 6E3/G --> 0 outs
          A = num.outs.startsWithNum(a) 
        } else if (startsWith(a, "C/E")) {
          A = 0
        } else if (startsWith(a, "S")) {
          A = 0
        } else if (startsWith(a, "D")) {
          A = 0
        } else if (startsWith(a, "T")) {
          A = 0
        } else if (startsWith(a, "E")) {
          A = 0
        } else if (startsWith(a, "FC")) {
          # ignore fielder's choice. looks at the baserunner activity
          A = 0
        } else if (startsWith(a, "FLE")) {
          A = 0
        } else if (startsWith(a, "HP") | startsWith(a, "HR") | startsWith(a, "H")) {
          A = 0
        } else if (startsWith(a, "K") & !startsWith(a, "K+")) {
          # sometimes 0, sometimes 1 --> look at baserunner advances
          # K.BX1(2E3) --> 0 outs
          # K.BX1 --> 1 outs
          # K --> 1 out
          A = if (grepl("B", b, fixed=TRUE)) 0 else 1
        } else if (startsWith(a, "K+SB")) {
          A = 1
          return(1)
        } else if (startsWith(a, "K+CS")) {
          # guaranteed double play
          A = 2 
          return(2)
        }  else if (startsWith(a, "K+PO")) {
          # guaranteed double play
          A = 2
          return(2)
        } else if (startsWith(a, "K+E")) {
          A = 0
        } else if (startsWith(a, "K+PB")) {
          # sometimes 0, sometimes 1 --> look at baserunner advances
          #K+PB.B-1 --> 0 outs
          #K+PB.2-3;1-2 --> 1 out
          A = if (grepl("B-", b, fixed=TRUE)) 0 else 1
        } else if (startsWith(a, "K+WP")) {
          #K+WP.1-2 --> 1 out
          #K+WP.B-1 --> 0 outs
          A = if (grepl("B-", b, fixed=TRUE)) 0 else 1
        } else if (startsWith(a, "K+OA")) {
          # sometimes 0, sometimes 1 --> look at baserunner advances
          A = 0
        } else if (startsWith(a, "NP")) {
          A = 0
        } else if ( (startsWith(a, "W") & !startsWith(a, "W+")) |
                    (startsWith(a, "IW") & !startsWith(a, "IW+")) | 
                    (startsWith(a, "I") & !startsWith(a, "I+")) )  {
          A = 0
        } else if (startsWith(a, "W+SB") | startsWith(a, "IW+SB") | startsWith(a, "I+SB")) {
          A = 0
        } else if (startsWith(a, "W+CS") | startsWith(a, "IW+CS") | startsWith(a, "I+CS")) {
          A = 1
        } else if (startsWith(a, "W+PO") | startsWith(a, "IW+PO") | startsWith(a, "I+PO")) {
          A = 1
        } else if (startsWith(a, "W+PB") | startsWith(a, "IW+PB") | startsWith(a, "I+PB")) {
          A = 0
        } else if (startsWith(a, "W+WP") | startsWith(a, "IW+WP") | startsWith(a, "I+WP")) {
          A = 0
        } else if (startsWith(a, "W+E") | startsWith(a, "IW+E") | startsWith(a, "I+E")) {
          A = 0
        } else if (startsWith(a, "W+OA")) {
          A = 0
        } else if (startsWith(a, "BK")) {
          A = 0
        } else if (startsWith(a, "CS")) {
          A = num.outs.cs(a)
        } else if (grepl("DI", a, fixed=TRUE)) {
          A = 0
        } else if (startsWith(a, "OA")) {
          A = 0
          # need to look at b
        } else if (startsWith(a, "PB")) {
          A = 0
        } else if (startsWith(a, "WP")) {
          A = 0
        } else if (startsWith(a, "PO")) {
          # includes "POCS"
          A = num.outs.po(a)
        } else if (startsWith(a, "SB")) {
          A = 0
          return(0)
        } else {
          print("*****")
          print(a)
        }
      }
      
      
    }
    
    
      
    D7 = D6 %>% mutate(EVENT_OUTS_CT = sapply(EVENT_TX, compute_event_outs_ct))
    
    print("D7")
    ########################################################
    
   
  
   
     
   
    ########################################################
    
    # OUTS_CT === number of outs PRIOR TO the play
    D8 = D7 %>% group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
                mutate(OUTS_CT_after = cumsum(EVENT_OUTS_CT)) %>%
                ungroup() %>%
                mutate(OUTS_CT = OUTS_CT_after - EVENT_OUTS_CT) %>%
                select(!c(OUTS_CT_after))
    print("D8")
    
    result = D8
    #write_csv(result, output_filename)




########################################################################


########################################################
########### SANITY CHECKS FOR EVENT_OUTS_CT ############
########################################################

{
  # # outs check: make sue all OUTS_CT are in c(0,1,2)
  # # CHECK this tibble is empty
  # View(E %>% filter(OUTS_CT >= 3) %>%
  #       select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT))
  # 
  # # outs check: make sure the inning ends with 3 outs (and drop the last inning)
  # # CHECK this tibble is empty
  # View(E %>% group_by(GAME_ID, BAT_HOME_IND) %>% filter(INNING < max(INNING)) %>% group_by(INNING) %>%
  #        slice_tail() %>% filter(EVENT_OUTS_CT + OUTS_CT != 3) %>% ungroup() %>% ungroup() %>%
  #        select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT))
  #
  # # specific game and inning check
  # game = "CHA201208260"
  # inning = 7
  # View(E %>% filter(GAME_ID == game, INNING == inning) %>% 
  #        select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT))
  # 
  # # specific full game check
  # game = "BOS201207310"
  # View(E %>% filter(GAME_ID == game) %>% 
  #        select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT))
}







