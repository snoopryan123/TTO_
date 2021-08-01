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
  starts.with.num = str_count(EVENT_TX, "^[0-9]"),
  ################################
  a = str_split_fixed(EVENT_TX, "\\.", 2)[,1], # string, description of the basic play
  A = compute_A(a),
  ################################
  b = str_split_fixed(EVENT_TX, "\\.", 2)[,2], # string, the advancement of any runners
  # S9/L.2-3;1X3(936) --> 1 baserunner out
  # FC4.1X2(4E6);B-1 --> 0 baserunner outs
  # FC6/G.2XH(NR)(6E5)(UR);B-2\n --> 0 outs (remove the (NR), (UR))
  b.nr = str_remove_all(b, "\\(NR\\)"),
  b.urnr = str_remove_all(b.nr, "\\(UR\\)"),
  num.baserunner.outs = str_count(b.urnr, "[123BH]X[123BH]") - str_count(b.urnr, "[123BH]X[123BH]\\([0-9]*E"),
  B = num.baserunner.outs,
  ################################
  dp = str_detect(EVENT_TX, "DP"),
  tp = str_detect(EVENT_TX, "TP"),
  EVENT_OUTS_CT = ifelse(dp, 2, ifelse(tp, 3, A+B))
)
    
    
    compute_event_outs_ct <- function(EVENT_TX_0) {
      A = 0
      # set the value of A
      {
        if (starts.with.num(a)) {
          # 63/G --> 1 out
          # 6E3/G --> 0 outs
          A = num.outs.startsWithNum(a) 
        } else if (str_detect(a, "^C/E")) {
          A = 0
        } else if (str_detect(a, "^S")) {
          A = 0
        } else if (str_detect(a, "^D")) {
          A = 0
        } else if (str_detect(a, "^T")) {
          A = 0
        } else if (str_detect(a, "^E")) {
          A = 0
        } else if (str_detect(a, "^FC")) {
          # ignore fielder's choice. looks at the baserunner activity
          A = 0
        } else if (str_detect(a, "^FLE")) {
          A = 0
        } else if (str_detect(a, "^HP") | str_detect(a, "^HR") | str_detect(a, "^H")) {
          A = 0
        } else if (str_detect(a, "^K") & !str_detect(a, "^K+")) {
          # sometimes 0, sometimes 1 --> look at baserunner advances
          # K.BX1(2E3) --> 0 outs
          # K.BX1 --> 1 outs
          # K --> 1 out
          A = if (str_detect(b, "B")) 0 else 1
        } else if (str_detect(a, "^K+SB")) {
          A = 1
          return(1)
        } else if (str_detect(a, "^K+CS")) {
          # guaranteed double play
          A = 2 
          return(2)
        }  else if (str_detect(a, "^K+PO")) {
          # guaranteed double play
          A = 2
          return(2)
        } else if (str_detect(a, "^K+E")) {
          A = 0
        } else if (str_detect(a, "^K+PB")) {
          # sometimes 0, sometimes 1 --> look at baserunner advances
          #K+PB.B-1 --> 0 outs
          #K+PB.2-3;1-2 --> 1 out
          A = if (str_detect(b, "B-")) 0 else 1
        } else if (str_detect(a, "^K+WP")) {
          #K+WP.1-2 --> 1 out
          #K+WP.B-1 --> 0 outs
          A = if (str_detect(b, "B-", b)) 0 else 1
        } else if (str_detect(a, "^K+OA")) {
          # sometimes 0, sometimes 1 --> look at baserunner advances
          A = 0
        } else if (str_detect(a, "^NP")) {
          A = 0
        } else if ( (str_detect(a, "^W") & !str_detect(a, "^W+")) |
                    (str_detect(a, "^IW") & !str_detect(a, "^IW+")) | 
                    (str_detect(a, "^I") & !str_detect(a, "^I+")) )  {
          A = 0
        } else if (str_detect(a, "^W+SB") | str_detect(a, "^IW+SB") | str_detect(a, "^I+SB")) {
          A = 0
        } else if (str_detect(a, "^W+CS") | str_detect(a, "^IW+CS") | str_detect(a, "^I+CS")) {
          A = 1
        } else if (str_detect(a, "^W+PO") | str_detect(a, "^IW+PO") | str_detect(a, "^I+PO")) {
          A = 1
        } else if (str_detect(a, "^W+PB") | str_detect(a, "^IW+PB") | str_detect(a, "^I+PB")) {
          A = 0
        } else if (str_detect(a, "^W+WP") | str_detect(a, "^IW+WP") | str_detect(a, "^I+WP")) {
          A = 0
        } else if (str_detect(a, "^W+E") | str_detect(a, "^IW+E") | str_detect(a, "^I+E")) {
          A = 0
        } else if (str_detect(a, "^W+OA")) {
          A = 0
        } else if (str_detect(a, "^BK")) {
          A = 0
        } else if (str_detect(a, "^CS")) {
          A = num.outs.cs(a)
        } else if (str_detect(a, "DI")) {
          A = 0
        } else if (str_detect(a, "^OA")) {
          A = 0
          # need to look at b
        } else if (str_detect(a, "^PB")) {
          A = 0
        } else if (str_detect(a, "^WP")) {
          A = 0
        } else if (str_detect(a, "^PO")) {
          # includes "POCS"
          A = num.outs.po(a)
        } else if (str_detect(a, "^SB")) {
          A = 0
          return(0)
        } else {
          print("*****")
          print(a)
        }
      }
      
    }
  
     
   
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







