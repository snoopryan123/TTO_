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

################################

compute_A <- function(a,b) {
    x1 = str_detect(a, "^C/E") | str_detect(a, "^S") | str_detect(a, "^D") | str_detect(a, "^T") | str_detect(a, "^E") | 
         str_detect(a, "^FC") | # ignore fielder's choice. looks at the baserunner activity
         str_detect(a, "^FLE") | str_detect(a, "^HP") | str_detect(a, "^HR") | str_detect(a, "^H") |
         str_detect(a, "^K+E") | str_detect(a, "^K+OA") | # sometimes 0, sometimes 1 --> look at baserunner advances
         str_detect(a, "^NP") | str_detect(a, "^W+SB") | str_detect(a, "^IW+SB") | str_detect(a, "^I+SB") |
         str_detect(a, "^W+PB") | str_detect(a, "^IW+PB") | str_detect(a, "^I+PB") | 
         str_detect(a, "^W+WP") | str_detect(a, "^IW+WP") | str_detect(a, "^I+WP") | 
         str_detect(a, "^W+E") | str_detect(a, "^IW+E") | str_detect(a, "^I+E") | 
         str_detect(a, "^W+OA") | str_detect(a, "^BK") | str_detect(a, "DI") | str_detect(a, "^OA") |
         str_detect(a, "^PB") | str_detect(a, "^WP") | str_detect(a, "^SB") # return(0)
    
    x2 = (str_detect(a, "^W") & !str_detect(a, "^W+")) |
         (str_detect(a, "^IW") & !str_detect(a, "^IW+")) | 
         (str_detect(a, "^I") & !str_detect(a, "^I+")) # return(0)
    
    x3 = str_detect(a, "^W+CS") | str_detect(a, "^IW+CS") | str_detect(a, "^I+CS") | 
         str_detect(a, "^W+PO") | str_detect(a, "^IW+PO") | str_detect(a, "^I+PO") |
         str_detect(a, "^K+SB") # return(1)
    
    x4 = str_detect(a, "^K+CS") | str_detect(a, "^K+PO") # return(2) # guaranteed double play
  
    # sometimes 0, sometimes 1 --> look at baserunner advances
    #K+PB.B-1 --> 0 outs
    #K+PB.2-3;1-2 --> 1 out
    x5 = str_detect(a, "^K+PB") & str_detect(b, "B-") # return(0)
    x6 = str_detect(a, "^K+PB") & !str_detect(b, "B-") # return(1)
  
    #K+WP.1-2 --> 1 out
    #K+WP.B-1 --> 0 outs
    x7 = str_detect(a, "^K+WP") & str_detect(b, "B-") # return(0)
    x8 = str_detect(a, "^K+WP") & !str_detect(b, "B-") # return(1)
    
    # sometimes 0, sometimes 1 --> look at baserunner advances
    # K.BX1(2E3) --> 0 outs
    # K.BX1 --> 1 outs
    # K --> 1 out
    x9 = str_detect(a, "^K") & !str_detect(a, "^K+") & str_detect(b, "B") # return(0)
    x10 = str_detect(a, "^K") & !str_detect(a, "^K+") & !str_detect(b, "B") # return(1)
    
    R = ifelse(x1 | x2 | x5 | x7 | x9, 0,
        ifelse(x3 | x6 | x8 | x10, 1,
        ifelse(x4, 2, 
          0 # forgot a case?
        )))
  
    return(R)
}
  
################################

# EVENT_OUTS_CT === number of outs recorded at that event
D1 = D %>% mutate(
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
             ifelse(str_detect(a, "^CS"), num.outs.cs,
                    ifelse(str_detect(a, "^PO"), num.outs.po, # includes "POCS"
                           compute_A(a,b)  
                    ))),
  #A = compute_A(a),
  ################################
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
print("D1")  


################################

# OUTS_CT === number of outs PRIOR TO the play
D2 = D1 %>% group_by(GAME_ID, BAT_HOME_IND, INNING) %>%
            mutate(OUTS_CT_after = cumsum(EVENT_OUTS_CT)) %>%
            ungroup() %>%
            mutate(OUTS_CT = OUTS_CT_after - EVENT_OUTS_CT) %>%
            select(!c(OUTS_CT_after))
print("D2")

################################

result = D2
#write_csv(result, output_filename)
E = result



########################################################################


########################################################
########### SANITY CHECKS FOR EVENT_OUTS_CT ############
########################################################

{
  # outs check: make sue all OUTS_CT are in c(0,1,2)
  # CHECK this tibble is empty
  View(E %>% filter(OUTS_CT >= 3) %>%
        select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT))

  # outs check: make sure the inning ends with 3 outs (and drop the last inning)
  # CHECK this tibble is empty
  View(E %>% group_by(GAME_ID, BAT_HOME_IND) %>% filter(INNING < max(INNING)) %>% group_by(INNING) %>%
         slice_tail() %>% filter(EVENT_OUTS_CT + OUTS_CT != 3) %>% ungroup() %>% ungroup() %>%
         select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT))

  # specific game and inning check
  game = "CHA201208260"
  inning = 7
  View(E %>% filter(GAME_ID == game, INNING == inning) %>%
         select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT))

  # specific full game check
  game = "BOS201207310"
  View(E %>% filter(GAME_ID == game) %>%
         select(GAME_ID, BAT_HOME_IND, INNING, EVENT_TX, EVENT_OUTS_CT, OUTS_CT))
}







