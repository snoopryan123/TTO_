library(tidyverse)
library(stringr)

##################################################################################
################ Aggregate the games from 1990-2020 (level 1 data) ###############
##################################################################################

IsDate <- function(date) {
  d1 = as.character(try(as.Date(date, "%Y-%m-%d"), silent = TRUE))
  d2 = as.character(try(as.Date(date, "%Y/%m/%d"), silent = TRUE))
  e1 = !is.na(d1)
  e2 = !is.na(d2)
  return(e1|e2)
}
date <- c("1991-04-08", "2020-12-31", "foo", "2021-31-12", "1992/04/06")
IsDate(date)

################ 

D <- tibble()
# the warnings from 1990 - 2020 are fine...
for (year in 1990:2020) {
  print(year)
  curr <- read_csv(str_glue("retro1_PA_{year}.csv")) 
  curr <- curr %>% select(!c(starttime, sky))
  if (year %in% c(1992)) {
    # Error: Can't combine `..1$date` <date> and `..2$date` <character>.
    # For SOME rows, site and date are switched !!
    curr <- curr %>% mutate(temp = ifelse(IsDate(date), date, site), # will become the date column
                            site = ifelse(IsDate(date), site, date),
                            date = as.Date(temp),
                            site = as.character(site)) %>%
                     select(!c(temp))
  }
  if (year %in% c(1999)) {
    # Error: Can't combine `..1$site` <character> and `..2$site` <date>.
    # For ALL rows, site and date are switched !!
    curr <- curr %>% mutate(temp = date, # will become the site column
                            date = site,
                            site = temp) %>%
                     select(!c(temp))
  }
  D <- bind_rows(D, curr)
}


write_csv(D, "retro1_PA_1990-2020.csv")


# curr0 <- read_csv(str_glue("retro1_PA_1999.csv")) 
# curr1 <- read_csv(str_glue("retro1_PA_1992.csv")) 
# View(curr0)
# View(curr1)










