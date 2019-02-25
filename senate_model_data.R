## Senate Model Data Prep


source("C:/Users/user/Dropbox (PUSA)/priorities/src/midterms/automation/functions.R")
setwd("C:/Users/user/Dropbox (PUSA)/priorities")

library(ggmcmc)
library(rjags)

# data for state priors ---------------------------------------------------

elex <- read_csv("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-16/master/2016_US_County_Level_Presidential_Results.csv") %>% select(-1)

e_2012 <- read.csv("state_2012_results.csv") %>%
  # rename(state = ï..state) %>% 
  mutate(twoway_12 = .01*(MOV + 100)/2)

e_16 <- elex %>% 
  group_by(state_abbr) %>% 
  summarise(twoway_16 = sum(votes_dem)/(sum(votes_dem + votes_gop))) %>% 
  select(state_abbr, twoway_16) %>% 
  rbind(data.frame(state_abbr = c("MS-Hyde-Smith", "MS-Wicker"), twoway_16 = c(.417, .417))) %>% 
  rbind(data.frame(state_abbr = c("MN-Smith", "MN-Klobuchar"), twoway_16 = c(.508, .508))) %>% 
  
  merge(e_2012, by.x = "state_abbr", by.y = "state", all.x = T) %>% 
  mutate(state_lean = case_when(twoway_12 %>% is.na == F ~ (twoway_16 + twoway_12)/2 -.5, 
                                TRUE ~ twoway_16 - .5)) %>% 
  select(-MOV)


# data for pollster priors ------------------------------------------------

pollsters <- read.csv("pollster_priors_condensed_all.csv") %>% as.tibble
#p %>% filter(bias %>% is.na() == F) %>% write.csv("pollster_priors_condensed.csv")
## adding var upper bound - 2x the median in Nick's work (switched to 40x for the model) (nick's distribution seemed very bimodal which is why this is big)
pollster_info <- pollsters %>% 
  mutate(pollster =  pollster %>% str_remove("[*]") %>% str_replace_all("[^[:alnum:]]", "_") %>% tolower) %>% 
  filter(bias %>% is.na == F) %>% 
  mutate(bias = ifelse(bias > 100, NA, bias),
         var = 40*0.00014,
         var_upper = ifelse(bias %>% is.na == T, .1, var),
         nodata = ifelse(bias %>% is.na == T, 1, 0),
         bias = ifelse(bias %>% is.na == T, 0, bias),
         bias = bias/100,
         var_lower = 0) %>% 
  select(-var) 


# data prep ---------------------------------------------------------------

dat <- read.csv("senate_polls_538_all.csv") %>% 
  rename(state = ï..state,
         pollster_raw = poll) %>%
  mutate(end_date = date %>% as.character() %>% str_remove(".* - ") %>% paste0("/2018") %>% mdy(),
         start_date = date %>% as.character() %>% str_remove(" - .*") %>% paste0("/2018") %>% mdy(),
         n_size = str_remove(sample, " .*"),
         pollster =  pollster_raw %>% str_remove("[*]") %>% str_replace_all("[^[:alnum:]]", "_") %>% str_replace_all("[^[:alnum:]]", "_") %>% tolower,
         univ = sample %>% str_remove(n_size) %>% str_remove(" ") %>% as.factor(),
         twoway = dem/(dem+rep),
         week = (mdy("11/06/2018") - end_date)/7,
         week = week %>% as.numeric %>% floor,
         month = (mdy("11/06/2018") - end_date)/30, 
         month = month %>% as.numeric %>% ceiling) %>% 
  mutate(n_size = n_size %>% as.numeric()) %>% 
  filter(n_size %>% is.na() == F & 
           end_date < mdy("11/08/2018") &
           univ != "") %>% 
  filter(state != "CA") %>% 
  mutate(pollster_num = as.numeric(as.factor(as.character(pollster))),
         univ_num = as.numeric(as.factor(as.character(univ))),
         state_num = as.numeric(as.factor(as.character(state))),
         prec = 1 / ((twoway * (1 - twoway)) / n_size),
         week_adj = -1 * (week - max(week)) + 1,
         month_adj = -1 * (month - max(month)) + 1) %>% 
  # select(-c(X:X.18)) %>% 
  as.tibble() %>% 
  left_join(pollster_info) %>% 
  left_join(e_16, by = c("state" = "state_abbr"))

dat %>% filter(week < 2) %>% 
  select(state, pollster, week, week_adj, end_date, date) %>% 
  print(n = Inf)

dat %>% write.csv("rawpolldata.csv")


#dat <- dat[1:542,]

## pushing to civis
# dat %>% write_civis("cody.polling_cleaned_1102")

