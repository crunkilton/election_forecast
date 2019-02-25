## Senate simulations

# s <- read_civis("cody.senate_model_output") %>% as.tibble()
# dat <- read_civis("cody.polling_cleaned") %>% as.tibble()

state_info <- dat %>% select(state, state_num) %>% unique %>% arrange(state_num) %>%
  mutate(dem_incumbent = case_when(state %in% c("AZ", "MS", "NE", "NV", 'TN', "TX", "UT", "WY", "MS-Hyde-Smith", "MS-Wicker", "WY") ~ 0,
                                   TRUE ~ 1))

# data and functions ------------------------------------------------------

## generating distributions to draw from

predictions <- s %>% 
  filter(str_detect(Parameter, "pred\\[") == T)  %>% 
  mutate(week_adj = Parameter %>% 
           str_remove("pred\\[") %>%
           str_remove(",.*") %>% 
           as.integer,
         state_num = Parameter %>% 
           str_remove("\\]") %>%
           str_remove(".*,") %>% 
           as.integer,
         week = -1*(week_adj - max(dat$week_adj)-0)) %>% 
  left_join(dat %>% select(state, state_num))%>% 
  group_by(state, week_adj, week) %>% 
  summarise(est = mean(value),
            sd = sd(value)) %>% 
  ungroup() %>% 
  filter(week_adj == 44)  %>% 
  arrange(state)


# or, read_civis("cody.senate_predictions") and filter for week_adj = 44.

## NOW, INCORPORATING TIME ERRORS

times <- s %>% 
  filter(str_detect(Parameter, "omega") == T | str_detect(Parameter, "mix") == T) %>% 
  group_by(Parameter) %>% 
  summarise(est = mean(value),
            sd = sd(value)) 

## NATIONWIDE SWINGS

nationwide_mix <- times %>% 
  filter(Parameter == "mix")

nationwide_omegas <- times %>% 
  filter(Parameter %>% str_detect("omega\\[") == T)



## state swings

state_mix <- times %>% 
  filter(str_detect(Parameter, "stateweek") == T) %>% 
  mutate(state_num = Parameter %>% 
           str_remove("\\]") %>%
           str_remove(".*,") %>% 
           str_remove(".*\\[") %>% 
           as.integer) %>% 
  left_join(dat %>% select(state, state_num) %>% unique) %>% 
  filter(str_detect(Parameter, "mix") == T)

state_omegas <- times %>% 
  filter(str_detect(Parameter, "stateweek") == T) %>% 
  mutate(state_num = Parameter %>% 
           str_remove("\\]") %>%
           str_remove(".*,") %>% 
           str_remove(".*\\[") %>% 
           as.integer) %>% 
  left_join(dat %>% select(state, state_num) %>% unique) %>% 
  filter(str_detect(Parameter, "omega") == T)

# ## regional stuff
 regions <- read.csv("state_regions_crosswalk.csv") %>% rename(state = ï..state) %>% arrange(region)
# 
# regions %>% write_civis("cody.senate_state_regions_crosswalk")

#regions <-  read_civis("cody.senate_state_regions_crosswalk")


# functions ---------------------------------------------------------------


states_baseline_sim <- function(n_sims) {
  
  list_holder <- NULL
  
  for (i in 1:length(predictions$state)) {
    list_holder[[i]] <- rnorm(n_sims, predictions$est[i], predictions$sd[i])
  }
  
  tidy_sims <- reshape2::melt(list_holder) %>% 
    left_join(dat %>% select(state, state_num) %>% unique, by = c("L1" = "state_num")) %>% 
    select(state, value) %>%
    group_by(state) %>% 
    mutate(simulation = seq(1, n(), 1)) %>% 
    as.tibble()
  
  return(tidy_sims)
  
}

list_holder <- NULL

nationwide_swings_sim <- function(n_sims) {

  big_or_small <- rbinom(n_sims, 1, nationwide_mix$est-1)
  
  swing <- NULL
  for (i in 1:length(big_or_small)) {
    if (big_or_small[i] == 1) {
      swing[i] <- rnorm(1, 0, nationwide_omegas$est[2]) * ifelse(rbinom(1, 1, .5) > 0, 1, -1)
  
    } else {
      swing[i] <- rnorm(1, 0, nationwide_omegas$est[1]) * ifelse(rbinom(1, 1, .5) > 0, 1, -1)
      
    }
    
  }
  
  
  tibble(simulation = seq(1, length(swing), 1), nationwide_swing = swing) %>% 
    return()
}

## STATE LEVEL SWINGS
list_holder <- list()
swing <- NULL

state_swings_sim <- function(n_sims) {
  
  for (j in 1:length(state_mix$state)) {
    
    big_or_small <- rbinom(n_sims, 1, state_mix$est[j]-1)
    
    swing <- NULL
    for (i in 1:length(big_or_small)) {
      if (big_or_small[i] == 1) {
        swing[i] <- rnorm(1, 0, state_omegas$est[2]) * ifelse(rbinom(1, 1, .5) > 0, 1, -1)
        
      } else {
        swing[i] <- rnorm(1, 0, state_omegas$est[1]) * ifelse(rbinom(1, 1, .5) > 0, 1, -1)
        
      }
      list_holder[[j]] <- swing
      
    }
  }
  
  reshape2::melt(list_holder) %>% 
    left_join(dat %>% select(state, state_num) %>% unique, by = c("L1" = "state_num")) %>% 
    select(state, value) %>% 
    rename(state_swing = value) %>% 
    group_by(state) %>% 
    mutate(simulation = seq(1, n(), 1)) %>% 
    return()
  
}

combine_time_errors <- function(n_sims) {
  
  s1 <- state_swings_sim(n_sims)
  n1 <- nationwide_swings_sim(n_sims)
  preds <- states_baseline_sim(n_sims)
  
  total_swing <- s1 %>% 
    left_join(n1) %>%
    left_join(preds) %>% 
    mutate(net_time_swing = state_swing+nationwide_swing,
           final_twoway = value + net_time_swing,
           win = ifelse(final_twoway > .5, 1, 0)) %>% 
    left_join(state_info) %>% 
    select(state, simulation, final_twoway, win, net_time_swing, dem_incumbent)
  
  return(total_swing)
  
}


# adding in nationwide and state-level polling errors  -----------------------------

statewide_errors <- list()
region_errors <- list()

vector_sim <- function(N) {
  sim_output <- NULL
  for (i in 1:N) {
    
    nationwide_error <- rnorm(1, 0, 2)/100 #2.614914)/100
    region_errors <- rnorm(regions$region %>% unique %>% length, nationwide_error, .01)
    #   
    region_errors2 <- reshape2::melt(region_errors) %>%
      mutate(region = regions$region %>% unique) 
    
    for (r in 1:nrow(region_errors2)) {
      this_sim <- region_errors2
      n_states_to_loop_over <- regions %>% filter(region == region_errors2$region[r]) %>% nrow
      statewide_errors[[r]] <- rnorm(n_states_to_loop_over, this_sim$value[r], .015) # this is the new state by state error
    }
    
    state_preds <- reshape2::melt(statewide_errors) %>% 
      rename(region_num = L1) %>%
      cbind(regions %>% arrange(region) %>% select(state, region)) %>% 
      select(value, state, region) %>% 
      mutate(simulation = rep(i, length(regions$state %>% unique))) ## CODY NOTE- IF WE ADD MORE STATES REMEMBER TO CHANGE THIS!!!
  
    if ("sim_output" %>% exists == F) {
      sim_output <- state_preds
    } else {
      sim_output <- rbind(sim_output, state_preds)
    }

  }
  sim_output <- sim_output %>% 
    rename(polling_error = value) %>% 
    as.tibble()
  return(sim_output)
}

# start <- Sys.time()
# vector_sim(1000)
# end <- Sys.time()
# end-start



# combining time + polling error shifts -----------------------------------

## 1 million simulations takes 5 hours to run - 100k takes 30 minutes
  
combine_all_errors <- function(n_sims) {
  s1 <- state_swings_sim(n_sims)
  n1 <- nationwide_swings_sim(n_sims)
  preds <- states_baseline_sim(n_sims)
  polling <- vector_sim(n_sims) 
  
total_swing <- s1 %>% 
    left_join(n1, by = "simulation") %>%
    left_join(preds, by = c("state", "simulation")) %>% 
  left_join(polling, by = c("state", "simulation")) %>% 
    mutate(net_time_swing = state_swing+nationwide_swing,
           final_twoway = value + net_time_swing + polling_error,
           win = ifelse(final_twoway > .5, 1, 0)) %>% 
    left_join(state_info, by = c("state")) %>% 
    select(state, simulation, final_twoway, win, net_time_swing, polling_error, dem_incumbent) %>% 
  ungroup() %>% 
  suppressWarnings() %>% 
  suppressMessages()
  
  return(total_swing)
  
}


# looking at results w/ polling errors ------------------------------------
# o <- read.csv("simulations.csv")
## what happens?
start <- Sys.time()
# o <- combine_all_errors(10000)
o <- combine_all_errors(100000)
end <- Sys.time()
end-start

#o %>% write.csv("simulations.csv")
#o %>% write_civis("cody.forecast_10k_sims_1102")

## are the polling errors reasonable here? 
o %>% 
  group_by(simulation) %>% 
  summarise(natl_swing = mean(polling_error),
            sd = sd(polling_error)) %>% 
  summarise(overall_natl_swing = mean(natl_swing),
            sd = sd(natl_swing))


## info for each state
sim_summary <- o %>% 
  group_by(state, dem_incumbent) %>% 
  summarise(mean_twoway = mean(final_twoway),
            sd_twoway = sd(final_twoway),
            win_pct = mean(win) * 100) %>% 
  ungroup()

sim_summary %>% arrange(win_pct) %>% print(n = Inf)

## win probability graph
sim_summary %>% 
  mutate(state = factor(state, levels = state[order(win_pct)])) %>% 
  ggplot(aes(x = state, y = win_pct, color = dem_incumbent)) +
  #geom_point() + 
  geom_dl(aes(label = state), method = list(dl.combine( "last.bumpup"), cex = 0.8)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  coord_flip()


## topline info
outcome_summary <- sim_summary %>% 
  summarise(mean_seats = sum(win_pct/100),
            squo_seats = sum(dem_incumbent),
            net_gain = mean_seats-squo_seats)

outcome_summary
## do we take back the senate?

o %>% 
  group_by(simulation) %>% 
  summarise(total_seats = 25 + sum(win)) %>% 
  mutate(win_senate = case_when(total_seats > 50 ~ 1,
                                TRUE ~ 0)) %>% 
  arrange(total_seats %>% desc) %>% 
  summarise(pct_win_senate = mean(win_senate)*100)

o %>% 
  group_by(simulation) %>% 
  summarise(seats = sum(win)+25) %>% 
  group_by(seats) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count)*100, 
         control = case_when(seats > 50 ~ "Democrats", 
                             TRUE ~ "Republicans")) %>% 
  ggplot(aes(x= seats, y = percent, fill = control)) + 
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  geom_col() +
  theme_bw()


## Tipping point state:

## number of dem seats not up for election: 23 + CA + HI = 25. need 26 to win

o %>% 
  group_by(simulation) %>% 
  arrange(final_twoway %>% desc) %>% 
  slice(26) %>% 
  ungroup %>% 
  group_by(state) %>% 
  summarise(count = n(),
            mean_twoway = mean(final_twoway),
            mean_winpct = mean(win)) %>% 
  mutate(pct_tipping_point = 100*count/sum(count)) %>% 
  arrange(count %>% desc)

## TN is mostly likely to be tipping point state

## if we only look at simulations where we did win the senate, 

wins <- o %>% 
  group_by(simulation) %>% 
  summarise(total_seats = 25 + sum(win)) %>% 
  mutate(win_senate = case_when(total_seats > 50 ~ 1,
                                TRUE ~ 0)) %>% 
  filter(win_senate == 1)

## the answer is the same. 

o %>% 
  filter(simulation %in% wins$simulation) %>% 
  group_by(simulation) %>% 
  arrange(final_twoway %>% desc) %>% 
  slice(26) %>% 
  ungroup %>% 
  group_by(state) %>% 
  summarise(count = n(),
            mean_twoway = mean(final_twoway),
            mean_winpct = mean(win)) %>% 
  arrange(count %>% desc)


# graph of results --------------------------------------------------------

## for week 1
o %>% 
  group_by(state) %>% 
  summarise(est = mean(final_twoway),
            sd = sd(final_twoway))  %>%
  arrange(est %>% desc) %>% 
  mutate(state = factor(state, levels = state[order(est)]),
         tipping_point = case_when(row_number() == 26 ~ 1,
                                   TRUE ~0) %>%
           as.factor()) %>% 
  ggplot(aes(x = state, y = est)) +
 # geom_dl(aes(label = state), method = list(dl.combine( "last.bumpup"), cex = 0.8)) +
  geom_point() + 
  geom_errorbar(aes(x = state, ymin = est-sd*2, ymax = est+sd*2), linetype = "dashed", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "statewide predictions week 1 - includes polling and time errors") +
  geom_hline(aes(yintercept = .5), color = "blue") +
  coord_flip()

## violin plot - uglier, rejected.
o %>% 
  ggplot(aes(x = state, y = final_twoway)) + 
  geom_violin() +
  scale_y_continuous(labels = scales::percent) +
  
  labs(title = "statewide predictions week 1 - includes polling and time errors") +
  geom_hline(aes(yintercept = .5, color = "50%")) +
  coord_flip()

# misc --------------------------------------------------------------------

## if we win TN, do we also win TX?

tn_wins <- o %>% 
  group_by(simulation) %>% 
  filter(state == "TN" & win == 1)

o %>% 
  filter(simulation %in% tn_wins$simulation) %>% 
  group_by(state) %>% 
  summarise(win_pct = mean(win)) %>% print(n = Inf)



# plot of win probability -------------------------------------------------

sim_summary %>% 
  mutate(state = factor(state, levels = state[order(win_pct)]),
         dem_incumbent = factor(dem_incumbent)) %>% 
  print(n = Inf) %>% 
  ggplot(aes(x = state, y = win_pct/100, color = dem_incumbent)) +
  scale_color_manual(values = c("red", "blue", "green")) + 
  #geom_point() + 
  geom_dl(aes(label = state), method = list(dl.combine( "last.bumpup"), cex = 0.8)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "Win Probability by State",
       x = "",
       y = "Win Percent") 
  
# one more table ----------------------------------------------------------

o %>% 
  group_by(state) %>% 
  summarise(twoway = mean(final_twoway)*100,
            sd = sd(final_twoway)*100,
            win_pct = mean(win)) %>% 
  mutate(lower = twoway - sd,
         upper = twoway + sd,
         range = paste0(lower %>% round(1), "% - ", upper %>% round(1), "%"),
         win_pct = paste0(win_pct*100, "%"),
         twoway_str = paste0(twoway %>% round(1), "%")) %>% 
  select(state, twoway, win_pct) %>% 
  arrange(abs(twoway - 50))


# ALL BELOW HERE IS OPTIONAL/WORKING/REJECTED -----------------------------



# looking at time-error results ------------------------------------------------------

o <- combine_time_errors(10000)
o
## info for each state
sim_summary <- o %>% 
  group_by(state, dem_incumbent) %>% 
  summarise(mean_twoway = mean(final_twoway),
            sd_twoway = sd(final_twoway),
            win_pct = mean(win) * 100) %>% 
  ungroup()

sim_summary 

## topline info
outcome_summary <- sim_summary %>% 
  summarise(mean_seats = sum(win_pct/100),
            squo_seats = sum(dem_incumbent),
            net_gain = mean_seats-squo_seats)

outcome_summary
## do we take back the senate?

o %>% 
  group_by(simulation) %>% 
  summarise(total_seats = 24 + sum(win)) %>% 
  mutate(win_senate = case_when(total_seats > 50 ~ 1,
                                TRUE ~ 0)) %>% 
  arrange(total_seats %>% desc) %>% 
  summarise(pct_win_senate = mean(win_senate)*100)



## Tipping point state:

## number of dem seats not up for election: 23 + CA + HI = 25. need 26 to win

o %>% 
  group_by(simulation) %>% 
  arrange(final_twoway %>% desc) %>% 
  slice(26) %>% 
  ungroup %>% 
  group_by(state) %>% 
  summarise(count = n(),
            mean_twoway = mean(final_twoway),
            mean_winpct = mean(win)) %>% 
  arrange(count %>% desc)

## TN is mostly likely to be tipping point state

## if we only look at simulations where we did win the senate, 

wins <- o %>% 
  group_by(simulation) %>% 
  summarise(total_seats = 25 + sum(win)) %>% 
  mutate(win_senate = case_when(total_seats > 50 ~ 1,
                                TRUE ~ 0)) %>% 
  filter(win_senate == 1)

## the answer is the same. 

o %>% 
  filter(simulation %in% wins$simulation) %>% 
  group_by(simulation) %>% 
  arrange(final_twoway %>% desc) %>% 
  slice(26) %>% 
  ungroup %>% 
  group_by(state) %>% 
  summarise(count = n(),
            mean_twoway = mean(final_twoway),
            mean_winpct = mean(win)) %>% 
  arrange(count %>% desc)


# old failed --------------------------------------------------------------


## states and regions


list_holder <- list()
nationwide_error <- NULL
region_list_holder <- NULL
region_errors <- NULL
statewide_errors <- NULL
state_preds <- NULL


polling_errors_sim <- function(n_sims) {
  
  for (i in 1:n_sims) {
    
    nationwide_errors <- rnorm(n_sims, 0, 2.614914)/100
    region_errors[[i]] <- rnorm(regions$region %>% unique %>% length, nationwide_errors[i], .01)
    #   
    region_errors2 <- reshape2::melt(region_errors) %>%
      rename(simulation = L1) %>%
      group_by(simulation) %>%
      mutate(region = regions$region %>% unique) 
    # %>%
    #   left_join(regions) %>%
    #   rename(regional_swing = value)
    
    for (r in 1:length(regions$region %>% unique)) {
      reg <- regions$region %>% unique
      this_sim <- region_errors2 %>% filter(simulation == i)
      n_states_to_loop_over <- regions %>% filter(region == reg[r]) %>% nrow
      statewide_errors[[r]] <- rnorm(n_states_to_loop_over, this_sim$value[r], .01) # this is the new state by state error
    }
    
    state_preds[[i]] <- reshape2::melt(statewide_errors) %>% 
      rename(region_num = L1) %>%
      cbind(regions %>% arrange(region) %>% select(state, region)) %>% 
      select(value, state, region)
    
  }
  compiled_errors <- reshape2::melt(state_preds, id.vars = c("state", "region"))%>% 
    rename(simulation = L1) %>% 
    select(state, region, value, simulation) %>% 
    rename(polling_error = value) %>% 
    as.tibble()
  
  return(compiled_errors)
  
}

start <- Sys.time()
polling_errors_sim(20) %>% 
  rbind(polling_errors_sim(20)) %>% 
  rbind(polling_errors_sim(20)) %>% 
  rbind(polling_errors_sim(20)) %>% 
  rbind(polling_errors_sim(20))

end <- Sys.time()

end-start

## looks like moving in smaller chunks makes this faster. Maybe I should write something to do everything at once for one simulation, then make that a function that repeats and rbinds itself



# lapply ------------------------------------------------------------------
set.seed(23)
r1 <- lapply(nationwide_errors, function(x) {rnorm(regions$region %>% unique %>% length, x, .01)}) 

reshape2::melt() %>%
  rename(simulation = L1) %>%
  group_by(simulation) %>%
  mutate(region = regions$region %>% unique)


lapply(r1, function(statewide_errors) {
  reg <- regions$region %>% unique
  this_sim <- region_errors2 %>% filter(simulation == i)
  n_states_to_loop_over <- regions %>% filter(region == reg[r]) %>% nrow
  statewide_errors[[r]] <- rnorm(n_states_to_loop_over, this_sim$value[r], .01) # this is the new state by state error 
})

# simulate once -----------------------------------------------------------

iteration <- runif(1, 1, length(o$simulation %>% unique)) %>% round

o %>% filter(simulation == iteration)

## to do- make a pretty map of this

o %>% write.csv("sims_1031.csv")
