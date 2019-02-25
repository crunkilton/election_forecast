## senate model output and diagnostics

## look at what you are interested in looking at!


# twoway predictions ------------------------------------------------------


## for week 1
s %>% 
  group_by(Parameter) %>% 
  filter(str_detect(Parameter, "pred\\[44,") == T)  %>% 
  summarise(est = mean(value),
            sd = sd(value))  %>% 
  cbind(dat %>% select(state, state_num) %>% unique %>% arrange(state_num)) %>% 
  mutate(state = factor(state, levels = state[order(est)])) %>% 
  ggplot(aes(x = state, y = est)) +
  geom_point() +
  # geom_hline(aes(yintercept = 0, color = "green")) +
  geom_errorbar(aes(x = state, ymin = est-sd*2, ymax = est+sd*2), linetype = "dashed", color = "blue") +
  scale_y_continuous(labels = scales::percent) +
    labs(title = "statewide predictions week 1") +
  geom_hline(aes(yintercept = .5, color = "50%")) +
  coord_flip()


# change over time --------------------------------------------------------

## this is the code for the cody.senate_predictions file
p <- s %>% 
  group_by(Parameter) %>% 
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
  group_by(Parameter, state, week_adj, week) %>% 
  summarise(est = mean(value),
            sd = sd(value))


p %>% write.csv("predictions.csv")

#p %>% write_civis("cody.senate_predictions")
# p %>% write.csv("predictions.csv")


###
## testing
# prediction_data <- p
# raw_data <- dat
# state_code <- "IN"

state_plot <- function(prediction_data, raw_data, state_code) {
  
  state_plot_polls <- raw_data %>% filter(state == state_code & week_adj > 0)
  state_plot_mod <- prediction_data %>% filter(state == state_code & week_adj > 0)
  
  ggplot(data=state_plot_mod, aes(x=week, y=est)) + 
    geom_point(data=state_plot_polls, aes(x=week, y=twoway, size=sqrt(n_size), color = univ), alpha=0.4) + 
    geom_ribbon(aes(ymin=est-2*sd,ymax=est+2*sd), alpha = 0.5) +
    geom_line(color = "blue",size = 0.75) +
    theme_bw() +
    scale_x_reverse(name = "Weeks Before Election", limits= c(20, 0)) +
    coord_cartesian(ylim = c(min(0.46, min(state_plot_polls$twoway) - 0.01), max(.54, max(state_plot_polls$twoway) + 0.01))) + 
    scale_y_continuous(labels = scales::percent) +
    guides(size=F, color = F) + 
    geom_hline( aes(yintercept = 0.5), linetype="dashed") +
    labs(title = paste(state_plot_mod$state %>% unique %>% as.character, "Two-way Forecasted Vote Share"), y = "Democratic Two-way Vote Share")
}


uq_states <- dat %>% select(state) %>% unique

for (i in 1:length(uq_states$state)) {
  
  plot <- state_plot(p, dat, uq_states$state[i])
  print(plot)
}

state_plot(p, dat, "MS-Hyde-Smith")
state_plot(p, dat, "TX")
state_plot(p, dat, "FL")



# working -----------------------------------------------------------------
prediction_data <- p
raw_data <- dat
state_code = "FL"

state_plot <- function(prediction_data, raw_data, state_code) {
  
  state_plot_polls <- raw_data %>% filter(state == state_code & week_adj > 0)
  state_plot_mod <- prediction_data %>% filter(state == state_code & week_adj > 0)
  
  ggplot(data=state_plot_mod, aes(x=week, y=est)) + 
    geom_point(data=state_plot_polls, aes(x=week, y=twoway, size=sqrt(n_size), color = univ), alpha=0.4) + 
    geom_ribbon(aes(ymin=est-2*sd,ymax=est+2*sd), alpha = 0.5) +
    geom_line(color = "blue",size = 0.75) +
    theme_bw() +
    scale_x_reverse(name = "Weeks Before Election", limits= c(20, 0)) +
    coord_cartesian(ylim = c(min(0.46, min(state_plot_polls$twoway) - 0.01), max(.54, max(state_plot_polls$twoway) + 0.01))) + 
    scale_y_continuous(labels = scales::percent) +
    guides(size=F) + 
    geom_hline( aes(yintercept = 0.5), linetype="dashed") +
    labs(title = paste(state_plot_mod$state %>% unique %>% as.character, "Two-way Forecasted Vote Share"), y = "Democratic Two-way Vote Share",
         caption = "Note that this does not account for systematic polling error, which our simulations do account for.",
         color = "Universe")
  }

# convergence -------------------------------------------------------------


ggs_traceplot(s %>% filter(Parameter %in% c("xi_week[10]", "xi_week[42]", "xi_week[30]", "xi_week[40]")))

s %>% 
  filter(str_detect(Parameter, "xi\\[43,4\\]") == T) %>% 
  ggs_traceplot()

s %>% 
  filter(str_detect(Parameter, "pred\\[4") == T & str_detect(Parameter, ",4\\]") == T) %>% 
  # ggs_density()
  # ggs_autocorrelation(nLags= 100)
  ggs_traceplot()

s %>% 
  filter(str_detect(Parameter, "pred\\[4") == T & str_detect(Parameter, ",4\\]") == T) %>% 
  # ggs_density()
  # ggs_autocorrelation(nLags= 100)
  ggs_traceplot()

s %>% select(Parameter) %>% unique %>% filter(str_detect(Parameter, "xi_week") == T)


ggs_traceplot(s %>% filter(str_detect(Parameter, "theta") == T)) # convergence?

ggs_traceplot(s %>% filter(Parameter %in% c("gamma[1]", "gamma[4]", "gamma[3]", "gamma[12]")))

ggs_traceplot(s %>% filter(Parameter %in% c("delta[10]", "delta[20]", "delta[30]", "delta[40]")))

ggs_traceplot(s %>% filter(Parameter %in% c("xi_week[10]", "xi_week[44]", "xi_week[30]", "xi_week[40]")))

## geweke - for overall convergence
ggs_geweke(s)

ggs_geweke(s %>% filter(str_detect(Parameter, "pred")==T))


s %>% 
  filter(str_detect(Parameter, "theta") == T) %>% ggs_autocorrelation(nLags = 100)

s %>% 
  filter(str_detect(Parameter, "pred\\[42,4\\]") == T) %>% ggs_autocorrelation(nLags = 100)



s %>% 
  filter(str_detect(Parameter, "xi\\[10,") == T | str_detect(Parameter, "xi_month\\[10") == T) %>% ggs_crosscorrelation()



s %>% 
  filter(str_detect(Parameter, "pred\\[42,4") == T) %>% 
  ggs_density()


s %>% 
  filter(str_detect(Parameter, "pred\\[29") == T &
           str_detect(Parameter, ",11") == T) %>% 
  ggs_traceplot()
 # ggs_autocorrelation()

# s %>% 
#   filter(str_detect(Parameter, "xi_month\\[4") == T) %>% 
#   ggs_density()

s %>% 
  filter(str_detect(Parameter, "xi\\[43") == T & str_detect(Parameter, ",1\\]") == T) %>% 
 # ggs_density()
 # ggs_autocorrelation(nLags= 1000)
  ggs_traceplot()


s %>% 
  filter(str_detect(Parameter, "xi_week\\[4") == T) %>% 
 # ggs_traceplot() %>% 
  ggs_autocorrelation(nLags = 500)


s %>% 
  filter(str_detect(Parameter, "xi\\[10,3\\]") == T) %>% 
  ggs_traceplot()


s %>% 
  filter(str_detect(Parameter, "gamma\\[1") == T) %>% 
  ggs_traceplot() 



s %>% 
  filter(str_detect(Parameter, "theta") == T) %>% 
  ggs_traceplot()



# results -----------------------------------------------------------------


## omegas

s %>% 
  filter(str_detect(Parameter, "omega") == T | str_detect(Parameter, "mix") == T) %>% 
  group_by(Parameter) %>% 
  summarise(est = mean(value),
            sd = sd(value)) %>% 
  print(n = Inf)

s %>% 
  filter(str_detect(Parameter, "omega") == T) %>% 
  group_by(Parameter) %>% 
  summarise(est = mean(value),
            sd = sd(value)) %>% 
  print(n = Inf)

## looking at pollster effects:


s %>% 
  group_by(Parameter) %>% 
  filter(str_detect(Parameter, "delta") == T)  %>% 
  summarise(est = mean(value),
            sd = sd(value)) %>% 
  cbind(dat %>%
          select(pollster, pollster_num) %>% unique %>% arrange(pollster_num)) %>%
  as.tibble() %>%
  mutate(pollster = factor(pollster, levels = pollster[order(est)])) %>% 
  
  ggplot(aes(x = pollster, y = est)) +
  geom_point() +
  geom_hline(aes(yintercept = 0, color = "green")) +
  geom_errorbar(aes(x = pollster, ymin = est-sd*2, ymax = est+sd*2)) +
  coord_flip()


## Pollster effects - pretty graphs


s %>% 
  group_by(Parameter) %>% 
  filter(str_detect(Parameter, "delta") == T)  %>% 
  summarise(est = mean(value),
            sd = sd(value)) %>% 
  cbind(dat %>%
          select(pollster, pollster_num) %>% unique %>% arrange(pollster_num)) %>%
  as.tibble() %>%
  mutate(pollster = factor(pollster, levels = pollster[order(est)])) %>% 
  filter(est < -.005) %>% 
  
  ggplot(aes(x = pollster, y = est)) +
  geom_point() +
  geom_hline(aes(yintercept = 0, color = "green")) +
  geom_errorbar(aes(x = pollster, ymin = est-sd*2, ymax = est+sd*2)) +
  coord_flip() +
  labs(title = "GOP +1% Pollsters")


s %>% 
  group_by(Parameter) %>% 
  filter(str_detect(Parameter, "delta") == T)  %>% 
  summarise(est = mean(value),
            sd = sd(value)) %>% 
  cbind(dat %>%
          select(pollster, pollster_num) %>% unique %>% arrange(pollster_num)) %>%
  as.tibble() %>%
  mutate(pollster = factor(pollster, levels = pollster[order(est)])) %>% 
  filter(est > .005) %>% 
  
  ggplot(aes(x = pollster, y = est)) +
  geom_point() +
  geom_hline(aes(yintercept = 0, color = "green")) +
  geom_errorbar(aes(x = pollster, ymin = est-sd*2, ymax = est+sd*2)) +
  coord_flip() +
  labs(title = "Dem +1% Pollsters")



s %>% 
  group_by(Parameter) %>% 
  filter(str_detect(Parameter, "delta") == T)  %>% 
  summarise(est = mean(value),
            sd = sd(value)) %>% 
  cbind(dat %>%
          select(pollster, pollster_num) %>% unique %>% arrange(pollster_num)) %>%
  as.tibble() %>%
  mutate(pollster = factor(pollster, levels = pollster[order(est)])) %>% 
  filter(est > -.005 & est <.005) %>% 
  
  ggplot(aes(x = pollster, y = est)) +
  geom_point() +
  geom_hline(aes(yintercept = 0, color = "green")) +
  geom_errorbar(aes(x = pollster, ymin = est-sd*2, ymax = est+sd*2)) +
  coord_flip() +
  labs(title = "Middle Pollsters")

## looking at weekly effects:

s %>% 
  group_by(Parameter) %>% 
  filter(str_detect(Parameter, "xi_week") == T)  %>% 
  summarise(est = mean(value),
            sd = sd(value)) %>% 
  mutate(week = seq(1, length(data$xi_week), 1)) %>% 
  ggplot(aes(x = week, y = est)) +
  geom_point(aes(size = 1.5)) +
  # geom_hline(aes(yintercept = 0, color = "green")) +
  geom_errorbar(aes(x = week, ymin = est-sd*2, ymax = est+sd*2), linetype = "dashed", color = "green") 

## new weekly effects for states:

dat %>% select(state, state_num) %>% unique %>% print(n = Inf) #AZ = 1, MO = 13, TN = 24, TX = 25, FL = 5

s %>% 
  filter(str_detect(Parameter, "xi\\[") == T & str_detect(Parameter, ",1\\]") == T)  %>% 
  group_by(Parameter) %>% 
  summarise(est = mean(value),
            sd = sd(value)) %>% 
  ggplot(aes(x = Parameter, y = est)) +
  geom_point(aes(size = 1.5)) +
  # geom_hline(aes(yintercept = 0, color = "green")) +
  geom_errorbar(aes(x = Parameter, ymin = est-sd*2, ymax = est+sd*2), linetype = "dashed", color = "green") +
  labs(title = "FL Forecast - higher numbers = more recent")

## state level effects

s %>% 
  group_by(Parameter) %>% 
  filter(str_detect(Parameter, "gamma\\[") == T)  %>% 
  summarise(est = mean(value),
            sd = sd(value)) %>% 
  cbind(dat %>%
          select(state, state_num) %>% unique %>% arrange(state_num)) %>%
  as.tibble() %>%
  mutate(state = factor(state, levels = state[order(est)])) %>% 
  
  ggplot(aes(x = state, y = est)) +
  geom_point() +
  geom_hline(aes(yintercept = 0, color = "green")) +
  geom_errorbar(aes(x = state, ymin = est-sd*2, ymax = est+sd*2)) +
  coord_flip()

## universe level effects

s %>% 
  group_by(Parameter) %>% 
  filter(str_detect(Parameter, "theta\\[") == T)  %>% 
  summarise(est = mean(value),
            sd = sd(value)) %>% 
  cbind(univ = c("A", "LV", "RV")) %>%
  as.tibble() %>%
  mutate(univ = factor(univ, levels = univ[order(est)])) %>% 
  
  ggplot(aes(x = univ, y = est)) +
  geom_point() +
  geom_hline(aes(yintercept = 0, color = "green")) +
  geom_errorbar(aes(x = univ, ymin = est-sd*2, ymax = est+sd*2)) +
  coord_flip()



