# the model ------------------------------------------------------------

library(lubridate)
library(tidyverse)
library(ggmcmc)
library(rjags)
library(civis)

#dat <- read_civis("cody.polling_cleaned_1102") %>% as.tibble()

model <- "
model {

### Likelihood

for (i in 1:length(twoway)) {
twoway[i] ~ dnorm(mu[i], prec[i])
#   yhat[i] ~ dnorm(mu[i], 5)
mu[i] = xi[week_adj[i], state_num[i]] + delta[pollster_num[i]] + theta[univ_num[i]] + xi_week[week_adj[i]] + gamma[state_num[i]]
}

## predictions

for (j in 1:length(gammas)) {
for (t in 1:length(xi_week))  {
pred[t,j] = xi[t, j] + xi_week[t] + gamma[j]

}
}

## time priors:

for (j in 1:length(gammas)) {
for (t in 2:length(xi_week))  {
xi[t,j]~dnorm(xi[t-1, state_num[j]], tau_stateweek[j])
}

xi[1, j]~dnorm(xi1_prior[j] + state_bias[j], tau_stateweek[j])

xi1_prior[j] ~ dunif(.01, .5)



mix_stateweek[j] ~ dcat(mix_prior_prob[])

omega_stateweek[1, j] ~ dunif(0, 0.04)
omega_stateweek[2, j] ~ dunif(0, 0.004)

tau_stateweek[j] <- 1/pow(omega_stateweek[mix, j], 2)

}

for (t in 2:length(xi_week))  {
xi_week[t]~dnorm(xi_week[t-1], tau_week)
}

xi_week[1]~dunif(.01, .5)

mix ~ dcat(mix_prior_prob[])
mix_prior_prob[1] <- .02
mix_prior_prob[2] <- .98

omega[1] ~ dunif(0, 0.035)
omega[2] ~ dunif(0, 0.0035)

tau_week <- 1/pow(omega[mix],2)

## Pollster priors:

for (i in 1:length(deltas)) {
delta[i] ~ dnorm(pollster_bias[i], tau_delta)
}

omega_delta ~ dunif(0, .05) #.00560)
tau_delta = 1/pow(omega_delta, 2)

## state priors

for (i in 1:length(gammas)) {
gamma[i] ~ dnorm(state_bias[i], tau_gamma)
}


omega_gamma ~ dunif(0, .1)
tau_gamma = 1/pow(omega_gamma, 2)

## Universe priors: 

for (i in 1:length(thetas)) {
theta[i] ~ dnorm(theta_mu_prior[i], 1/theta_var_prior[i])
}

}
"
# data and running model --------------------------------------------------

## extracting pollster and state info for JAGS
pollster_leans <- dat %>% select(pollster, pollster_num, bias, var_upper) %>% unique 

state_leans <- dat %>% select(state, state_lean, state_num) %>% unique %>% arrange(state_num) %>% select(state_lean)

## making the jags object
data = with(dat, 
            list(
              twoway = twoway,
              state_num = state_num,
              week_adj = week_adj,
              # month_adj = month_adj,
              
              pollster_num = pollster_num,
              univ_num = univ_num,
              pollster_bias = pollster_leans$bias,
              state_bias = state_leans$state_lean,
              #    pollster_var_lower = var_lower,
              #    pollster_var_upper = var_upper,
              prec = 1 / ((twoway * (1 - twoway)) / n_size),
              deltas = rep(NA, length(pollster %>% unique)),
              thetas = rep(NA, length(univ %>% unique)),
              gammas = rep(NA, length(state_num %>% unique)),
              xi = matrix(data = NA, nrow = length(rep(NA, max(length(unique(week_adj)), max(week_adj)))), ncol = length(state_num %>% unique)),
              pred = matrix(data = NA, nrow = length(rep(NA, max(length(unique(week_adj)), max(week_adj)))), ncol = length(state_num %>% unique)),
              xi_week = rep(NA, max(length(unique(week_adj)), max(week_adj))),
              # xi_month = rep(NA, max(length(unique(month_adj)), max(month_adj))),
              # theta_mu_prior = c(0, 0, 0),
              # theta_var_prior = c(10, 10, 10)
              #  theta_mu_prior = c(.00925, .0131, .0386),
              theta_mu_prior = c(.0386, -.01, .026),    
              theta_var_prior = c(0.00000676, 0.00000827, .000038)
              #theta_var_prior = c(1000,1000,1000)
              
            ))

#lapply(data, len

# run model ---------------------------------------------------------------
mod <- jags.model(textConnection(model),
                  data = data,
                  n.chains = 3)

samp <- coda.samples(mod, 
                     variable.names = c("xi", "delta", "theta", "xi_week", "gamma",
                                        #  "tau_gamma", "tau_week", "tau_stateweek",
                                        "pred",
                                        "omega", "omega_stateweek",
                                        "mix", "mix_stateweek"), 
                     burnin = 10000,
                     n.iter = 1000000,
                     adapt = 1000,
                     thin = 1000
                      # burnin = 10000,
                      # n.iter = 10000,
                      # adapt = 1000,
                      # thin = 10
)

s <- ggs(samp)

#s %>% write.csv("1031_1m_iter.csv")

#s %>% write_civis("cody.senate_model_output") ## note this takes a really long time for the big simulatoins - I'd recommend keeping this all local given we're updating this every day.
