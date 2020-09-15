#Here we provide of BMM estimation in Stan. 
#Our example simulates and estimates parameters of the 
#"unified model of fatigue-related performance impairment" (Rajdev et al., 2013).

# Rajdev, P., Thorsley, D., Rajaraman, S.,
# Rupp, T. L., Wesensten, N. J., Balkin, T. J., & Reifman, J. (2013).
# A unified mathematical model to quantify performance impairment for 
# both chronic sleep restriction and total sleep deprivation.
# Journal of theoretical biology, 331, 66-77.

#In text, we tested recovery using sleep times from a field operation.
#Here, we demonstrate using a simulated laboratory data. The simulated data
#structure was based on experiment T1 of: 

# Ramakrishnan, S., Wesensten, N. J., Balkin, T. J., & Reifman, J. (2016).
# A unified model of performance: validation of its predictions across different sleep/wake schedules.
# Sleep, 39(1), 249-262.

library(rstan)

#The stan code requires that data is formatted in a "data list", which encodes time of sleep initation, 
#sleep end, and observations of fatigue. The data list requires the following elements:

#Nsubj - number of subjects
#Ntotal - Number of total data points
#subject - subject number
#event_number - event number (in order) for each subject
#previous_episode_type - was the previous episode sleep (1) or wake (2)
#time_since_previous - time of last episode
#timeofday
#valid - does the event contain a fatigue observation
#Nvalid - number of fatigue observation events
# fatigue - a fatigue observation for each event. For
# the simulation script, we enter a vector of 0s.

#Example lab data list
load("03_ParameterEstimation/data_list_sleepdep.RData")

#To simulate also need a list of parameters
#randomly generated from ranges in paper
set.seed(321)

parm_list = list()
parm_list$U0 = runif(n = 1, min = 10, max = 35)   
parm_list$L0_raw = runif(n = 1, min = 0, max = 1)
parm_list$S0_raw = runif(n = 1, min = 0, max = 1)
parm_list$phi = runif(n = 1, min = -8, max = 8)
parm_list$kappa = runif(n = 1, min = 1, max = 8)

parm_list$tau_d = runif(n = 1, min = 0, max = 6)
parm_list$tau_r <- runif(n = 1, min = 0, max = 40)
parm_list$tau_la<- runif(n = 1, min = 97.44-20, max = 97.44+20)

parm_list$sigma =  runif(n = 1, min = 5, max = 20)

#Simulate some fatigue data

sim_list <- c(data_list, parm_list)
sim_list$fatigue <- rep(0, data_list$Ntotal)

simmed_fatigue <- stan( file=
                        "03_ParameterEstimation/unified_group_simulate.stan" , 
                      data = sim_list, 
                      pars = c("pp"),
                      include = TRUE,
                      cores=1,
                      iter=1,
                      chains = 1,
                      seed=1234 )

#Fit to the simulated data as if it was real data

fit_list <- data_list
fit_list$fatigue = round(as.vector(rstan::extract(simmed_fatigue,"pp")$pp))

#Slow: Bayesian MCMC parameter estimation
fit_fatigue <- stan( file="03_ParameterEstimation/unified_group_fit.stan" , 
                     data = fit_list, 
                     cores=4,
                     chains = 4,
                     seed=1234,
)




