# ------------------------------------------------------------------------------
# Title: Monte Carlo approacc to estimating excess asthma emergency depart
#        visits due to wildfire-generated ozone; stratum-specific estimates
# Date Created: 8/3/16
# ------------------------------------------------------------------------------

# No libraries need as we use the base functions

# check the dataframes Jake created
list.files('./data')

# start with one loop first of the state-specific estimates
state_path <- paste0('./data/Total.csv')

state_specific_df <- read.csv(state_path)
head(state_specific_df)

# output the vector of state names
state_list <- state_specific_df[,1]
state_list

# state-specific loop
set.seed(1234)
# number of interations for HIA MC
n <- 10000

# yo and beta as well as their std errors do not change
# simulate the distributions outside the loop
# output the value for alabama since they are the same for each state

# these estimate comes from a meta analysis of the estimate and prev.
yo_total <- state_specific_df[1,2] 
yo_se_total <- state_specific_df[1,3]/100 # i think I need to divide by 
# check with Sheryl and Jake on if this should be a proportion or a rate
# ie .1068 vs 10.68
yo_distribution <- rnorm(n, mean = yo_total, sd = yo_se_total)
# distribution of ED_Rate
plot(density(yo_distribution))
hist(yo_distribution)
# Calculation of median, and 2.5% observation and 97.5% observation. 
# Estimates are 10.67 pooled ed rate over this time, with 95% CI of 9.74 - 11.6
quantile(yo_distribution, c(0.025, 0.50, 0.975))
# median = 0.1068, 2.5th percentile is .0977 and 97th percentile is  0.1160
# Estimates correspond to the meta analysis estimates. 
# I do need to either divide SE by 100 or multiple the prevalence by 100
# I need to clarify with Jake and Sheryl the most appropriate estimate to use

# Using the same beta estimate for all simulations
beta_total <- state_specific_df[1, 8]
beta_se_total <- state_specific_df[1, 9]
# distribution of betas
beta_distribution <- rnorm(n, mean = beta_total, sd = beta_se_total)
# check distribution
hist(beta_distribution)
# check quantiles
quantile(beta_distribution, c(0.025, 0.5, 0.975))
# I think the standard error around the beta estimate needs to be .0003043
# ask Jake where 0.000298 came from. Likely not a big deal since it's pretty close

# create an empty dataframe to populate with state specific median and bounds
hia_state_specific_df <- data.frame(matrix(vector(), 48, 4, 
                        dimnames = list(c(), c("state", "median", "lower_bound",
                                        "upper_bound"))), stringsAsFactors = F)
# start loop to run MC for each state
for(i in 1:length(state_list)){
 
 # feed state name in to hia dataframe
 hia_state_specific_df[[i, 1]] <- as.character(state_specific_df[i, 1])
 
 # Now I need to simulate the distributions for population at risk and 
 # change in ozone for each state

 # output the state-specific pop_at_risk values
 state_par_val <- as.numeric(state_specific_df[i, 4])  
 state_par_se <- as.numeric(state_specific_df[i, 5]) 
 # create state-specific pop at risk distribution
 state_par_distribution <- rnorm(n, mean = state_par_val, sd = state_par_se)
 
 # output the state-specific delta ozone values
 state_delta_o3 <- as.numeric(state_specific_df[i, 6])
 state_do3_se <- as.numeric(state_specific_df[i, 7])
 # create state-specific o3 distribution
 state_o3_distribution <- rnorm(n, mean = state_delta_o3, sd = state_do3_se)
 
 # make empty vector 
 delta_y <- vector("double", length = n)
 # beginning of inner loop to estimate n interations of HIA formula
 for(j in 1:n) {
   # take one random value with replacement from the ED_rate distribution
   est_y0 <- sample(yo_distribution, 1, replace = T)
   # take one random value from the beta distribution with replacement
   est_beta <- sample(beta_distribution, 1, replace = T)
   # take one random value from the ozone distribution with replacement
   est_delta_o3 <- sample(state_o3_distribution, 1, replace = T)
   # take one random value from the population at risk distribution with replacement
   pop_at_risk <- sample(state_par_distribution, 1, replace = T)
   
   # apply the HIA formula with the randomly selected values above
   # and save the estimate delta y in the vector, n times
   delta_y[[j]] <- est_y0*(1-exp((-est_beta)*(est_delta_o3)))*pop_at_risk
   
 } 
 # end of inner loop
 
 # fill in the state-specific median, 2.5% lower bound, and 97.5% upper bound
 # from the monte carlo vector
 # now calculate the median value of delta y
 hia_state_specific_df[[i, 2]] <- as.numeric(quantile(delta_y, 0.50))
 # 2.5% lower bound
 hia_state_specific_df[[i, 3]]<- as.numeric(quantile(delta_y, 0.025))
 # 97.5 % upper bound
 hia_state_specific_df[[i, 4]] <- as.numeric(quantile(delta_y, 0.975))

}

# check to make sure delta ozone and beta are the same units 

# write out dataframe for now, if ran in the future, I don't think it will produce
# the exact same values, but should be darn close.

write_path <- paste('./data/total_state_hia_monte_carlo.csv')

write.csv(hia_state_specific_df , file = write_path)

