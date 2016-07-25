# ------------------------------------------------------------------------------
# Title: Draft Monte Carlo approahc to estimating excess asthma emergency depart
#        visits due to wildfire-generated ozone
# Authors: Jacob Pratt 
# Date Created: 7/25/16
# ------------------------------------------------------------------------------

# HIA Distributions ------------------------------------------------------------
# The first step is to generate a distribution of values for each term in the 
# Health Impact Assessment formula for our Monte Carlo approach to draw from

set.seed(25) # random seed

# Population at risk term distribution (add source)
asthmatic_population <- rnorm(1000000, mean = 6319623, sd = 716510)
# Standard error (SE) will work in place of standard deviation (SD)
# SE is generally the model estimation of SD

# with a large enough sample, our distribution should be normal due to central
# limit theorem

# Check that the distribution approximates the source point estimate and 95% CI
plot(density(asthmatic_population))
hist(asthmatic_population)
# check bounds; should line up with mean and CI pretty well
quantile(asthmatic_population, c(0.025, .5, 0.975))

# Delta ozone over timeframe (add source and insert timeframe here)
delta_o3 <- rnorm(1000000, mean = 5.91, sd = 2.53)
plot(density(delta_o3))
# 95% bounds and median
quantile(delta_o3, c(0.025, .5, 0.975))

# Beta estimate of the effect of ozone on asthma hospitalization (add source)
beta <- rnorm(1000000, mean = .001594, sd = .0003043)

# distribution of beta
plot(density(beta))
hist(beta)
# Calculation of median, and 2.5% observation and 97.5% observation. 
quantile(beta, c(0.025, 0.50, 0.975))

# ED visit rate
ED_rate <- rnorm(1000000, mean = .1068, sd = .0182)
# distribution of ED_Rate
plot(density(ED_rate))
hist(ED_rate)
# Calculation of median, and 2.5% observation and 97.5% observation. 
quantile(ED_rate, c(0.025, 0.50, 0.975))

# Monte Carlo approach ---------------------------------------------------------
# Estimating median ED visits due to wildfire-generated ozone and 95% bounds 

# number of times to run resampling
n <- 100000

# create a vector to populate with a loop (computationally more efficient)

delta_y <- vector("double", length = n)

# beginning of loop
for(i in 1:n) {
  # take one random value with replacement from the ED_rate distribution
  est_y0 <- sample(ED_rate, 1, replace = T)
  # take one random value from the beta distribution with replacement
  est_beta <- sample(beta, 1, replace = T)
  # take one random value from the ozone distribution with replacement
  est_delta_o3 <- sample(delta_o3, 1, replace = T)
  # take one random value from the population at risk distribution with replacement
  pop_at_risk <- sample(asthmatic_population, 1, replace = T)
  
  # apply the HIA formula with the randomly selected values above
  # and save the estimate delta y in the vector, n times
  delta_y[[i]] <- est_y0*(1-exp((-est_beta)*(est_delta_o3)))*pop_at_risk
  
} 
# end of loop

# Check distribution of our delta y values
plot(density(delta_y))
hist(delta_y)

# now calculate the median value of delta y
median <- quantile(delta_y, 0.50)
# 2.5% lower bound
lower_bound <- quantile(delta_y, 0.025)
# 97.5 % upper bound
upper_bound <- quantile(delta_y, 0.975)

title <- cbind(median, lower_bound, upper_bound)



