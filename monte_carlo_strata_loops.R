# ------------------------------------------------------------------------------
# Title: Monte Carlo approacc to estimating excess asthma emergency depart
#        visits due to wildfire-generated ozone; stratum-specific estimates
# Date Created: 8/3/16
# ------------------------------------------------------------------------------

# No libraries need as we use the base functions

# check the dataframes Jake created
list.files('./data')

# Read in stratum dataframes ---------------------------------------------------
# start with one loop first of the total state-specific estimates
total_path <- paste0('./data/Total.csv')
total <- read.csv(total_path)
head(total)
# standard error of yo needs to be divided by 100
total$yo_se <- total$yo_se/100
# done; check below
head(total)

# read in african american df
black_path <- paste0('./data/Black.csv')
black <- read.csv(black_path)
head(black)

# hispanic
hisp_path <- paste0('./data/HIspanic.csv')
hisp <- read.csv(hisp_path)
head(hisp)

# white
white_path <- paste0('./data/White.csv')
white <- read.csv(white_path)
head(white)

# male
m_path <- paste0('./data/Male.csv')
male <- read.csv(m_path)
head(male)

# female
f_path <- paste0('./data/Female.csv')
female <- read.csv(f_path)
head(female)

# create a list of dataframes to cycle through
df_list <- list(total=total, male=male, female=female, black=black, 
                hisp=hisp, white=white)

df_name <- c('total', 'male', 'female', 'black', 'hisp', 'white')

# output the vector of state names
state_list <- total[,1]
state_list


# Loop -------------------------------------------------------------------------
# Set seed for reproducibility
set.seed(1234)
# number of interations for HIA MC and simulated distribution sample size
n <- 10000

# start with each dataframe
for(k in 1:length(df_list)){
  # set dataframe to loop through  

  data_frame <- data.frame(df_list[k]) 
  data_frame_name <- df_name[k]
  

# state-specific loop

# yo and beta as well as their std errors do not change
# simulate the distributions outside the loop
# output the value for alabama since they are the same for each state

# these estimate comes from a meta analysis of the estimate and prev.
yo_total <- data_frame[1,2] 
yo_se_total <- data_frame[1,3] # i think I need to divide by 
# check with Sheryl and Jake on if this should be a proportion or a rate
# ie .1068 vs 10.68
yo_distribution <- rnorm(n, mean = yo_total, sd = yo_se_total)
# distribution of ED_Rate
# I commented out checks of distribution 
#plot(density(yo_distribution))
#hist(yo_distribution)
# Calculation of median, and 2.5% observation and 97.5% observation. 
# Estimates are 10.67 pooled ed rate over this time, with 95% CI of 9.74 - 11.6
# quantile(yo_distribution, c(0.025, 0.50, 0.975))
# median = 0.1068, 2.5th percentile is .0977 and 97th percentile is  0.1160
# Estimates correspond to the meta analysis estimates. 
# I do need to either divide SE by 100 or multiple the prevalence by 100
# I need to clarify with Jake and Sheryl the most appropriate estimate to use

# Using the same beta estimate for all simulations
beta_total <- data_frame[1, 8]
beta_se_total <- data_frame[1, 9]
# distribution of betas
beta_distribution <- rnorm(n, mean = beta_total, sd = beta_se_total)
# commenting out
# check distribution
#hist(beta_distribution)
# check quantiles
#quantile(beta_distribution, c(0.025, 0.5, 0.975))
# I think the standard error around the beta estimate needs to be .0003043
# ask Jake where 0.000298 came from; guessing he used a critical value of 2
# rather than 1.96, Likely not a big deal in this scenario.

# create an empty dataframe to populate with state specific median and bounds
hia_df <- data.frame(matrix(vector(), 48, 4, 
                     dimnames = list(c(), c("state", "median", "lower_bound",
                                        "upper_bound"))), stringsAsFactors = F)
  # start 2nd loop to run MC for each state
  for(i in 1:length(state_list)){
   
   # feed state name in to hia dataframe
   hia_df[[i, 1]] <- as.character(data_frame[i, 1])
   
   # Now I need to simulate the distributions for population at risk and 
   # change in ozone for each state
  
   # output the state-specific pop_at_risk values
   state_par_val <- as.numeric(data_frame[i, 4])  
   state_par_se <- as.numeric(data_frame[i, 5]) 
   # create state-specific pop at risk distribution
   state_par_distribution <- rnorm(n, mean = state_par_val, sd = state_par_se)
   
   # output the state-specific delta ozone values
   state_delta_o3 <- as.numeric(data_frame[i, 6])
   state_do3_se <- as.numeric(data_frame[i, 7])
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
     
    } # end of inner loop of HIA estimate
   
   # fill in the state-specific median, 2.5% lower bound, and 97.5% upper bound
   # from the monte carlo vector
   # now calculate the median value of delta y
   hia_df[[i, 2]] <- as.numeric(quantile(delta_y, 0.50))
   # 2.5% lower bound
   hia_df[[i, 3]]<- as.numeric(quantile(delta_y, 0.025))
   # 97.5 % upper bound
   hia_df[[i, 4]] <- as.numeric(quantile(delta_y, 0.975))
  
  } # end state specific

# create environment dataframe with name of original dataset hia is based off
hia_name <- paste(data_frame_name, 'hia', sep = '_')
assign(hia_name, hia_df)

# write permanent dataset
write_path <- paste('./data/', hia_name, '.csv', sep = '')
write.csv(hia_df , file = write_path)

} # end dataframe specific loop
# check to make sure delta ozone and beta are the same units 

# rerunning this code will in the future will not produce the exact same
# median and bound values because of the random replacement factor, but 
# the values should be very close.



