# ------------------------------------------------------------------------------
# Title: Monte Carlo Estimate for Wildfire Ozone and Asthma ED Visits
# Author: Jacob Pratt and Ryan Gan
# Date: 2/18/17
# ------------------------------------------------------------------------------

# library
library(tidyverse)

# data read ----

# read smoky days df
ozone_path <- paste0('./data/state_delta_o3.csv')
ozone  <- read_csv(ozone_path) %>% rename(State = state)

# start with one loop first of the marginal state-specific estimates
marginal_path <- paste0('./data/total.csv')
# import csv and make sure variables as numeric like they should be
marginal <- read_csv(marginal_path, col_types = "cdddddddd", na = "NA") 

# read in african american df
black_path <- paste0('./data/Black.csv')
black <- read_csv(black_path, col_types = "cdddddddd", na = "NA")


# hispanic
hisp_path <- paste0('./data/Hispanic.csv')
hisp <- read_csv(hisp_path, col_types = "cdddddddd", na = "NA") 


# white
white_path <- paste0('./data/White.csv')
white <- read_csv(white_path, col_types = "cdddddddd", na = "NA") 


# male
m_path <- paste0('./data/Male.csv')
male <- read_csv(m_path, col_types = "cdddddddd", na = "NA") 

# female
f_path <- paste0('./data/Female.csv')
female <- read_csv(f_path, col_types = "cdddddddd", na = "NA")


# create a list of dataframes to cycle through
df_list <- list(marginal=marginal, male=male, female=female, black=black, 
                hisp=hisp, white=white)

df_name <- c('marginal', 'male', 'female', 'black', 'hisp', 'white')

df_list[1]

# join smokey days df to each dataframe
for(q in 1:length(df_list)){
  
  # loop through df list and remove old delta ozone and beta smoky
  data_join_df <- df_list[[q]] %>% 
    select(-delta_o3, -do3_se, -beta, -beta_se) %>% 
    full_join(ozone, by = "State") %>% filter(complete.cases(.))
  # rename original df lists to contain smoky days val
  assign(names(df_list[q]), data_join_df)
}

# re-create list of dataframes to cycle through now that it has smoke days
df_list <- list(marginal=marginal, male=male, female=female, black=black, 
                hisp=hisp, white=white)
# check out first element of list to make sure smoky days variable is there
# df_list[1]



# Estimates of ED visit burden in U.S. children with asthma that may be due to 
# exposure to ozone generated from wildfires. Beta estimate and standard error 
# now come from the Mar and Koenig 2009 paper. This estimate is child-specific 
# (ages 0-18) of same day concentrations of ozone and ED visits. They found a 
# risk ratio of 1.10 (95%CI: 1.01-1.19) for an ED visit for every 10 ppb increase 
# in ozone, which were averaged across two study sites. We decided on this 
# estimate for our HIA because it was specific to children, had a continous 
# estimate of ozone, in the U.S., and on the same day (lag day 0). 
# I calculated the beta estimate to be 0.0095 for a 1 ppb increase, 
# with a standard error of 0.004. 

# Note: I will need to remove beta and SE from datasets. 
# Yo and beta are now simulated outside the MC so that we draw 
# from the same distribution each time.

# Monte Carlo ----

# Loop 
# Set seed  to use so sample distributions and resampling is somewhat similar
sim_seed <- 1234
# number of interations for HIA MC and simulated distribution sample size
n <- 10000

# simulate beta distribution as this estimate applies across all strata
# using the same beta estimate for all simulations
beta <- 0.0095
beta_se <- 0.0040

# distribution of beta
set.seed(sim_seed)
beta_distribution <- rnorm(n, mean = beta, sd = beta_se)

# check on beta distribution
# ggplot(data = as.data.frame(beta_distribution), aes(x=beta_distribution))+ 
#   geom_density()
# 
# # check against original estimates
# exp(as.numeric(quantile(beta_distribution, 0.50, na.rm = T))*10)
# # 2.5% lower bound
# exp(as.numeric(quantile(beta_distribution, 0.025, na.rm = T))*10)
# # 97.5 % upper bound
# exp(as.numeric(quantile(beta_distribution, 0.975, na.rm = T))*10)


# strata loop ---
for(k in 1:length(df_list)){
  
  # set dataframe to loop through  
  # Note: changing name 'data_frame' to 'df_to_loop' 
  # since it's also a function in dplyr
  df_to_loop <- data.frame(df_list[k]) 
  df_name <- names(df_list[k])
  
  # yo and beta as well as their std errors do not change
  # simulate the distributions outside the loop
  # output the value for alabama since they are the same for each state
  # these estimate comes from a meta analysis of the estimate and prev.
  yo_marginal <- df_to_loop[1,2] 
  yo_se_marginal <- df_to_loop[1,3] 
  
  yo_distribution <- rnorm(n, mean = yo_marginal, sd = yo_se_marginal)
  
  # yo check  
  # ggplot(data = as.data.frame(yo_distribution), aes(x=yo_distribution))+ 
  #   geom_density()
  
  # create an empty matrix to populate with state specific median and bounds
  hia_matrix <- matrix(data=NA , nrow(df_to_loop), 5) 
  colnames(hia_matrix) <- c("state", "median", "lower_bound", "upper_bound",
                            "group")
  
  # convert hia_matrix to dataframe
  hia_prop_df <- hia_df <- hia_daily_df <- data.frame(hia_matrix)
  # duplicate empty df to fill with daily estimates
  
  # assign a group name
  hia_prop_df[,5] <- hia_df[,5] <- hia_daily_df[,5] <- df_name
  
  # start 2nd loop to run MC for each state ----
  for(i in 1:nrow(df_to_loop)){

    # feed state name in to hia dataframe
    hia_df[[i, 1]] <- as.character(df_to_loop[i, 1])
    hia_daily_df[[i, 1]] <- as.character(df_to_loop[i, 1])
    hia_prop_df[[i, 1]] <- as.character(df_to_loop[i, 1])
    
    # Now I need to simulate the distributions for population at risk and 
    # output the state-specific pop_at_risk values
    state_par_val <- df_to_loop[i, 4] 
    state_par_se <- df_to_loop[i, 5] 
    # create state-specific pop at risk distribution
    state_par_distribution <- rnorm(n, mean = state_par_val, sd = state_par_se)
    
    # output the state-specific delta ozone values
    state_delta_o3 <- df_to_loop[i, 6]
    state_do3_se <- df_to_loop[i, 7]
    # create state-specific o3 distribution
    state_o3_distribution <- rnorm(n, mean = state_delta_o3, sd = state_do3_se)
    
    # output the state-specific n smoke days value
    state_smk_d_n <- df_to_loop[i, 8]
    # create state_specific smoke days Poisson dist
    state_smoky_days_distribution <- rpois(n, state_smk_d_n)
    
    # make empty vectors to fill with change in y
    # daily 
    delta_y_daily <- vector("double", length = n)
    # period
    delta_y <- vector("double", length = n)
    # period proportion
    prop_y <- vector("double", length = n)
    
    # beginning of inner loop to estimate n interations of HIA formula ----
    for(j in 1:n) {
      # take one random value with replacement from the ED_rate distribution
      # divide by 365 to estimate daily rate
      est_yo <- (sample(yo_distribution, 1, replace = T))/365
      # take one random value from the beta distribution with replacement
      est_beta <- sample(beta_distribution, 1, replace = T)
      # take one random value from the ozone distribution with replacement
      est_delta_o3 <- sample(state_o3_distribution, 1, replace = T)
      # take one random value from the population at risk distribution with replacement
      pop_at_risk <- sample(state_par_distribution, 1, replace = T)
      #take a random value from the poisson distribution of smoky days
      est_smoky_days <- sample(state_smoky_days_distribution, 1, replace = T)
      
      
      # apply the HIA formula with the randomly selected values above
      # and save the estimate delta y in the vector, n times to
      # fill in the state-specific median, 2.5% lower bound, and 97.5% upper bound
      # from the monte carlo vector
      
      # average daily estimate
      delta_y_daily[[j]] <- (est_yo*(1-exp((-est_beta)*(est_delta_o3)))*pop_at_risk)
      # average period estimate
      delta_y[[j]] <- (est_yo*(1-exp((-est_beta)*(est_delta_o3)))*pop_at_risk)*est_smoky_days
      
      # adding a new formula to calculate the delta y proportion 
      # that may be due to wfo3 
      prop_y[[j]] <- (est_yo*(1-exp((-est_beta)*(est_delta_o3))))*est_smoky_days
      
    } # end of inner loop of HIA estimate
    
    # now calculate the median value of delta y and fill in dataframes
    # daily estimates ----
    hia_daily_df[[i, 2]] <- as.numeric(quantile(delta_y_daily, 0.50, na.rm = T))
    # 2.5% lower bound
    hia_daily_df[[i, 3]]<- as.numeric(quantile(delta_y_daily, 0.025, na.rm = T))
    # 97.5 % upper bound
    hia_daily_df[[i, 4]] <- as.numeric(quantile(delta_y_daily, 0.975, na.rm = T))
    
    # smoke period estimates ----
    hia_df[[i, 2]] <- as.numeric(quantile(delta_y, 0.50, na.rm = T))
    # 2.5% lower bound
    hia_df[[i, 3]]<- as.numeric(quantile(delta_y, 0.025, na.rm = T))
    # 97.5 % upper bound
    hia_df[[i, 4]] <- as.numeric(quantile(delta_y, 0.975, na.rm = T))
    
    # smoke period for proportion estimates ----
    hia_prop_df[[i, 2]] <- as.numeric(quantile(prop_y, 0.50, na.rm = T))
    # 2.5% lower bound
    hia_prop_df[[i, 3]]<- as.numeric(quantile(prop_y, 0.025, na.rm = T))
    # 97.5 % upper bound
    hia_prop_df[[i, 4]] <- as.numeric(quantile(prop_y, 0.975, na.rm = T))
    
  } # end state specific
  
  # U.S. estimates for daily and period
  # daily ----
  # add a final sum of each column to get a U.S. estimate
  us_estimate <- cbind("state_sum", sum(hia_daily_df[,2]), sum(hia_daily_df[,3]),
                       sum(hia_daily_df[,4]), df_name) 
  # assign same column name
  colnames(us_estimate) <- colnames(hia_daily_df)
  # bind US estimate to full dataframe
  hia_daily_df <- rbind(hia_daily_df, us_estimate)
  
  # period ----
  # add a final sum of each column to get a U.S. estimate
  us_estimate <- cbind("state_sum", sum(hia_df[,2]), sum(hia_df[,3]),
                       sum(hia_df[,4]), df_name) 
  # assign same column name
  colnames(us_estimate) <- colnames(hia_df)
  # bind US estimate to full dataframe
  hia_df <- rbind(hia_df, us_estimate)
  
  # proportion ----
  # add a final sum of each column to get a U.S. estimate
  us_estimate <- cbind("state_sum", sum(hia_prop_df[,2]), sum(hia_prop_df[,3]),
                       sum(hia_prop_df[,4]), df_name) 
  # assign same column name
  colnames(us_estimate) <- colnames(hia_prop_df)
  # bind US estimate to full dataframe
  hia_prop_df <- rbind(hia_prop_df, us_estimate)
  
  # create environment dataframe with name of original dataset hia is based off
  # period estimates
  hia_p_name <- paste(df_name, 'hia_period', sep = '_')
  assign(hia_p_name, hia_df)
  # write permanent dataset
  #write_path <- paste('./data/mc_estimates/', hia_p_name, '.csv', sep = '')
  #write_csv(hia_df, write_path)
  # daily estimates
  hia_d_name <- paste(df_name, 'hia_daily', sep = '_')
  assign(hia_d_name, hia_daily_df)
  # write permanent dataset
  #write_path <- paste('./data/mc_estimates/', hia_d_name, '.csv', sep = '')
  #write_csv(hia_daily_df, write_path)
  
  # period proportion estimates
  hia_pr_name <- paste(df_name, 'hia_prop', sep = '_')
  assign(hia_pr_name, hia_prop_df)
  # write permanent dataset
  #write_path <- paste('./data/mc_estimates/', hia_pr_name, '.csv', sep = '')
  #write_csv(hia_prop_df, write_path)
  
} # end loop

# end dataframe specific loop
# check to make sure delta ozone and beta are the same units 

# Important Note ----
# rerunning this code will in the future will not produce the exact same
# median and bound values because of the simulated distributions and random replacement
# will not be the same each time, but the values should be very close.

# aggregate dataframes to use for small-multiples figure in results ----
# Daily estimates -----
# create a dataframe with all strata estimates and bind rows for 
# small multiples plot
daily_df <- rbind(marginal_hia_daily, female_hia_daily, male_hia_daily,
                  white_hia_daily, black_hia_daily, hisp_hia_daily) %>% 
  # capitalize first letter of each state
  mutate(median = as.numeric(median),
         lower_bound = as.numeric(lower_bound),
         upper_bound = as.numeric(upper_bound),
         group2 = ifelse(group == "marginal", "Marginal",
         ifelse(group == "female", "Female",
         ifelse(group == "male", "Male",
         ifelse(group == "white", "White",
         ifelse(group == "black", "Black",
         ifelse(group == "hisp", "Hispanic", NA))))))) %>% 
  select(-group) %>% 
  rename(group = group2) 

# write permanent file
write_csv(daily_df, "./data/mc_estimates/mc_daily.csv")

# study period 2005-2014 dataframe ----
period_df <- rbind(marginal_hia_period, female_hia_period, male_hia_period,
  white_hia_period, black_hia_period, hisp_hia_period) %>% 
  # capitalize first letter of each state
  mutate(median = as.numeric(median),
         lower_bound = as.numeric(lower_bound),
         upper_bound = as.numeric(upper_bound),
         group2 = ifelse(group == "marginal", "Marginal",
                  ifelse(group == "female", "Female",
                  ifelse(group == "male", "Male",
                  ifelse(group == "white", "White",
                  ifelse(group == "black", "Black",
                  ifelse(group == "hisp", "Hispanic", NA))))))) %>% 
  select(-group) %>% 
  rename(group = group2) 

# write permanent file
write_csv(period_df, "./data/mc_estimates/mc_period.csv")

# proportion dataframe ----
prop_df <- rbind(marginal_hia_prop, female_hia_prop, male_hia_prop,
                 white_hia_prop, black_hia_prop, hisp_hia_prop) %>% 
  # capitalize first letter of each state
  mutate(median = as.numeric(median),
         lower_bound = as.numeric(lower_bound),
         upper_bound = as.numeric(upper_bound),
         median_100k = as.numeric(median)*100000,
         lower_bound_100k = as.numeric(lower_bound)*100000,
         upper_bound_100k = as.numeric(upper_bound)*100000,
         group2 = ifelse(group == "marginal", "Marginal",
                  ifelse(group == "female", "Female",
                  ifelse(group == "male", "Male",
                  ifelse(group == "white", "White",
                  ifelse(group == "black", "Black",
                  ifelse(group == "hisp", "Hispanic", NA))))))) %>% 
  select(-group) %>% 
  rename(group = group2)

filter(prop_df, group == "marginal")

# write permanent file
write_csv(prop_df, "./data/mc_estimates/mc_prop_100k.csv")
