# ------------------------------------------------------------------------------
# Title: Monte Carlo approacc to estimating excess asthma emergency depart
#        visits due to wildfire-generated ozone; stratum-specific estimates
# Date Created: 8/3/16      
# ------------------------------------------------------------------------------

# Note: Ryan modification on Dec 1 2016
# Jake, I'm going to remove packages I don't need for MC loop for now.

library(tidyverse)

# Read in stratum dataframes ---------------------------------------------------
# start with one loop first of the total state-specific estimates
total_path <- paste0('./data/Total.csv')
# import csv and make sure variables as numeric like they should be
total <- read_csv(total_path, col_types = "cdddddddd")

# read in african american df
black_path <- paste0('./data/Black.csv')
black <- read_csv(black_path, col_types = "cdddddddd")

# hispanic
hisp_path <- paste0('./data/Hispanic.csv')
hisp <- read_csv(hisp_path, col_types = "cdddddddd")

# white
white_path <- paste0('./data/White.csv')
white <- read_csv(white_path, col_types = "cdddddddd")

# male
m_path <- paste0('./data/Male.csv')
male <- read_csv(m_path, col_types = "cdddddddd")

# female
f_path <- paste0('./data/Female.csv')
female <- read_csv(f_path, col_types = "cdddddddd")

# read smoky days df
sd_path <- './data/smoky_days.csv'
smoky_days <- read_csv(sd_path) %>% rename(n_smoke_days = nSmoky)

# create a list of dataframes to cycle through
df_list <- list(total=total, male=male, female=female, black=black, 
                hisp=hisp, white=white)

df_name <- c('total', 'male', 'female', 'black', 'hisp', 'white')

# join smokey days df to each dataframe
for(q in 1:length(df_list)){

  # loop through df list and join smokey days variable to strata dfs
  data_join_df <- df_list[[q]] %>% 
    full_join(smoky_days, by = "State") %>% filter(complete.cases(.))
  # rename original df lists to contain smoky days val
  assign(names(df_list[q]), data_join_df)
}

# re-create list of dataframes to cycle through now that it has smoke days
df_list <- list(total=total, male=male, female=female, black=black, 
                hisp=hisp, white=white)
# check out first element of list to make sure smoky days variable is there
glimpse(df_list[1]) # it is

# Loop -------------------------------------------------------------------------
# Set seed for reproducibility
set.seed(1234)
# number of interations for HIA MC and simulated distribution sample size
n <- 10000

# start with each dataframe
for(k in 1:length(df_list)){
  # set dataframe to loop through  
  # Note: changing name 'data_frame' to 'df_to_loop' 
  # since it's also a function in dplyr
  df_to_loop <- data.frame(df_list[k]) 
  df_name <- names(df_list[k])
    
  # state-specific loop
  
  # yo and beta as well as their std errors do not change
  # simulate the distributions outside the loop
  # output the value for alabama since they are the same for each state
  
  # these estimate comes from a meta analysis of the estimate and prev.
  yo_total <- df_to_loop[1,2] 
  yo_se_total <- df_to_loop[1,3] 
  
  yo_distribution <- rnorm(n, mean = yo_total, sd = yo_se_total)
  
  # using the same beta estimate for all simulations
  beta_total <- df_to_loop[1, 8]
  beta_se_total <- df_to_loop[1, 9]
  
  # distribution of betas
  beta_distribution <- rnorm(n, mean = beta_total, sd = beta_se_total)
  
  # create an empty matrix to populate with state specific median and bounds
  hia_matrix <- matrix(data=NA ,nrow(df_to_loop), 4) 
  colnames(hia_matrix) <- c("state", "median", "lower_bound", "upper_bound")

  # convert hia_matrix to dataframe
  hia_df <- data.frame(hia_matrix)
  
  # start 2nd loop to run MC for each state
  for(i in 1:nrow(df_to_loop)){

   # feed state name in to hia dataframe
   hia_df[[i, 1]] <- as.character(df_to_loop[i, 1])
   
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
   state_smk_d_n <- df_to_loop[i, 10]
   # create state_specific smoke days Poisson dist
   state_smoky_days_distribution <- rpois(n, state_smk_d_n)
  
   # make empty vector to fill with change in y
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
      #take a random value from the poisson distribution of smoky days
      est_smoky_days <- sample(state_smoky_days_distribution, 1, replace = T)
      
      
      # apply the HIA formula with the randomly selected values above
      # and save the estimate delta y in the vector, n times
      delta_y[[j]] <- (est_y0*(1-exp((-est_beta)*(est_delta_o3)))*pop_at_risk)*est_smoky_days
     
    } # end of inner loop of HIA estimate
   
   # fill in the state-specific median, 2.5% lower bound, and 97.5% upper bound
   # from the monte carlo vector
   # now calculate the median value of delta y
   hia_df[[i, 2]] <- as.numeric(quantile(delta_y, 0.50, na.rm = T))
   # 2.5% lower bound
   hia_df[[i, 3]]<- as.numeric(quantile(delta_y, 0.025, na.rm = T))
   # 97.5 % upper bound
   hia_df[[i, 4]] <- as.numeric(quantile(delta_y, 0.975, na.rm = T))
  
  } # end state specific

# create environment dataframe with name of original dataset hia is based off
hia_name <- paste(df_name, 'hia', sep = '_')
assign(hia_name, hia_df)

# write permanent dataset
write_path <- paste('./data/mc_estimates/', hia_name, '.csv', sep = '')
write.csv(hia_df , file = write_path)

}

# end dataframe specific loop
# check to make sure delta ozone and beta are the same units 

# Important Note ----
# rerunning this code will in the future will not produce the exact same
# median and bound values because of the random replacement factor, but 
# the values should be very close.



