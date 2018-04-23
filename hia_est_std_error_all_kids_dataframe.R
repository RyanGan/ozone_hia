# ------------------------------------------------------------------------------
# Title: Script to assemble all state-specific estimates for HIA
# Author: Ryan Gan
# Date: 2/21/17
# Update: 4/19/18
# ------------------------------------------------------------------------------

# load tidyverse package ----
library(tidyverse)

# import created dataframes ----
# pop at risk; updated to use the new population at risk
state_par_df <- read_csv("./data/state_strata_pop_lt_18.csv")

glimpse(state_par_df)

# delta ozone adnd smoky day
ozone_smoke_df <- read_csv("./data/state_delta_o3.csv") %>% 
  rename(state = State)

glimpse(ozone_smoke_df)

# entering yo and se from meta analysis based on CDC document (Table 14)
# https://www.cdc.gov/nchs/data/series/sr_03/sr03_035.pdf
# yo = 0.0095
# yo_se = 0.0004
strata <- unique(state_par_df$strata)

# create dataframe
strata_yo_estimates <- data_frame(strata) %>% 
  # enter same yo and se for each strata 
  mutate(yo =  0.0095, yo_se = 0.0004)

glimpse(strata_yo_estimates)

# bind all estimates together ----
state_strata_estimates <- state_par_df %>% 
  # join ozone by state
  full_join(ozone_smoke_df, by = "state") %>% 
  # join pop at risk by strata
  full_join(strata_yo_estimates, by = "strata") %>% 
  # enter beta and se for all states and strata
  mutate(beta = 0.0095, beta_se = 0.0040)

glimpse(state_strata_estimates)

# write permanent dataframe
write_csv(state_strata_estimates, "./data/state_strata_hia_estimates.csv")
