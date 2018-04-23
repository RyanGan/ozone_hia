# ------------------------------------------------------------------------------
# Title: Script to assemble all state-specific estimates for HIA
# Author: Ryan Gan
# Date: 2/21/17
# ------------------------------------------------------------------------------

# load tidyverse package ----
library(tidyverse)

# import created dataframes ----
# pop at risk
state_par_df <- read_csv("./data/state_strata_pop_at_risk.csv") %>% 
  rename(state = State)

glimpse(state_par_df)

# delta ozone adnd smoky day
ozone_smoke_df <- read_csv("./data/state_delta_o3.csv") %>% 
  rename(state = State)

glimpse(ozone_smoke_df)

# entering yo and se from meta analysis based on CDC document (Table 14)
# https://www.cdc.gov/nchs/data/series/sr_03/sr03_035.pdf
# yo = 0.1067
# yo_se = 0.0047
strata <- unique(state_par_df$strata)

# create dataframe
strata_yo_estimates <- data_frame(strata) %>% 
  # enter yo and se for each strata from table 15
  mutate(yo = ifelse(strata == "marginal", 0.1067,
              ifelse(strata == "female", 0.093,
              ifelse(strata == "male", 0.118, 
              ifelse(strata == "white", 0.094,
              ifelse(strata == "black", 0.152,
              ifelse(strata == "hispanic", 0.125, NA)))))),
      yo_se = ifelse(strata == "marginal", 0.0047,
              ifelse(strata == "female", 0.011,
              ifelse(strata == "male", 0.013 , 
              ifelse(strata == "white", 0.010,
              ifelse(strata == "black", 0.021,
              ifelse(strata == "hispanic", 0.021, NA)))))))

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
