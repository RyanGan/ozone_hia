# ------------------------------------------------------------------------------
# Title: Wrangling of 2010 State Population Census Estimates
# Author: Ryan Gan
# Date: 4/19/18
# ------------------------------------------------------------------------------

# Note 4/19/18: It was pointed out by an astute reviewer that we are accounting
# for ED visits in multiple ways. After careful thought, we decided to replace
# the population at risk using all persons under 18 in a state, rather than 
# the estimated number of children with asthma. This code prepares 2010 census
# data to be used as the population at risk estimates.

# library -----
library(tidyverse)

# Downloaded 2010 census estimate for age/race/sex from following link:
# https://www2.census.gov/programs-surveys/popest/datasets/2010-2011/state/asrh/

# read in state fips key
fips_key <- read_delim("./data/state_fips_key.txt", delim = "|") %>% 
  # to lower all state names to match other files
  mutate(names = tolower(STATE_NAME)) %>% 
  # filter to under state fips under 56 to get rid of islands
  filter(as.numeric(STATE) <= 56) %>% 
  # get rid of DC=11, alaska == 02, and hawaii = 15
  filter(!(STATE %in% c("02", "11", "15")))

# reading in population 2010 estimates and reducing to estimate strata
pop2010 <- read_csv("./data/sc-est2011-alldata5-all.csv") %>% 
  # filter to 18 and under
  filter(AGE < 18) %>% 
  # filter to states in continental US
  filter(STATE %in% fips_key$STATE) %>% 
  # grouping by state, sex, origin, and race to sum up 2010 census estimates
  group_by(STATE, SEX, ORIGIN, RACE) %>% 
  # sum census estimates for groups under 18
  summarize(pop2010 = sum(CENSUS2010POP))

summary(pop2010)
# estimating marginal/total population of each state under age 18 
total_pop <- pop2010 %>% 
  filter(SEX == 0 & ORIGIN == 0) %>%  
  group_by(STATE) %>% 
  summarise(pop_at_risk = sum(pop2010)) %>% 
  mutate(strata = "marginal")

# population estimates for sex under age 18
sex_pop <- pop2010 %>% 
  filter(SEX != 0 & ORIGIN == 0) %>% 
  group_by(STATE, SEX) %>% 
  summarise(pop_at_risk = sum(pop2010)) %>% 
  mutate(strata = case_when(SEX == 1 ~ "male",
                            SEX == 2 ~ "female")) %>% 
  select(-SEX)

# population estimates for white/black race under age 18
race_pop <- pop2010 %>% 
  filter(SEX == 0 & ORIGIN == 0 & RACE %in% c(1:2)) %>% 
  group_by(STATE, RACE) %>% 
  summarise(pop_at_risk = sum(pop2010)) %>% 
  mutate(strata = case_when(RACE == 1 ~ "white",
                            RACE == 2 ~ "black")) %>% 
  select(-RACE)

# hispanic population under 18
hisp_pop <- pop2010 %>% 
  filter(SEX == 0 & ORIGIN == 2) %>% 
  group_by(STATE) %>% 
  summarise(pop_at_risk = sum(pop2010)) %>% 
  mutate(strata = "hispanic") 

# bind all groups together
pop_at_risk <- bind_rows(total_pop, sex_pop, race_pop, hisp_pop) %>% 
  # join to state
  left_join(fips_key, by = "STATE") %>% 
  mutate(state = names) %>% 
  select(state, strata, pop_at_risk)
  
# write file
write_csv(pop_at_risk, "./data/state_strata_pop_lt_18.csv")
