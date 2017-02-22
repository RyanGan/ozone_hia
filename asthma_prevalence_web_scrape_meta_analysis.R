# ------------------------------------------------------------------------------
# Title: Web-scraping of CDC and BRFSS asthma estimates
# Author: Ryan Gan
# Date: 2/21/17
# ------------------------------------------------------------------------------

# load rvest library for web scrapping
library(rvest)
library(XML)
# load rmeta for meta analysis
library(rmeta)
library(tidyverse)

# General process
# 1. Bring in each BRFSS HTML table
# 2. Find the prevalence number and calculated standard error for all US and 
#    each state for all the years available
# 3. Run meta-analysis for all US then each US to get a pooled estimate and se


# state list
state_name <- c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
           "ID", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI",
           "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
           "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
           "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "US")

year <- c("2014", "2013", "2012", "2011", "2010", "2009", 
          "2008", "2007", "2006", "2005")


# Marginal Prevalence of Asthma Estimate -----------------------------------------------------

# Loop to scrape CDC BRFSS for childhood asthma prevalences 

# list of BRFSS URLs to loop through (05 - 10 has slightly different name)
year_url <- c("2014/child", "2013/child", "2012/child", "2011/child", 
              "2010/child/current", "09/child/current", "08/child/current", 
              "07/child/current", "06/child/current", "05/child/current")

# create empty dataframe
yearly_prev_se_est <- data.frame()

for(j in 1:length(year_url)){

  # define URL path for current asthma prev tables
  url <- paste("http://www.cdc.gov/asthma/brfss/", year_url[j], "/tableC1.htm",
               sep = "")
  
  # Use rvest package to extract the table
  asthma_html <- url %>% read_html() %>% 
    html_nodes('table') %>% 
    html_table(fill = T, header = T, dec = '.')
  
  # convert html table to data.frame
  asthma_table <- data.frame(asthma_html) 
  # exclude Puerto Rico, Alaska, and Hawaii
  asthma_table <- subset(asthma_table, State != "PR" & State != "Territories"
                         & State != "Territory" & State != "HI" & State != "AK")
  
  # output estimate names
  state <- unlist(asthma_table$State)
  # Replace 'Total **' with 'US' label
  state[1] <- "US"
  
  # I want the prevalence in estimated numbers
  # subset prevalence number (2014 has a slightly different name on webscrape)
    if(j == 1){
      prev_n_est <- unlist(asthma_table$Prevalence..Number.)
    } else {
      prev_n_est <- unlist(asthma_table$Prevalence.Number.) } # end if else
  
  prev_n_est <- as.numeric(gsub(",", "", prev_n_est)) # seperate out commas
  
  # split the confidence bounds by '-'
  # same issue as before. name differs slightly by table year
    if(j == 1){
    bounds <- unlist(strsplit(asthma_table$X95..CI....Number., split = "-")) 
    } else {  
    bounds <- unlist(strsplit(asthma_table$X95..CI...Number., split = "-")) } 
  
  
  # subset out lower bound vector and convert to numeric
  lower <- bounds[c(T,F)]
  lower # i need to escape left '(', need '\\('
  count_95lower <- as.numeric(gsub(",", "", (gsub("\\(", " ", lower)))) 
  # subset out upper bound vector and convert to numeric
  upper <- bounds[c(F,T)]
  count_95upper <- as.numeric(gsub(",", "", (gsub(")", "", upper))))
  
  # replicate year length of times state is repeated
  year_var <- replicate(length(state), year[j])
  
  state_prev_counts <- data.frame(year_var, state, prev_n_est, 
                                  count_95lower, count_95upper)
  # calculate SE for each 
  # might not be exact as BRFSS likely uses weights for their SE
  # but should be close due to large sample size
  state_prev_counts$prev_se_est <- (state_prev_counts$count_95upper - 
                                      state_prev_counts$prev_n)/1.96
  
  
  yearly_prev_se_est <- rbind(yearly_prev_se_est, state_prev_counts)

}

summary(yearly_prev_se_est) # dataframe of state prevalence and se estimates

state_count <- aggregate(yearly_prev_se_est, by = list(yearly_prev_se_est$state), 
                        FUN = length)[1:2]

state_list <- state_count[,1]
state_est_count <- state_count[,2]
pooled_prev_n <- replicate(length(state_list), NA)
pooled_se_n <- replicate(length(state_list), NA)

# Loop to run meta analyses on state 

# empty dataframe of pooled estimates
us_state_meta_results <- data.frame(state_list, state_est_count, pooled_prev_n,
                                    pooled_se_n)

# I need to subset each strata and the years available to run a meta analysis 
for(k in 1:length(unique(yearly_prev_se_est$state))){ 

  # subset the strata values
  strata_estimates <- subset(yearly_prev_se_est, state == state_list[k])
  
  # output the strata-specific prevalence estimates in to a vector
  prev_n_vector <- strata_estimates[,3]
  # output the strata-specific standard errors
  prev_std_err_vector <- strata_estimates[,6]
  # run meta analysis of prevalence estimates and standard errors
  pooled_estimates <- meta.summaries(prev_n_vector, prev_std_err_vector, 
                                     # fixed and random produce same results
                                     method = 'fixed') 
  # I need to extract the final pooled prevalence and bounds
  pooled_prev <- pooled_estimates$summary
  pooled_se <- pooled_estimates$se.summary
  
  # bind in these values to the specific strata/state
  us_state_meta_results[k, 3] <- pooled_prev
  us_state_meta_results[k, 4] <- pooled_se
  
}

# marinal estimates for each state
marginal_prev_meta_results <- us_state_meta_results %>% 
  # adding in a strata idicator
  mutate(strata = "marginal")


# check US estimate
us <- subset(yearly_prev_se_est, state == "US")
us
# meta analysis is the same as produced in the above loop; it's working.
us_meta <- meta.summaries(us$prev_n_est, us$prev_se_est, method = 'fixed')
str(us_meta)



# Asthma Prevalence by Sex -----------------------------------------------------
# Sex and race strata estimates are in a different format in the table, so they
# cannot be added as another loop and need a slightly modified code

# Comes from BRFSS table current 2.2 (tablec22.htm)
# example of 2014 current asthma by sex 
# https://www.cdc.gov/asthma/brfss/2014/child/tablec22.htm

# empty dataframe 
yearly_prev_se_est_sex <- data.frame()

# webscrape loop for sex estimates 
for(j in 1:length(year_url)){

  # define URL path for current asthma prev tables
  url <- paste("http://www.cdc.gov/asthma/brfss/", year_url[j], "/tablec22.htm",
               sep = "")
  
  # Use rvest package to extract the table
  asthma_html <- url %>% read_html() %>% 
    html_nodes('table') %>% 
    html_table(fill = T, header = T, dec = '.')
  
  # convert html table to data.frame
  asthma_table <- data.frame(asthma_html) 
  # exclude Puerto Rico, Alaska, and Hawaii
  asthma_table <- subset(asthma_table, State != "PR" & State != "Territories"
                         & State != "Territory" & State != "HI" & State != "AK")
  
  # output estimate names
  state <- unlist(asthma_table$State)
  # Replace 'Total **' with 'US' label
  state[1] <- "US"
  
  head(asthma_table)
  
  # same issue as before where 2014 has a slightly different way to name cols
  # numbers and CI for males and females are in different columns
  # also has different names in webscrape
  # males
  if(j == 1) {
  male_prev_n_est <- unlist(asthma_table$Male.Prevalence)
  # same issue as before. female and male in different cols
  male_bounds <- unlist(strsplit(asthma_table$Male.95..CI.., split = "-")) 
  # females
  female_prev_n_est <- unlist(asthma_table$Female.Prevalence)
  # female conf bounds
  female_bounds <- unlist(strsplit(asthma_table$Female.95..CI.., split = "-")) 
  } else {
  # males
  male_prev_n_est <- unlist(asthma_table$MalePrevalence)
  male_bounds <- unlist(strsplit(asthma_table$Male95..CI.., split = "-")) 
  # females
  female_prev_n_est <- unlist(asthma_table$FemalePrevalence)   
  # female conf bounds
  female_bounds <- unlist(strsplit(asthma_table$Female95..CI.., split = "-")) 
  }
  
  # seperate out commas
  male_prev_n_est <- as.numeric(gsub(",", "", male_prev_n_est)) 
  female_prev_n_est <- as.numeric(gsub(",", "", female_prev_n_est))
  
  # split the confidence bounds by '-'
  # subset out lower bound vector and convert to numeric
  male_lower <- male_bounds[c(T,F)]
  # lower # i need to escape left '(', need '\\('
  male_count_95lower <- as.numeric(gsub(",", "", (gsub("\\(", " ", male_lower)))) 
  # subset out upper bound vector and convert to numeric
  male_upper <- male_bounds[c(F,T)]
  male_count_95upper <- as.numeric(gsub(",", "", (gsub(")", "", male_upper))))
  
  # create a dataframe for males
  # replicate year length of times state is repeated
  year_var <- replicate(length(state), year[j])
  # replicate strata statement time state is repeated
  male_strata <- replicate(length(state), "male")
  # create a dataframe for males
  male_state_prev_counts <- data.frame(year_var, state, male_prev_n_est, 
                                  male_count_95lower, male_count_95upper,
                                  male_strata)

  # subset out lower bound vector and convert to numeric
  female_lower <- female_bounds[c(T,F)]
  # lower # i need to escape left '(', need '\\('
  female_count_95lower <- as.numeric(gsub(",", "", (gsub("\\(", " ", female_lower)))) 
  # subset out upper bound vector and convert to numeric
  female_upper <- female_bounds[c(F,T)]
  female_count_95upper <- as.numeric(gsub(",", "", (gsub(")", "", female_upper))))
  
  # create a dataframe for females
  female_strata <- replicate(length(state), "female")
  female_state_prev_counts <- data.frame(year_var, state, female_prev_n_est, 
                                  female_count_95lower, female_count_95upper,
                                  female_strata)
  
  # rename columns for row bind
  colnames(male_state_prev_counts) <- c("year_var", "state", "prev_n_est",
                                        "count_95lower", "count_95upper",
                                        "strata")
  
  colnames(female_state_prev_counts) <- c("year_var", "state", "prev_n_est",
                                          "count_95lower", "count_95upper",
                                          "strata")
  # rowbind the two strata
  state_prev_counts <- rbind(male_state_prev_counts, female_state_prev_counts)
  
  # calculate SE for each 
  # might not be exact as BRFSS likely uses weights for their SE
  # but should be close due to large sample size
  state_prev_counts$prev_se_est <- (state_prev_counts$count_95upper - 
                                      state_prev_counts$prev_n)/1.96
  
  
  yearly_prev_se_est_sex <- rbind(yearly_prev_se_est_sex, state_prev_counts)

}


summary(yearly_prev_se_est_sex) # dataframe of state prevalence and se estimates


# Loop to run meta analyses on sex strata 

# first loop is by sex
sex_strata <- c("male", "female")

for(i in 1:2){

df_to_loop <- yearly_prev_se_est_sex %>% 
  filter(strata == sex_strata[i])

# empty dataframe of pooled estimates
sex_state_meta_results <- df_to_loop %>% 
  group_by(state) %>% 
  summarise(state_count = n()) %>% 
  mutate(pooled_prev_n,
         pooled_se_n,
         strata = sex_strata[i])

state_list <- as.character(sex_state_meta_results$state)

head(sex_state_meta_results)

  # I need to subset each strata and the years available to run a meta analysis 
  for(k in 1:nrow(sex_state_meta_results)){ 

    # subset the strata values
    strata_estimates <- filter(df_to_loop, state == state_list[k])
    
    # run meta analysis of prevalence estimates and standard errors
    pooled_estimates <- meta.summaries(strata_estimates$prev_n_est, 
                                       strata_estimates$prev_se_est, 
                                       # fixed and random produce same results
                                       method = 'fixed') 
    # I need to extract the final pooled prevalence and bounds
    pooled_prev <- pooled_estimates$summary
    pooled_se <- pooled_estimates$se.summary
    
    # bind in these values to the specific strata/state
    sex_state_meta_results[k, 3] <- pooled_prev
    sex_state_meta_results[k, 4] <- pooled_se
    
  } # end state loop

assign(paste0(sex_strata[i], "_state_meta_results"), sex_state_meta_results)

} # end sex strata 



# Race strata estimates of prevalence ------------------------------------------

race_name <- c("white", "black", "hispanic")
race_strata <- c("White NH", "Black NH", "Hispanic")

# empty dataframe
yearly_prev_se_est_race <- data.frame()

# comes from table C5 of BRFSS data
# This time race is for each row
for(j in 1:length(year_url)){

  # define URL path for current asthma prev tables
  url <- paste("http://www.cdc.gov/asthma/brfss/", year_url[j], "/tableC5.htm",
               sep = "")
  
  # Use rvest package to extract the table
  asthma_html <- url %>% read_html() %>% 
    html_nodes('table') %>% 
    html_table(fill = T, header = T, dec = '.')
  
  # convert html table to data.frame
  asthma_table <- data.frame(asthma_html) 
  # exclude Puerto Rico, Alaska, and Hawaii
  asthma_table <- subset(asthma_table, State != "PR" & State != "Territories"
                         & State != "Territory" & State != "HI" & State != "AK")
  head(asthma_table)
  
  # loop through each race
  for(k in 1:length(race_strata)){

  # only retain rows with a standard error (and 95%CI)
  df_to_loop <- filter(asthma_table, Race.Ethnicity == race_strata[k] & 
                      State != "Total **")
  
  # filter by standard error, this is different based on year too
  if(j == 1){
  df_to_loop <- df_to_loop %>% filter(!is.na(as.numeric(Standard.Error))) 
  } else { 
  df_to_loop <- df_to_loop %>% filter(!is.na(as.numeric(StandardError)))  
  }
  
  # output estimate names
  state <- unlist(df_to_loop$State)

  # I want the prevalence in estimated numbers
  # subset prevalence number (2014 has a slightly different name on webscrape)
    if(j == 1){
      prev_n_est <- unlist(df_to_loop$Prevalence..Number.)
    } else {
      prev_n_est <- unlist(df_to_loop$Prevalence.Number.) } # end if else
  
  prev_n_est <- as.numeric(gsub(",", "", prev_n_est)) # seperate out commas
  
  head(df_to_loop)
  # split the confidence bounds by '-'
  # same issue as before. name differs slightly by table year
  if(j == 1){
      bounds <- unlist(strsplit(df_to_loop$X95..CI...Number., split = "-")) 
    } else {
      bounds <- unlist(strsplit(df_to_loop$X95..CI..Number., split = "-"))} # end if else
    

  # subset out lower bound vector and convert to numeric
  lower <- bounds[c(T,F)]
  lower # i need to escape left '(', need '\\('
  count_95lower <- as.numeric(gsub(",", "", (gsub("\\(", " ", lower)))) 
  
  prev_n_est
  count_95lower
  # subset out upper bound vector and convert to numeric
  upper <- bounds[c(F,T)]
  count_95upper <- as.numeric(gsub(",", "", (gsub(")", "", upper))))
  
  # replicate year length of times state is repeated
  year_var <- replicate(length(state), year[j])
  # strata
  strata <- replicate(length(state), race_name[k]) 
  
  
  state_prev_counts <- data.frame(year_var, state, prev_n_est, 
                                  count_95lower, count_95upper, strata)
  # calculate SE for each 
  # might not be exact as BRFSS likely uses weights for their SE
  # but should be close due to large sample size
  state_prev_counts$prev_se_est <- (state_prev_counts$count_95upper - 
                                      state_prev_counts$prev_n)/1.96
  
  
  yearly_prev_se_est_race <- rbind(yearly_prev_se_est_race, state_prev_counts)
  } # end race starata
} # end year strata

summary(yearly_prev_se_est_race) # dataframe of state prevalence and se estimates


# Loop to run meta analyses on race strata 

for(i in 1:3){

df_to_loop <- yearly_prev_se_est_race %>% 
  filter(strata == race_name[i]) 

# empty dataframe of pooled estimates
race_state_meta_results <- df_to_loop %>% 
  group_by(state) %>% 
  summarise(state_count = n()) %>% 
  mutate(pooled_prev_n = NA,
         pooled_se_n = NA,
         strata = race_name[i])

state_list <- as.character(race_state_meta_results$state)


  # I need to subset each strata and the years available to run a meta analysis 
  for(k in 1:nrow(race_state_meta_results)){ 

    # subset the strata values
    strata_estimates <- filter(df_to_loop, state == state_list[k])
    
    # run meta analysis of prevalence estimates and standard errors
    pooled_estimates <- meta.summaries(strata_estimates$prev_n_est, 
                                       strata_estimates$prev_se_est, 
                                       # fixed and random produce same results
                                       method = 'fixed') 
    # I need to extract the final pooled prevalence and bounds
    pooled_prev <- pooled_estimates$summary
    pooled_se <- pooled_estimates$se.summary
    
    # bind in these values to the specific strata/state
    race_state_meta_results[k, 3] <- pooled_prev
    race_state_meta_results[k, 4] <- pooled_se
    
  } # end state loop

assign(paste0(race_name[i], "_state_meta_results"), race_state_meta_results)

} # end race strata 

# rename some variables
marginal_prev_meta_results <- rename(marginal_prev_meta_results, 
                                     state_count = state_est_count,
                                     state = state_list)
# force all states in each dataframe to be character
marginal_prev_meta_results$state <- as.character(marginal_prev_meta_results$state)
female_state_meta_results$state <- as.character(female_state_meta_results$state)
male_state_meta_results$state <- as.character(male_state_meta_results$state)
white_state_meta_results$state <- as.character(white_state_meta_results$state)
black_state_meta_results$state <- as.character(black_state_meta_results$state)
hispanic_state_meta_results$state <- as.character(hispanic_state_meta_results$state)

# merege state lowercase name to abbreviation
state_lowercase <- tolower(state.name)
state_abbr <- state.abb
# create key
state_key <- as.data.frame(cbind(state_lowercase, state_abbr))
state_key$state_abbr <- as.character(state_key$state_abbr)
state_key <- rename(state_key, state = state_lowercase)

# Bind all dataframes of prevalence in to one
strata_pooled_prev_df <- bind_rows(marginal_prev_meta_results,
  female_state_meta_results, male_state_meta_results, white_state_meta_results,
  black_state_meta_results, hispanic_state_meta_results) %>% 
  rename(state_abbr = state,
         pop_at_risk = pooled_prev_n,
         par_se = pooled_se_n) %>% 
  # set DC to MD
  mutate(state_abbr = ifelse(state_abbr == "DC", "MD", state_abbr)) %>% 
  # join state names to abbreviations
  full_join(state_key, by = "state_abbr") %>% 
  select(state, strata, pop_at_risk, par_se) %>% 
  filter(complete.cases(.))

# check dataframe
glimpse(strata_pooled_prev_df)
summary(strata_pooled_prev_df)

# output dataframe
write_csv(strata_pooled_prev_df, "./data/state_strata_pop_at_risk.csv")

