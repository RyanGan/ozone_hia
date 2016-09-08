# ------------------------------------------------------------------------------
# Title: Web-scraping of CDC and BRFSS asthma estimates
# Author: Ryan Gan
# Date: 8/31/16
# ------------------------------------------------------------------------------

# load rvest library for web scrapping
library(rvest)
# load rmeta for meta analysis
library(rmeta)

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

year <- c("2013", "2012", "2011", "2010", "2009", "2008", "2007", "2006", "2005")

# create empty dataframe
yearly_prev_se_est <- data.frame()

# for (i in 1:length(year)){
# 
# year_var <- replicate(length(state_abbrev), year[i])
# prev_n_est <- replicate(length(state_abbrev), NA)
# prev_se_est <- replicate(length(state_abbrev), NA)
# 
# df <- data.frame(year_var, state_name, prev_n_est, prev_se_est)
# 
# yearly_prev_se_est <- rbind(yearly_prev_se_est, df)
# 
# }
# create an empty dataframe to fill with prevalence n and se 


# Loop to scrape CDC BRFSS for childhood asthma prevalences --------------------

# list of BRFSS URLs to loop through (05 - 10 has slightly different name)
year_url <- c("2013/child", "2012/child", "2011/child", "2010/child/current", 
              "09/child/current", "08/child/current", "07/child/current", 
              "06/child/current", "05/child/current")

for (j in 1:length(year_url)){

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
# subset prevalence number
prev_n_est <- unlist(asthma_table$Prevalence.Number.)
prev_n_est <- as.numeric(gsub(",", "", prev_n_est)) # seperate out commas

# split the confidence bounds by '-'
bounds <- unlist(strsplit(asthma_table$X95..CI...Number., split = "-"))
bounds

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
# calculate SE for each (might not be exact as BRFSS likely uses weights for their SE)
state_prev_counts$prev_se_est <- (state_prev_counts$count_95upper - 
                                    state_prev_counts$prev_n)/1.96


yearly_prev_se_est <- rbind(yearly_prev_se_est, state_prev_counts)

}

summary(yearly_prev_se_est) # dataframe of state prevalence and se estimates

state_count <- aggregate(yearly_prev_se_est, by = list(yearly_prev_se_est$state), 
                        FUN = length)[1:2]

state_list <- state_count[,1]
state_est_count <- state_count[,2]

# loop to run meta analyses

# I need to subset each strata and the years available to run a meta analysis
for
