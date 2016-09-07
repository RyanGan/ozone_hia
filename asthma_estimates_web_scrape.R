# ------------------------------------------------------------------------------
# Title: Web-scraping of CDC and BRFSS asthma estimates
# Author: Ryan Gan
# Date: 8/31/16
# ------------------------------------------------------------------------------

# load rvest library for web scrapping
library(rvest)

# General process
# 1. Bring in each BRFSS HTML table
# 2. Find the prevalence number and calculated standard error for all US and 
#    each state for all the years available
# 3. Run meta-analysis for all US then each US to get a pooled estimate and se

# state list
state_abbrev <- c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
                  "ID", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI",
                  "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
                  "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
                  "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

# list of BRFSS URLs to loop through
year_url <- c("2013", "2012", "2011", "2010", "09", "08", "07", "06", "05")


year <- c("2013", "2012", "2011", "2010", "2009", "2008", "2007", "2006", "2005")

yearly_prev_se_est <- data.frame()

for (i in 1:length(year)){

year_var <- replicate(length(state_abbrev), year[i])
prev_n_est <- replicate(length(state_abbrev), NA)
prev_se_est <- replicate(length(state_abbrev), NA)

df <- data.frame(year_var, state_abbrev, prev_n_est, prev_se_est)

yearly_prev_se_est <- rbind(yearly_prev_se_est, df)

}
# create an empty dataframe to fill with prevalence n and se 


# Loop to scrape CDC BRFSS for childhood asthma prevalences --------------------



# define URL path for current asthma prev tables
url <- paste("http://www.cdc.gov/asthma/brfss/", year_url, "/child/tableC1.htm",
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
estimate_name <- unlist(asthma_table$State)
# Replace 'Total **' with 'US' label
estimate_name[1] <- "US"

# I want the prevalence in estimated numbers
# subset prevalence number
prev_n <- unlist(asthma_table$Prevalence.Number.)
prev_n <- as.numeric(gsub(",", "", prev_n)) # seperate out commas

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

state_prev_counts <- data.frame(estimate_name, prev_n, count_95lower, count_95upper)
# calculate SE for each (might not be exact as BRFSS likely uses weights for their SE)
state_prev_counts$se_n <- (state_prev_counts$count_95upper - state_prev_counts$prev_n)/1.96

us_prev <- subset(state_prev_counts, estimate_name == "US")

