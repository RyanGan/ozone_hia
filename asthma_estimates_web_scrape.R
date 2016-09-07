# ------------------------------------------------------------------------------
# Title: Web-scraping of CDC and BRFSS asthma estimates
# Author: Ryan Gan
# Date: 8/31/16
# ------------------------------------------------------------------------------

# load rvest library for web scrapping
library(rvest)

# Scrap CDC BRFSS for childhood asthma prevalences -----------------------------

# define URL path
url <- paste0("http://www.cdc.gov/asthma/brfss/05/child/lifetime/",
              "tableL1.htm#modalIdString_CDCTable_0")

# Use rvest package to extract the table
asthma_html <- url %>% read_html() %>% 
  html_nodes('table') %>% 
  html_table(fill = T, header = T, dec = '.')

# convert html table to data.frame
asthma_table <- data.frame(asthma_html)[1:24,] # exclude Puerto Rico
asthma_table

# output estimate names
estimate_name <- unlist(asthma_table$State)
estimate_name[1] <- "all_us"

# I want the prevalence in estimated numbers
# subset prevalence number
prev_n <- unlist(asthma_table$Prevalence.Number.)
prev_n <- as.numeric(gsub(",", "", prev_test)) # seperate out commas

# split the confidence bounds by '-'
bounds <- unlist(strsplit(asthma_table$X95..CI...Number., split = "[-]"))
bounds

# subset out lower bound vector and convert to numeric
lower <- bounds[c(T,F)]
count_95lower <- as.numeric(gsub(",", "", (gsub("(", "", lower)))) 
# subset out upper bound vector and convert to numeric
upper <- bounds[c(F,T)]
count_95upper <- as.numeric(gsub(",", "", (gsub(")", "", upper))))

state_prev_counts <- data.frame(estimate_name, prev_n, count_95lower, count_95upper)
# calculate SE for each (might not be exact as BRFSS likely uses weights for their SE)
state_prev_counts$se_n <- (state_prev_counts$count_95upper - state_prev_counts$prev_n)/1.96

us_prev <- subset(state_prev_counts, estimate_name == "all_us")
str(state_prev_counts)
head(state_prev_counts)
