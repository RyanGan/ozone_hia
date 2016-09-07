# ------------------------------------------------------------------------------
# Title: Web-scraping of CDC and BRFSS asthma estimates
# Author: Ryan Gan
# Date: 8/31/16
# ------------------------------------------------------------------------------

# load rvest library for web scrapping
library(rvest)

# Scrap CDC BRFSS for childhood asthma prevalences -----------------------------
url <- paste0("http://www.cdc.gov/asthma/brfss/05/child/lifetime/",
              "tableL1.htm#modalIdString_CDCTable_0")

# table is in there, but I need it in a usable dataframe
asthma_2012 <- url %>% read_html() %>% 
  html_nodes('table') %>% 
  html_table(fill = T, header = T, dec = '.')


head(asthma_2012)

# data.frame does the conversion
asthma_table <- data.frame(asthma_2012)
asthma_table_test <- as.numeric(asthma_table$Prevalence.Number.)

test <- unlist(strsplit(asthma_table$X95..CI...Number., split = "[-]"))
test
regre