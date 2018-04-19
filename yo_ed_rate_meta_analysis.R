# ------------------------------------------------------------------------------
# Title: Using meta-analysis to estimate beta and standard error
# Author: Ryan Gan
# Date: 7/25/16
# Update: 4/19/18
#  -----------------------------------------------------------------------------

# load library rmeta for standard meta analysis
library(rmeta)

# Update Note 4/19/18: It was pointed out by an astute reviewer that our use of
# the rate in children with asthma is not in line with the EPA HIA forumla.
# We have updated estimates to reflect the ED visit rate of asthma per 10,000
# children (Table 13 in the same CDC document)

# Using CDC asthma document, table 13, age (child 0 -17) estimates from 2001-09
# http://www.cdc.gov/nchs/data/series/sr_03/sr03_035.pdf
# Table 13 estimates
year <- c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009)
ed_rate <- c(91.6, 99.7, 95.6, 103.2, 102.4, 80.7, 87.2, 103.1, 111.5)
std_err <- c(11.2, 13.7, 10.3, 14.4, 11.3, 9.5, 12.9, 13.8, 15.3)

# Using meta.summaries procedure to pool and estimate the emergency department
# rate and corresponding standard errors
pooled_estimates <- meta.summaries(ed_rate, std_err, method = 'fixed')
summary(pooled_estimates)

# pooled estimate
yo_est <- pooled_estimates$summary
round(yo_est/10000,4)

# standard error
yo_se <- pooled_estimates$se.summary
# convert to a proportion
round(yo_se/10000,4)
yo_se
# estimates to use in monte carlo simulation
# yo = 0.0095
# yo_se = 0.0004


# smaller varience estiamtes carry more weight.
# random or exact method doesn't change results.

# Meta analysis estimate is lower than averaged estimate; standard error from 
# meta analysis is much tighter compared to averaged standard error

