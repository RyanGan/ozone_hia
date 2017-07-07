# ------------------------------------------------------------------------------
# Title: Using meta-analysis to estimate beta and standard error
# Author: Ryan Gan
# Date: 7/25/16
#  -----------------------------------------------------------------------------

# load library rmeta
library(rmeta)
library(lme4)

# Using CDC asthma document, table 14, age (child 0 -17) estimates from 2001-09
# http://www.cdc.gov/nchs/data/series/sr_03/sr03_035.pdf
# Table 14 estimates
year <- c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009)
ed_rate <- c(10.5, 12.0, 11.2, 12.2, 11.5, 8.7, 9.6, 11.0, 11.6)
std_err <- c(1.3, 1.7, 1.3, 1.8, 1.3, 1.1, 1.5, 1.6, 1.7)

# Using meta.summaries procedure to pool and estimate the emergency department
# rate and corresponding standard errors
pooled_estimates <- meta.summaries(ed_rate, std_err, method = 'fixed')
summary(pooled_estimates)

# pooled estimate
yo_est <- pooled_estimates$summary
round(yo_est/100,4)

# standard error
yo_se <- pooled_estimates$se.summary
# convert to a percentage
round(yo_se/100,4)

# estimates to use in monte carlo simulation
# yo = 0.1067
# yo_se = 0.0047


# smaller varience estiamtes carry more weight.
# random or exact method doesn't change results.

# Meta analysis estimate is lower than averaged estimate; standard error from 
# meta analysis is much tighter compared to averaged standard error


