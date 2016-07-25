# ------------------------------------------------------------------------------
# Title: Using meta-analysis to estimate beta and standard error
# Author: Ryan Gan
# Date: 7/25/16
#  -----------------------------------------------------------------------------

# load library rmeta
library(rmeta)

# Using CDC asthma document, talbe 14, age (child 0 -17) estimates from 2001-09
# http://www.cdc.gov/nchs/data/series/sr_03/sr03_035.pdf
# Table 14 estimates
year <- c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009)
ed_rate <- c(10.5, 12.0, 11.2, 12.2, 11.5, 8.7, 9.6, 11.0, 11.6)
std_err <- c(1.3, 1.7, 1.3, 1.8, 1.3, 1.1, 1.5, 1.6, 1.7)

# Using meta.summaries procedure to pool and estimate the emergency department
# rate and corresponding standard errors
?meta.summaries

pooled_estimates <- meta.summaries(ed_rate, std_err, method = 'exact')
summary(pooled_estimates)

# Estimates are 10.67 pooled ed rate over this time, with 95% CI of 9.74 - 11.6
(9.74 - 10.67)/-1.96
(11.6 -10.67)/1.96

# std error from meta analysis if 0.474

# Compared to averaging estimate and standard error from 2001 to 2009
# avg ed rate
sum(ed_rate)/9
# avg std error
sum(std_err)/9

# 95% CI
# upper
10.922 + (1.96*1.477)
# lower
10.922 - (1.96*1.477)

# Meta analysis estimate is lower than averaged estimate; standard error from 
# meta analysis is much tighter compared to averaged standard error