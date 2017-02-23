# Smoke Ozone and Asthma Health Impact Assessment 

This repository contains project R code for *A National Burden Assessment of Estimated Pediatric Asthma Emergency Department Visits that May be Attributed to Elevated Ozone Levels Associated with the Presence of Smoke* that attempts to quantify the potential burden between elevated ozone associated with wildfire smoke and asthma emergency department visits over a time period of 2005-2014.

## List of R scripts and descriptions
1. asthma_prevalence_web_scrape_meta_analysis.R 
- Scrapes CDC BRFSS website for the yearly child asthma prevalence for each state. It also takes prevalence estimates for each year and pools the estimates using a meta-analysis. Outputs the dataframe "state_strata_pop_at_risk.csv".

2. hia_est_std_error_dataframe.R
- Combines the estimates of central tendancy (pooled/mean), and standard errors for each state and strata of interest in one dataframe to feed in to the Monte Carlo simulation.

3. ozone_estimates_webscrape_meta_analysis.R
- Pulls estimated ozone and smoky days data from Steve Brey's GitHub repository used in *Smoke in the City* paper. 

4. ozone_hia_monte_carlo.R
- Runs the Monte Carlo simulation. Produces the results for the paper.

5. ozone_hia_results.Rmd
- R Markdown file that creates the results used in the paper using the Monte Carlo estimates.

6. understanding_hia_formula.Rmd
- Simulation that creates a hypothetical "true" association that could be measured in epidmeiologic studies, and compares the estimates from HIA simulation to "true" estimates.

7. yo_ed_rate_meta_analysis.R
- Uses CDC document http://www.cdc.gov/nchs/data/series/sr_03/sr03_035.pdf tables 14 and 15 to pool baseline ED visit rates for each strata. 

## Data and key

1. state_delta_o3.csv 
- **State**: state of ozone estimate
- **delta_o3**: Pooled estimate of elevated ozone associated with wildfire smoke based on station estimates in a given state
- **do3_se**: Pooled standard error estimate of elevated ozone associated with wildfire smoke based on station estimates in a given state
- **n_smoky**: Average number of smoky days observed for a given state

2. state_strata_hia_estimates.csv
- **state**: state for HIA input
- **strata**: strata for HIA input
- **pop_at_risk**: Population at risk. This is the count of prevalent children with asthma
- **par_se**: Standard error for the pop_at_risk variable
- **delta_o3**: See key for state_delta_o3.csv
- **do3_se**: See key for state_delta_o3.csv
- **n_smoky**: See key for state_delta_o3.csv
- **yo**: Baseline yearly ED visit rate for children with asthma
- **yo_se**: Standard error for yo
- **beta**: Beta estimate used in HIA. Natural-log transformed relative risk.
- **beta_se**: Standard error for the beta estimate

3. state_strata_pop_at_risk.csv
- **state**: Same as above.
- **strata**: strata for HIA inpput
- **pop_at_risk**: See data key for state_strata_hia_estimate.csv
- **par_se**: See data key for state_strata_hia_estimate.csv

4. mc_estimates contains data from Monte Carlo simulations

