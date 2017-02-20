# ------------------------------------------------------------------------------
# Title: Pooled estimates of delta ozone and standard error
# Author: Jacob Pratt and Ryan Gan
# Date Created: 2/20/2017
# R Versions 3.3.2
# ------------------------------------------------------------------------------

# libraries ----
library(RCurl) # pull data from github
library(tidyverse)
library(maptools)
library(sp)
library(maps)
library(rmeta)
library(broom)


# pull ozone estimates from Steve's GitHub page ----
ozone_path <- getURL(paste0("https://raw.githubusercontent.com/stevenjoelbrey/",
                "SmokeInTheCity/master/figures/PM2.5_non_FRM_Mass_24hr_Arithmetic",
                ".Mean_allSummers_ecmwf_nDays%3D10_TSdFactor%3D0/summary_data.csv"))

ozone <- read.csv(text = ozone_path)

summary(ozone)

# function to find state ----

# pull out lat /lon to determine what state each measurement corresponds 
pointsDF <- ozone[c(4,5)]


latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

# Test the function using points in Wisconsin and Oregon.
testPoints <- data.frame(x = c(-90, -120), y = c(44, 44))

latlong2state(testPoints)

# some sites end up missing from dataset that should be assigned
# example berkeley
# looks like this can be fixed by rounding a bit
berk <- data.frame(x = -122, y=40)

latlong2state(berk)

# create a rounded lat lon
ozone <- ozone %>% mutate(lon2 = round(lon,1),
                          lat2 = round(lat,1))

summary(ozone)

# Assign states based on lat/lon from Steve's site locations using exact lat/lon
ozone$state <- latlong2state(pointsDF)
ozone$state2 <- latlong2state(ozone[c(17,18)])

xtabs(~ state + state2, ozone) # some states likely can't be assigned a state since
# they don't fall in the state polygon as it's not a finest resolution

# fill in state if missing a value
ozone <- ozone %>% mutate(new_state = ifelse(is.na(state), state2, state))
xtabs(~ new_state, ozone)

# check states 
# looks like some states will just be missing unless I have a very detailed
# shapefile
summary(as.factor(ozone$new_state)) 

# limit to complete case and calculate standard error for each site
ozone2 <- ozone %>% select(-lon2, -lat2, -state2, -new_state) %>% 
  filter(complete.cases(.)) %>% 
  mutate(std_error = (confInt_upper - differenceOfMeans)/1.96)

summary(ozone2)


# meta analysis to pool station estimates ----
# set up empty matrix
state_ozone <- matrix(ncol=4, nrow = length(unique(ozone2$state)))
colnames(state_ozone) <- c("state", "delta_o3", "do3_se", "n_smoky")
# convert to dataframe
state_ozone <- as.data.frame(state_ozone)

# vector of state names
state_vector <- unique(ozone2$state)


# state pooled est and std error ----
for(i in 1:length(state_vector)){

  state_ozone[i,1] <- state_name <- state_vector[i]
  
  # subset ozone dataset to just state
  state_df <- subset(ozone2, state == state_name)
  
  # meta analysis
  pooled_estimates <- meta.summaries(differenceOfMeans, std_error, 
                                     method = "fixed", data = state_df)
  
  pooled_ozone <- pooled_estimates$summary
  pooled_se <- pooled_estimates$se.summary
  
  # fill matrix with state specific estimates
  state_ozone[i, 2] <- pooled_ozone
  state_ozone[i, 3] <- pooled_se
  
  # calculate smoky days
  # mean of smoky days
  state_ozone[i, 4] <- state_df %>% summarise(n_smk_days_mean = mean(nSmoky))

}

# save dataframe
write_csv(state_ozone, "./data/state_delta_o3.csv")



