# ------------------------------------------------------------------------------
# Title: Pooled estimates of delta ozone and standard error
# Author: Jacob Pratt and Ryan Gan
# Date Created: 2/20/2017
# R Versions 3.3.2
# ------------------------------------------------------------------------------

# libraries ----
library(RCurl) # pull data from github
library(tidyverse) # dplyr function
library(maptools) # spatial
library(sp) # spatial
library(maps) # spatial
library(rmeta) # meta analysis



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

# Assign states based on lat/lon from Steve's site locations using exact lat/lon
ozone$State <- latlong2state(ozone[c(4,5)])

summary(as.factor(ozone$State)) # 14 station NAs

# spent too much time trying to figure out how to not miss sites on the coast
# hard coding state location in for missing state
# find missing states in ozone df
state_missing <- ozone %>% filter(is.na(State))

# view stations missing states
state_missing[, c(2:5, 16)]

# monitors in cali
ozone$State[ozone$stationPlotted == "060012004-01"] <- "california"
ozone$State[ozone$stationPlotted == "060950004-01"] <- "california"
ozone$State[ozone$stationPlotted == "060950006-01"] <- "california"
# florida
ozone$State[ozone$stationPlotted == "120050006-01"] <- "florida"
ozone$State[ozone$stationPlotted == "121130015-01"] <- "florida"
ozone$State[ozone$stationPlotted == "121290001-01"] <- "florida"
# maine
ozone$State[ozone$stationPlotted == "230050029-01"] <- "maine"
ozone$State[ozone$stationPlotted == "230090103-01"] <- "maine"
# louisiana
ozone$State[ozone$stationPlotted == "280470008-01"] <- "louisiana"
# texas (don't mess with it)
ozone$State[ozone$stationPlotted == "480610006-01"] <- "texas"
ozone$State[ozone$stationPlotted == "481410055-01"] <- "texas"
ozone$State[ozone$stationPlotted == "481671034-01"] <- "texas"
ozone$State[ozone$stationPlotted == "482011050-01"] <- "texas"
# also going to stick the DC monitor in to maryland
ozone$State[ozone$State == "district of columbia"] <- "maryland"

# oh man. just realized the state FIPS code is the first two digits of the 
# station id. doh... ceste la vie

summary(as.factor(ozone$State))

# removing alaska site and calculating standard error
ozone <- ozone %>% filter(!is.na(State)) %>% 
  mutate(std_error = (confInt_upper - differenceOfMeans)/1.96)


# meta analysis to pool station estimates ----
# set up empty matrix
state_ozone <- matrix(ncol=4, nrow = length(unique(ozone$State)))
colnames(state_ozone) <- c("State", "delta_o3", "do3_se", "n_smoky")
# convert to dataframe
state_ozone <- as.data.frame(state_ozone)

# vector of state names
state_vector <- unique(ozone$State)


# state pooled est and std error ----
for(i in 1:length(state_vector)){

  state_ozone[i,1] <- state_name <- state_vector[i]
  
  # subset ozone dataset to just state
  state_df <- subset(ozone, State == state_name)
  
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



