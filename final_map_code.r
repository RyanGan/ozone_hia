#This script uses a loop to produce maps showing ed visit rates by state and strata

#Load Packages

library(ggplot2)
library(maptools)
library(RColorBrewer)
library(sp)
library(maps)
library(rgdal)
library(raster)
library(dplyr)
library(tidyverse)
library(cowplot)
library(broom)
library(plyr)

setwd('C:/Users/Jake/Desktop')

#Import datasets and set up paths

total_path <- paste0('./data/mc_estimates/rates/total_hia.csv')
total <- read_csv(total_path)

#african american
black_path <- paste0('./data/mc_estimates/rates/black_hia.csv')
black <- read_csv(black_path)

# hispanic
hisp_path <- paste0('./data/mc_estimates/rates/hisp_hia.csv')
hisp <- read_csv(hisp_path)

# white
white_path <- paste0('./data/mc_estimates/rates/white_hia.csv')
white <- read_csv(white_path)

# male
m_path <- paste0('./data/mc_estimates/rates/male_hia.csv')
male <- read_csv(m_path)

# female
f_path <- paste0('./data/mc_estimates/rates/female_hia.csv')
female <- read_csv(f_path)

#convert variables to numeric

total$median <- as.numeric(total$median)
male$median <- as.numeric(male$median)
female$median <- as.numeric(female$median)
black$median <- as.numeric(black$median)
hisp$median <- as.numeric(hisp$median)
white$median <- as.numeric(white$median)

#create a list of the dataframes to loop through

df_list <- list(total=total, male=male, female=female, black=black, 
                hisp=hisp, white=white)

df_name <- c('total', 'male', 'female', 'black', 'hisp', 'white')

# read zip code polygon shapefile for entire US.

bound_dir <- paste0("C:\\Users\\Jake\\Desktop\\state_shape_file\\", "\\gz_2010_us_040_00_500k")

zip <- readOGR(dsn = bound_dir, layer = 'gz_2010_us_040_00_500k')

#summary(zip)

states <- readOGR(dsn = bound_dir, layer = 'gz_2010_us_040_00_500k')

#plot(states)

states@data$id <- rownames(states@data)


states_df <- fortify(states) # convert to data frame suitable for plotting

states_df <- join(states_df, states@data, by="id")


names(states_df)[names(states_df)=="STATE"] <- 'state_num'

states_df$state_num <- as.numeric(states_df$state_num)


#Get rid of Alaksa and Hawaii

states_df <- states_df[!(states_df$state_num == 2),]
states_df <- states_df[!(states_df$state_num == 12),]

#Lets make a loop to make a map for each strata's dataframe

for(z in 1:length(df_list)){
  
  # choose dataframe to loop through  
 
  df_to_loop <- data.frame(df_list[[z]]) 
  df_name <- names(df_list[z])
  
  median <- df_to_loop[ ,2]
  state_num <- df_to_loop[ ,5]
  
  #merge each dataframe with shapefiles
  
  choro <- merge(states_df, df_to_loop, by = "state_num")
  
  #restrict map lat and long
  choro <- choro[choro$long > -124.848 &
                   choro$long < -66.886 &
                   choro$lat > 24.3964 &
                   choro$lat < 49.3844, ]
  
  #order and fortify
  choro <- choro[order(choro$order), ]
  fortify(choro, region='state')
  
  #produce a map each time we run the loop
  
plot_2_print <- ggplot(choro, aes(long, lat)) +
    geom_polygon(aes(group = group, fill = choro$median)) +
    coord_map("albers",  at0 = 45.5, lat1 = 29.5)+
    scale_fill_gradientn(name="ED Visits",
                         colours=brewer.pal(9,"Reds"))+
    geom_path(aes(group = group, fill = choro$median)) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          legend.position="none")


file_name <- paste0(df_name, "map.pdf")
save_plot(file_name, plot_2_print, base_height = 7, base_width = 7)
assign((df_name), plot_2_print)
}  


