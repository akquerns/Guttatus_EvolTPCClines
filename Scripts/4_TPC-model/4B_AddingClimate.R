library(viridis)
library(readr)
library(tidyverse)
library(dplyr)
library(naniar)
library(magrittr)
library(readxl)
library(plotly)

###################################################################################################
#######################################Adding climate to mean_dfsubset################################################################################################################################

mean_df_subset <- read.csv("TPC-outputs/mean_df_subset.csv")[,-1] # average parameters for each population
worldclim <- read.csv("Raw-data/1-17bioclim_pops.csv") #Climate data

#Add Lat, MAT, and Seasonality params back in
mean_df_subset$lat <- c(rep(NA,length(mean_df_subset$Population)))
mean_df_subset$long <- c(rep(NA,length(mean_df_subset$Population)))
mean_df_subset$MAT <- c(rep(NA,length(mean_df_subset$Population)))
mean_df_subset$tempseason <- c(rep(NA,length(mean_df_subset$Population)))
mean_df_subset$maxwarmest <- c(rep(NA,length(mean_df_subset$Population)))#max temp of warmest month, bio_5
mean_df_subset$meanwarmest <- c(rep(NA,length(mean_df_subset$Population)))#mean temp of warmest quarter, bio_10
mean_df_subset$mincoldest <- c(rep(NA,length(mean_df_subset$Population))) #min temp of coldest month, bio_6

#Add a column for "ID3", in which I have renamed the populations
mean_df_subset$pop2 <- c(rep(NA,length(mean_df_subset$Population))) #populations renumbered based on latitude within each range


for(i in 1:dim(mean_df_subset)[1]){
  mean_df_subset$lat[i] <- as.numeric(worldclim$Latitude[which(worldclim$ID2==mean_df_subset$Population[i])])
  mean_df_subset$long[i] <- as.numeric(worldclim$Longitude[which(worldclim$ID2==mean_df_subset$Population[i])])
  mean_df_subset$MAT[i] <- as.numeric(worldclim$bio_1[which(worldclim$ID2==mean_df_subset$Population[i])])
  mean_df_subset$tempseason[i] <- as.numeric(worldclim$bio_4[which(worldclim$ID2==mean_df_subset$Population[i])]) 
  mean_df_subset$maxwarmest[i] <- as.numeric(worldclim$bio_5[which(worldclim$ID2==mean_df_subset$Population[i])]) 
  mean_df_subset$meanwarmest[i] <- as.numeric(worldclim$bio_10[which(worldclim$ID2==mean_df_subset$Population[i])]) 
  mean_df_subset$mincoldest[i] <- as.numeric(worldclim$bio_6[which(worldclim$ID2==mean_df_subset$Population[i])]) 
 mean_df_subset$pop2[i] <- as.character(worldclim$ID3[which(worldclim$ID2==mean_df_subset$Population[i])]) 
}

#Edit variables to reflect actual vals
mean_df_subset$MAT <- mean_df_subset$MAT/10
mean_df_subset$maxwarmest <- mean_df_subset$maxwarmest/10
mean_df_subset$meanwarmest <- mean_df_subset$meanwarmest/10
mean_df_subset$mincoldest <- mean_df_subset$mincoldest/10
mean_df_subset$tempseason <- mean_df_subset$tempseason/100 #Standard deviation *100

write.csv(mean_df_subset, "Processed-data/mean_df_subsetwithclim.csv")


