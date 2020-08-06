#The purpose of this file was to conduct a PCA on temperature related variables for ALL of my available populations

#This PCA aided my selection of populations which were used in my temperature growth experiments by showing how populations were distributed across the range of temperature-related bioclimatic variables, which allowed me to select populations without over-representing specific thermal environments

# load libraries
library(tidyverse)
library(raster)
library(sf)
library(spData)
library(viridis)
library(readr)
library(ggplot2)
library(FactoMineR)
library(maps)
library(mapdata)
library(tidyr)

#Read in files. Rename them.

guttatus_worldclim_seed_collections <- read_csv("Raw-data/guttatus_worldclim_seed_collections.csv")
View(guttatus_worldclim_seed_collections)#%>%
# (guttatus_worldclim_seed_collections$bio_1<-as.numeric(guttatus_worldclim_seed_collections$bio_1))

#Boxplot of US vs UK: TEMPERATURE
boxplot((guttatus_worldclim_seed_collections$bio_1/10)~guttatus_worldclim_seed_collections$ID1)

###################################################################################
#PCA to tell us which clim variables are most valuable for clustering data        #
###################################################################################


#####################
## 1. Prepare data ##
#####################

## read CSV file ##
mg_wcTEMP <- read_csv("Raw-data/guttatus_worldclim_seed_collections.csv")
View(mg_wcTEMP)

## remove variables unnecessary for TEMPERATURE PCA and rows with NA ##
mg_wcTEMP <- subset(mg_wcTEMP, select = -c(bio_12, bio_13, bio_14, bio_15, bio_16, bio_17, bio_18, bio_19))
mg_wcTEMP <- na.omit(mg_wcTEMP)
mg_wcTEMP2 <- subset(mg_wcTEMP, select = -c(Longitude, Latitude))
mg_wcTEMP3 <- subset(mg_wcTEMP2, select = -c(ID1))

#################################
## 2. run PCA using FactoMineR ##
#################################
library(FactoMineR)
library(multcompView)
mg_pcaTEMP <- PCA(mg_wcTEMP3, scale.unit=TRUE, quali.sup = 1, graph = FALSE)

## (optional) view PCA summary and variable and individual  ##
## values(coordinates, correlation, cosine2, contribuytion) ##
summary(mg_pcaTEMP)
write.csv(mg_pcaTEMP$var, "1-PCA/pcaTEMPvar.csv")
mg_pcaTEMP$var
mg_pcaTEMP$ind

#########################
## 3. Plot PCA results ##
#########################

## extract individual pc1 and pc2 coordinates from FactoMineR PCA ##
mg_wcTEMP$pc1 <- mg_pcaTEMP$ind$coord[,1]
mg_wcTEMP$pc2 <- mg_pcaTEMP$ind$coord[,2]

#geomtext repel
library(ggrepel)

## create PCA plot, point color by country ##
ggplot(data=mg_wcTEMP, aes(x=pc1, y=pc2, label=ID2)) + scale_fill_manual(values=c("red","blue","green","purple"), name="") + geom_point(aes(fill=ID1), shape=21, color="gray90", size=3) + labs(x="PC1",y="PC2")

#save as IMG: 400x400

#####################################################################################
#export data
write.csv(mg_wcTEMP, "1-PCA/PCATEMP.csv")

#What populations are most similar based on the top 3 climate variables from PCA? 
#a Tukey test to group them together?
#Once you know which are most similar, create lists of each group

#********************************
# 4. Definitions and units of bioclim variables (from http://www.worldclim.org/bioclim)
#********************************

# BIO1 = Annual Mean Temperature CHECK
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
# BIO3 = Isothermality (BIO2/BIO7) (* 100)
# BIO4 = Temperature Seasonality (standard deviation *100) (CHECK)
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO7 = Temperature Annual Range (BIO5-BIO6)
# BIO8 = Mean Temperature of Wettest Quarter
# BIO9 = Mean Temperature of Driest Quarter CHECK
# BIO10 = Mean Temperature of Warmest Quarter
# BIO11 = Mean Temperature of Coldest Quarter
