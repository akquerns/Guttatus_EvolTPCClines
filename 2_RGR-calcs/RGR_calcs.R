library(viridis)
library(readr)
library(tidyverse)
library(dplyr)
library(naniar)
library(magrittr)
library(readxl)
library(plotly)

Raw <- read_csv("Raw-data/Rawdata.csv")

#All values in this dataset contain measurements for Avg SSL going in--however, some may have no measurement for Avg SSL coming out due to plant deterioration; therefore correct these NAs to zero
Raw$AvgsecondaryOUT[is.na(Raw$AvgsecondaryOUT)] <- 0

######################################################################################################################################RGRTOT 1##############################################################################################################################################################

#secondarytot(IN/OUT)= total number secondary branches (going in or out of chambers)
#Avgsecondary(IN/OUT)=length of an average secondary stem (going in or out of chambers)
#Stem(IN/OUT)=length of primary stem (going in or out of chambers)

################First let's get our total grow IN/OUT

##Write FXN: IN = (secondarytotIN*AvgsecondaryIN) +StemIN

TotIN_Subfxn <- function(secondarytotIN, AvgsecondaryIN, StemIN){
  ((secondarytotIN*AvgsecondaryIN) + StemIN)
}

##Write fxn: OUT
TotOUT_Subfxn <- function(secondarytotOUT, AvgsecondaryOUT, StemOUT){
  ((secondarytotOUT*AvgsecondaryOUT) + StemOUT)
}

##Add columns

Raw$TotIN_Sub <- c(rep(NA,length(Raw$Family)))
Raw$TotOUT_Sub <- c(rep(NA,length(Raw$Family)))

#Calc IN and OUT

for(i in 1:dim(Raw)[1]){
  Raw$TotIN_Sub[i] <- TotIN_Subfxn(secondarytotIN=Raw$secondarytotIN[i], AvgsecondaryIN = Raw$AvgsecondaryIN[i], StemIN = Raw$StemIN[i])
}

for(i in 1:dim(Raw)[1]){
  Raw$TotOUT_Sub[i] <- TotOUT_Subfxn(secondarytotOUT=Raw$secondarytotOUT[i], AvgsecondaryOUT = Raw$AvgsecondaryOUT[i], StemOUT = Raw$StemOUT[i])
}

##RGRTOT1 Function ##
RGRTOT1fxn <- function(TotIN_Sub, TotOUT_Sub){
  (TotOUT_Sub - TotIN_Sub)/(TotIN_Sub*7)
}

Raw$RGRTOT1 <- c(rep(NA,length(Raw$Family)))

for(i in 1:dim(Raw)[1]){
  Raw$RGRTOT1[i] <- RGRTOT1fxn(TotIN_Sub =Raw$TotIN_Sub[i], TotOUT_Sub = Raw$TotOUT_Sub[i])
}


##RGRtot1x: Correcting negative RGR to zero
Raw$RGRTOT1x <- as.numeric(c(Raw$RGRTOT1))

Raw$RGRTOT1x[which(Raw$RGRTOT1<0)] <- 0

####################################################################################################
#############################Renamed populations based on latitude#####################################################################################################################################

#Within my worldclim data, I renamed the populations based on their latitude, rather than my pre-existing number system

#Here I'm adding that column

Raw$pop <- c(rep(NA,length(Raw$Population))) 

data <- read_csv("Raw-data/1-17bioclim_pops.csv")

for(i in 1:dim(Raw)[1]){
  Raw$pop[i] <- as.character(data$ID3[which(data$ID2==Raw$Population[i])])}


########################################################################################################################################NAME FILE#######################################################################################################################################################
write.csv(Raw, "Raw-data/RGRcalcs.csv")

