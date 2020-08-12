#Load Libraries
library(rstan)
library(devtools)
library(tidyverse)
library(ggridges)
library(cowplot)
library(ggpubr)
library(plotly)
library(lemon)
library(readxl)
library(car)
library(lme4)
library(gridExtra)
library(MuMIn)
library("dplyr")

#Load Data
data <- read.csv("Processed-data/mean_df_subsetwithclim.csv")

##First we need to check for normality of resids##

shapiro.test(data$maximaBT)
shapiro.test(data$x_maxBT)
shapiro.test(data$x_minBT)
shapiro.test(data$B50)
shapiro.test(data$max_RGR)

#should be all good

#Terms:
#maximaBT: Thermal optima (Topt)
#max_RGR: performance maxima (Pmax)
#B50: Thermal breadth (Tbreadth)
#lat: Latitude
#MAT: Mean annual temperature
#TS: Temperature seasonality
#x_maxBT: Upper thermal limit (Tmax)
#x_minBT: Lower thermal limit (Tmin)


#######################################################################################################################################Linear mods for each TPC Var######################################################################################################################################


############################################Optima#################################################

#Optima by Lat + Range + LatxRange
optvslat1<- lm(maximaBT~lat*Range, data=data)
summary(optvslat1)

#Remove N.S. Interaction term
optvslat2<- lm(maximaBT~lat+Range, data=data)
summary(optvslat2)
#All terms are N.S.

#Optima by MAT + Range + MATxRange
optvsMAT1<- lm(maximaBT~MAT*Range, data=data)
summary(optvsMAT1)

#Remove N.S. Interaction term
optvsMAT2<- lm(maximaBT~MAT+Range, data=data)
summary(optvsMAT2)

#Overall effect of MAT is significant

###############################################Pmax################################################

#(S-G Tradeoffs)

#perfmax by breadth *Range
perfmaxvsB501<- lm(max_RGR~B50*Range, data=data)
summary(perfmaxvsB501)

#Remove N.S. Interaction term
perfmaxvsB502<- lm(max_RGR~B50+Range, data=data)
summary(perfmaxvsB502)

#Overall effect of breadth is significant

############################################TBreadth###############################################

#Breadth by Lat*Range
B50vslat1<- lm(B50~lat*Range, data=data)
summary(B50vslat1)

#Remove nonsignificant interaction term (close but no cigar--about 0.08)
B50vslat2<- lm(B50~lat+Range, data=data)
summary(B50vslat2)

#individual models for ranges (in case we need, because interaction term was somewhat close to significant)

#B50vslatN <- lm (B50~lat, data=data%>% dplyr::filter(Range=="N"))
#summary(B50vslatN)

#B50vslatI <- lm (B50~lat, data=data %>% dplyr::filter(Range=="I"))
#summary(B50vslatI)

#effect of lat on Thermal breadth is significant in invasive range

#Breadth by TS*Range
B50vsTS1<- lm(B50~tempseason*Range, data=data)
summary(B50vsTS1)

#Interaction term is N.S., so we need to remove interaction term
B50vsTS2<- lm(B50~tempseason+Range, data=data)
summary(B50vsTS2)
#All are N.S.

########################################Upper thermal limit########################################

#Upper limit by LAT * Range
xmaxvslat1<- lm(x_maxBT~lat*Range, data=data)
summary(xmaxvslat1)

#Interaction term is N.S., so we remove
xmaxvslat2<- lm(x_maxBT~lat+Range, data=data)
summary(xmaxvslat2)

#All are N.S.

#Upper limit by MAT * Range
xmaxvsMAT1<- lm(x_maxBT~MAT*Range, data=data)
summary(xmaxvsMAT1)

#Interaction term is nonsignificant, so we remove interaction term

xmaxvsMAT2<- lm(x_maxBT~MAT+Range, data=data)
summary(xmaxvsMAT2)

#Interaction term could be considered marginally significant, so we conduct models for each range just in case

#xmaxvsMATN <- lm (x_maxBT~MAT, data=data%>% dplyr::filter(Range=="N"))
#summary(xmaxvsMATN)

#xmaxvsMATI <- lm (x_maxBT~MAT, data=data %>% dplyr::filter(Range=="I"))
#summary(xmaxvsMATI)

#effect of MAT on Tmax is marginally sign. in the invasive range (0.053)

########################################Lower thermal limit########################################

#Lower limit by LAT * Range
xminvslat1<- lm(x_minBT~lat*Range, data=data)
summary(xminvslat1)

#Interaction term is significant, so we conduct models for each range

xminvslatN <- lm (x_minBT~lat, data=data%>% dplyr::filter(Range=="N"))
summary(xminvslatN)

xminvslatI <- lm (x_minBT~lat, data=data %>% dplyr::filter(Range=="I"))
summary(xminvslatI)

#effect of lat on Tmin is significant in the invasive range

#lower limit by MAT *Range
xminvsMAT1<- lm(x_minBT~MAT*Range, data=data)
summary(xminvsMAT1)

#Interaction term is significant, so we conduct models for each range

xminvsMATN <- lm (x_minBT~MAT, data=data%>% dplyr::filter(Range=="N"))
summary(xminvsMATN)

xminvsMATI <- lm (x_minBT~MAT, data=data %>% dplyr::filter(Range=="I"))
summary(xminvsMATI)

#effect of MAT on Tmin is marginally significant (0.055) in the invasive range

################################################Area###############################################
#Does area under the curve differ with breadth? does it differ with maximum performance?

#If area under the curve varies with breadth and Pmax

#B50
areavsB50<- lm(area~ B50*Range, data=data)
summary(areavsB50)

#remove NS interaction term

areavsB502<- lm(area~ B50+Range, data=data)
summary(areavsB502)

#Pmax

areavsmaxperf<- lm(area~ max_RGR*Range, data=data)
summary(areavsmaxperf)

#remove NS interaction term

areavsmaxperf2<- lm(area~ max_RGR+Range, data=data)
summary(areavsmaxperf2)

