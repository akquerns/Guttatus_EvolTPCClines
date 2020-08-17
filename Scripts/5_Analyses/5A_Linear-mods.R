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

#w/o Interaction term
optvslat2<- lm(maximaBT~lat+Range, data=data)
summary(optvslat2)

#AIC

AIC(optvslat1, k=2)
AIC(optvslat2, k=2)

#Optima by MAT + Range + MATxRange
optvsMAT1<- lm(maximaBT~MAT*Range, data=data)
summary(optvsMAT1)

#w/o Interaction term
optvsMAT2<- lm(maximaBT~MAT+Range, data=data)
summary(optvsMAT2)

#AIC
AIC(optvsMAT1, k=2)
AIC(optvsMAT2, k=2) #<-better model

#Overall effect of MAT is significant

#Let's try to further understand the low R-square in this relationship

ToptvsMATN <- lm (maximaBT~MAT, data=data%>% dplyr::filter(Range=="N"))
summary(ToptvsMATN)

ToptvsMATI <- lm (maximaBT~MAT, data=data %>% dplyr::filter(Range=="I"))
summary(ToptvsMATI)

###############################################Pmax################################################

#(S-G Tradeoffs)

#perfmax by breadth *Range
perfmaxvsB501<- lm(max_RGR~B50*Range, data=data)
summary(perfmaxvsB501)

#w/o Interaction term
perfmaxvsB502<- lm(max_RGR~B50+Range, data=data)
summary(perfmaxvsB502)

#AIC
AIC(perfmaxvsB501, k=2)
AIC(perfmaxvsB502, k=2)

#Overall effect of breadth is significant

############################################TBreadth###############################################

#Breadth by Lat*Range
B50vslat1<- lm(B50~lat*Range, data=data)
summary(B50vslat1)

#w/o interaction term
B50vslat2<- lm(B50~lat+Range, data=data)
summary(B50vslat2)

#AIC
AIC(B50vslat1, k=2)
AIC(B50vslat2, k=2)

#individual models for ranges (in case we need, because interaction term was somewhat close to significant)

#B50vslatN <- lm (B50~lat, data=data%>% dplyr::filter(Range=="N"))
#summary(B50vslatN)

#B50vslatI <- lm (B50~lat, data=data %>% dplyr::filter(Range=="I"))
#summary(B50vslatI)

#effect of lat on Thermal breadth is significant in invasive range

#Breadth by TS*Range
B50vsTS1<- lm(B50~tempseason*Range, data=data)
summary(B50vsTS1)

#w/o interaction term
B50vsTS2<- lm(B50~tempseason+Range, data=data)
summary(B50vsTS2)

#AIC

AIC(B50vsTS1, k=2)
AIC(B50vsTS2, k=2)


########################################Upper thermal limit########################################

#Removed from manuscript d/t limits (upper and lower) being extrapolations beyond the range of temperatures which we measured (AKA--not really great estimates of thermal limits). Kept in code just in case :)

#Upper limit by LAT * Range
#xmaxvslat1<- lm(x_maxBT~lat*Range, data=data)
#summary(xmaxvslat1)

#model w/o interaction
#xmaxvslat2<- lm(x_maxBT~lat+Range, data=data)
#summary(xmaxvslat2)

#AIC
#AIC(xmaxvslat1, k=2)
#AIC(xmaxvslat2, k=2)

#Upper limit by MAT * Range
#xmaxvsMAT1<- lm(x_maxBT~MAT*Range, data=data)
#summary(xmaxvsMAT1)

#w/o interaction term
#xmaxvsMAT2<- lm(x_maxBT~MAT+Range, data=data)
#summary(xmaxvsMAT2)

#AIC
#AIC(xmaxvsMAT1, k=2)
#AIC(xmaxvsMAT2, k=2)

#Interaction term could be considered marginally significant, so we conduct models for each range just in case

#xmaxvsMATN <- lm (x_maxBT~MAT, data=data%>% dplyr::filter(Range=="N"))
#summary(xmaxvsMATN)

#xmaxvsMATI <- lm (x_maxBT~MAT, data=data %>% dplyr::filter(Range=="I"))
#summary(xmaxvsMATI)

#effect of MAT on Tmax is marginally sign. in the invasive range (0.053)

########################################Lower thermal limit########################################

#Removed from manuscript d/t limits (upper and lower) being extrapolations beyond the range of temperatures which we measured (AKA--not really great estimates of thermal limits). Kept in code just in case :)

#Lower limit by LAT * Range
#xminvslat1<- lm(x_minBT~lat*Range, data=data)
#summary(xminvslat1)

#w/o interaction term

#xminvslat2<- lm(x_minBT~lat+Range, data=data)
#summary(xminvslat2)

#AIC
#AIC(xminvslat1, k=2)
#AIC(xminvslat2, k=2)

#Interaction term is sig--do range level mods
#xminvslatN <- lm (x_minBT~lat, data=data%>% dplyr::filter(Range=="N"))
#summary(xminvslatN)

#xminvslatI <- lm (x_minBT~lat, data=data %>% dplyr::filter(Range=="I"))
#summary(xminvslatI)

#effect of lat on Tmin is significant in the invasive range

#lower limit by MAT *Range
#xminvsMAT1<- lm(x_minBT~MAT*Range, data=data)
#summary(xminvsMAT1)

#w/o interaction
#xminvsMAT2<- lm(x_minBT~MAT+Range, data=data)
#summary(xminvsMAT2)

#AIC
#AIC
#AIC(xminvsMAT1, k=2)
#AIC(xminvsMAT2, k=2)

#Interaction term is significant, so we conduct models for each range

#xminvsMATN <- lm (x_minBT~MAT, data=data%>% dplyr::filter(Range=="N"))
#summary(xminvsMATN)

#xminvsMATI <- lm (x_minBT~MAT, data=data %>% dplyr::filter(Range=="I"))
#summary(xminvsMATI)

#effect of MAT on Tmin is marginally significant (0.055) in the invasive range

################################################Area###############################################
#Does area under the curve differ with breadth? does it differ with maximum performance?

#If area under the curve varies with breadth and Pmax

#B50
areavsB50<- lm(area~ B50*Range, data=data)
summary(areavsB50)

#w/o interaction
areavsB502<- lm(area~ B50+Range, data=data)
summary(areavsB502)

#AIC
AIC(areavsB50, k=2)
AIC(areavsB502, k=2)

#Pmax

areavsmaxperf<- lm(area~ max_RGR*Range, data=data)
summary(areavsmaxperf)

#w/o interaction term

areavsmaxperf2<- lm(area~ max_RGR+Range, data=data)
summary(areavsmaxperf2)

#AIC
AIC(areavsmaxperf)
AIC(areavsmaxperf2)

