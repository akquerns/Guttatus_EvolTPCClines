##################Cartoon Figs for Paper#######################################################################################################################################################################

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

test <- read_csv("~/Documents/ShethLabFall2019/Experiment/testgaus.csv")

##################################################################################################################################Fig 1: First set of hypotheses (TPC)######################################################################################################################################

#Changes in Optima
p1 <- ggplot(data = data.frame(x = c(-6, 6)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), color="red") +
  scale_y_continuous(breaks = NULL) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 2, sd = 1), color="blue")
p1

#Specialist generalist 
p2 <- ggplot(data = data.frame(x = c(-6, 6)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1), color="red") +
  scale_y_continuous(breaks = NULL) +
  stat_function(fun = dnorm, n = 120, args = list(mean = 0, sd = 1.7), color="blue")
p2

#Changes in Tolerance


##################################################################################################################################Fig 2: Second set of hypotheses (CLINES)##################################################################################################################################
ExampleClines <- read_csv("~/Documents/ShethLabFall2019/Experiment/ExampleClines.csv")

#Thermal Optima/Limits with LAT

p1<-ggplot()+ geom_smooth(data=ExampleClines %>% dplyr::filter(Range=="Native"), aes(x=Latitude, y=Topt, group=Range), method= "glm", se=FALSE, color="blue", size=1.5) + geom_smooth(data=ExampleClines %>% dplyr::filter(Range=="Invasive"), aes(x=Latitude, y=Topt, group=Range), method= "glm", se=FALSE, color="red", size=1.5, linetype="dashed")+ theme_classic()

#Thermal Optima/Limits with MAT
p2<-ggplot()+ geom_smooth(data=ExampleClines %>% dplyr::filter(Range=="Native"), aes(x=MAT, y=Topt, group=Range), method= "glm", se=FALSE, color="blue", size=1.5) + geom_smooth(data=ExampleClines %>% dplyr::filter(Range=="Invasive"), aes(x=MAT, y=Topt, group=Range), method= "glm", se=FALSE, color="red", size=1.5, linetype="dashed")+ theme_classic()

#Thermal Breadth/Plasticity with LAT
p3<-ggplot()+ geom_smooth(data=ExampleClines %>% dplyr::filter(Range=="Native"), aes(x=Latitude, y=TB_PP, group=Range), method= "glm", se=FALSE, color="blue", size=1.5) + geom_smooth(data=ExampleClines %>% dplyr::filter(Range=="Invasive"), aes(x=Latitude, y=TB_PP, group=Range), method= "glm", se=FALSE, color="red", size=1.5, linetype="dashed")+ theme_classic()


#Thermal Breadth/Plasticity with Temperature Seasonality
p4<-ggplot()+ geom_smooth(data=ExampleClines %>% dplyr::filter(Range=="Native"), aes(x=Seasonality, y=TB_PP, group=Range), method= "glm", se=FALSE, color="blue") + geom_smooth(data=ExampleClines %>% dplyr::filter(Range=="Invasive"), aes(x=Seasonality, y=TB_PP, group=Range), method= "glm", se=FALSE, color="red")+ theme_classic()

#Put together
grid.arrange(p1,p2,p3, nrow=1)
