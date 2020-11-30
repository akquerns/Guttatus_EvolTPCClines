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
dataN <- read.csv("Processed-data/mean_df_subsetwithclim.csv") %>% dplyr::filter(Range=="N")
dataI <- read.csv("Processed-data/mean_df_subsetwithclim.csv") %>% dplyr::filter(Range=="I")

#Set colors
group.colors <- c(I = "firebrick3", N = "deepskyblue1")
group.colors1 <- c(UK = "firebrick3", US = "deepskyblue1")

#set potential linetypes
group.linesNsig <- c(I = "dotted", N = "solid")
group.linesNsig1 <- c(UK = "dotted", US = "solid")
group.linesUKsig <- c(I = "solid", N = "dotted")
group.linesUKsig1 <- c(UK = "solid", US = "dotted")
group.linesneithersig <- c(I = "dashed", N = "dotted")

##NOTE: Originally included Tmax and Tmin clines in analyses; later removed due to unreliable estimates caused by overextrapolation of extreme/unmeasured temperatures. Also chose not to use clines of Pmax ("Maxima") due to lower biological relevance. I have blocked out these models from the final figure, but feel free to unblock them in order to see all of the params!

#######################################################################################################################################Linear mods of latitude for each ####################################################################################################################################

#Optima-NS
z1<-ggplot() + geom_jitter(data=data, aes(x = lat, y = maximaBT, group=Range, col=Range, shape=Range), size=3, alpha=1) +
  theme_bw() + 
  theme(text = element_text(size=16), axis.text.x = element_text(angle=360, size=12), axis.text.y = element_text(size=12), legend.position="none")+scale_color_manual(values=group.colors)+
  scale_x_continuous(name=expression(paste("Latitude (°N)")))+
  scale_y_continuous(name=expression(paste(T[opt], " (°C)"))) #+ geom_smooth(data=data, aes(x = lat, y = maximaBT, group=Range, col=Range), method="lm", linetype="dashed")
z1

#Maxima--NOT USED IN MANUSCRIPT
#z2<-ggplot() + geom_jitter(data=data, aes(x = lat, y = max_RGR, group=Range, col=Range, shape=Range))+ geom_smooth(data=data, aes(x = lat, y = max_RGR, group=Range, col=Range), method="lm") +
# theme_bw() + 
#theme(text = element_text(size=16), axis.text.x = element_text(angle=360, size=12), axis.text.y = element_text(size=12), legend.position="none")+scale_color_manual(values=group.colors)

#Upper limit-nothing is sig.
#z3 <- ggplot() + geom_jitter(data=data, aes(x = lat, y = x_maxBT, group=Range, col=Range, shape=Range), size=3, alpha=1) +
 # theme_bw() + 
 # theme(text = element_text(size=16), axis.text.x = element_text(angle=360, size=12), axis.text.y = element_text(size=12), legend.position="none")+scale_color_manual(values=group.colors) +
 # scale_x_continuous(name=expression(paste("Latitude (°N)")))+
  #scale_y_continuous(name=expression(paste(T[max], " (°C)")))#+ geom_smooth(data=data, aes(x = lat, y = x_maxBT, group=Range, col=Range), method="lm", linetype="dashed")
#z3


#Lower  limit
#UK is sig. US is N.S.
#z4 <- ggplot() + geom_jitter(data=data, aes(x = lat, y = x_minBT, group=Range, col=Range, shape=Range), size=3, alpha=1)+ geom_smooth(data=dataI, aes(x = lat, y = x_minBT, group=Range, col=Range, alpha=1), method="lm", level=0.95, size=1.25, alpha=0.3)+
 # theme_bw() + 
  #theme(text = element_text(size=16), axis.text.x = element_text(angle=360, size=12), axis.text.y = element_text(size=12), legend.position="none")+scale_color_manual(values=group.colors) +
  #scale_x_continuous(name=expression(paste("Latitude (°N)")))+
  #scale_y_continuous(name=expression(paste(T[min], " (°C)")))
#z4

#Thermal breadth--NS
z5<-ggplot() + geom_jitter(data=data, aes(x = lat, y = B50, group=Range, col=Range, shape=Range), size=3, alpha=1)+
  theme_bw() + 
  theme(text = element_text(size=16), axis.text.x = element_text(angle=360, size=12), axis.text.y = element_text(size=12), legend.position="none")+scale_color_manual(values=group.colors) +  scale_x_continuous(name=expression(paste("Latitude (°N)")))+
  scale_y_continuous(name=expression(paste(T[breadth], " (°C)")))#+ scale_linetype_manual(values=group.linesUKsig)+ geom_smooth(data=dataI, aes(x = lat, y = B50, group=Range, col=Range, linetype=Range), method="lm", level=0.95, size=1.25, alpha=0.3)
z5

#######################################################################################################################################Linear mods of Mean annual temp###################################################################################################################################

#Optima--overall effect of MAT is significant
y1<-ggplot() + geom_jitter(data=data, aes(x = MAT, y = maximaBT, group=Range, col=Range, shape=Range), size=3, alpha=1) +
 geom_smooth(data=data, aes(x = MAT, y = maximaBT, col=Range), method="lm", level=0.95, col="black", size=1.25, alpha=0.3) + 
  theme_bw() + 
  theme(text = element_text(size=16), axis.text.x = element_text(angle=360, size=12), axis.text.y = element_text(size=12), legend.position="none")+scale_color_manual(values=group.colors) +
  scale_x_continuous(name=expression(paste("MAT (°C)")))+
  scale_y_continuous(name=expression(paste(T[opt], " (°C)")))+ geom_smooth(data=dataI, aes(x = MAT, y = maximaBT, group=Range, col=Range), linetype="dotted", method="lm", level=0.95, size=1.25, alpha=0.3, se=FALSE)+ geom_smooth(data=dataN, aes(x = MAT, y = maximaBT, group=Range, col=Range), linetype="solid", method="lm", level=0.95, size=1, se=FALSE)
y1

#Maxima--NOT USED IN MANUSCRIPT
#y2<-ggplot() + geom_jitter(data=data, aes(x = MAT, y = max_RGR, group=Range, col=Range, shape=Range))+ geom_smooth(data=data, aes(x = MAT, y = max_RGR, group=Range, col=Range), method="lm") +
# theme_bw() + 
#theme(text = element_text(size=16), axis.text.x = element_text(angle=360, size=12), axis.text.y = element_text(size=12), legend.position="none")+scale_color_manual(values=group.colors)

#Upper limit--N.S. for all (would've been marginally sig. at 0.053)
#y3<-ggplot() + geom_jitter(data=data, aes(x = MAT, y = x_maxBT, group=Range, col=Range, shape=Range), size=3, alpha=1) +
  #theme_bw() + 
  #theme(text = element_text(size=16), axis.text.x = element_text(angle=360, size=12), axis.text.y = element_text(size=12), legend.position="none")+scale_color_manual(values=group.colors)+ scale_linetype_manual(values=group.linesUKsig)+
  #scale_x_continuous(name=expression(paste("MAT (°C)")))+
 # scale_y_continuous(name=expression(paste(T[max], " (°C)")))#+ geom_smooth(data=data, aes(x = MAT, y = x_maxBT, group=Range, col=Range, linetype=Range), method="lm", level=0.95, size=1.5)
#y3

#Lower limit--again NS, but would've been marginally sign. at 0.055
#y4<-ggplot() + geom_jitter(data=data, aes(x = MAT, y = x_minBT, group=Range, col=Range, shape=Range), size=3, alpha=1)+
  #theme_bw() + 
 # theme(text = element_text(size=16), axis.text.x = element_text(angle=360, size=12), axis.text.y = element_text(size=12), legend.position="none")+scale_color_manual(values=group.colors)+ scale_linetype_manual(values=group.linesUKsig)+
  #scale_x_continuous(name=expression(paste("MAT (°C)")))+
  #scale_y_continuous(name=expression(paste(T[min], " (°C)")))
#+ geom_smooth(data=data, aes(x = MAT, y = x_minBT, group=Range, col=Range, linetype=Range), method="lm", level=0.95, size=1.5) 
#y4

##############################################################################################################################################TS by Tbreadth cline#######################################################################################################################################

#breadth--NS
y5<-ggplot() + geom_jitter(data=data, aes(x = tempseason, y = B50, group=Range, col=Range, shape=Range), size=3, alpha=1) +
  theme_bw() + 
  theme(text = element_text(size=16), axis.text.x = element_text(angle=360, size=12), axis.text.y = element_text(size=12), legend.position="none")+scale_color_manual(values=group.colors)+
  scale_y_continuous(name=expression(paste(T[breadth], "(°C)")))+
  scale_x_continuous(name=expression(paste("Seasonality (°C)")))#+ geom_smooth(data=data, aes(x = tempseason, y = B50, group=Range, col=Range), method="lm", linetype="dashed")
y5

###################################################################################################
##################################Create Full Fig#####################################################################################################################################################

##FIG 5

grid.arrange(z1,y1, z5, y5, ncol=2) # export as image: 700 x 700; save in Figures, edit in Manuscript-figs

#Reduced figs: 1200x600 (for ppt)

grid.arrange(z1,y1, nrow=1)
grid.arrange(z3,y3, nrow=1)
grid.arrange(z4,y4, nrow=1)
grid.arrange(z5,y5, nrow=1)

##################################################################################################
##################################Breadth vs Maxima (S-G TRADEOFF)################################
##################################################################################################

##FIG 4
# breadth by max RGR: significant, no diffc across ranges
library(viridis)
B2<-ggplot() + geom_jitter(data=data, aes(x = B50, y = max_RGR, group=Range, col=Range, shape=Range), size=4, alpha=1)+ geom_smooth(data=data, aes(x = B50, y = max_RGR, col=Range), method="lm", col="black", level=0.95, size=1.5, alpha=0.3) +
  theme_bw() + 
  theme(text = element_text(size=16), axis.text.x = element_text(angle=360, size=12), axis.text.y = element_text(size=12), legend.position="none")+scale_color_manual(values=group.colors)+
  scale_x_continuous(name=expression(paste(T[breadth], " (°C)")))+
  scale_y_continuous(name=expression(paste(P[max], " (cm/cm/day)")))
B2

#save as IMG 400x 400 in Figures folder As Fig4-SGTradeoff --edited to add legend in Manuscript-figs
