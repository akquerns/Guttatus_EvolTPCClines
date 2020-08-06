# Example code for TPC curves
#install.packages("performr")
library(performr)
library(ggplot2)
library(gridExtra)

###############################################################################################################################################First set of hypotheses: TPCS#############################################################################################################################

####################################   
# Example curve to put parameters onto
####################################   

# build the tpc
xs1 <- seq(15,45, length.out=100) # temperatures for the tpc
tpc1 <- performance_mu(xs = xs1, # Temperatures for the tpc
                       shape1 = 2, # When shape1 is larger than shape2, the curve will skew right
                       shape2 = 3, # When shape2 is larger than shape2, the curve will skew right
                       stretch = 0.4, # Dictates the maximum expected value of the response trait
                       x_min = 15, # Location along the environmental axis left of the optimum where the response trait falls to 0
                       x_max = 45) # Location along the environmental axis right of the optimum where the response trait falls to 0

# make tpcs into dataframes containing temperature and performance
tpc1df <- data.frame(x= xs1, y=tpc1)

ex_tpc <- ggplot() +
  labs(x=expression(paste("Temperature")), y="Performance", tag="A") + 
  scale_x_continuous(limits=c(5,50), expand = c(0, 0), breaks = NULL) +
  scale_y_continuous(limits=c(0,1), expand = c(0, 0), breaks = NULL) +
  geom_line(data=tpc1df, aes(x, y), lwd = 2, color="black") +
  guides(fill=FALSE, color=FALSE) + theme_bw() + 
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=13), strip.background=element_rect(colour=NA,fill=NA), 
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5))
ex_tpc


####################################   
# Hypothesis: breadth will differ
####################################   

# build a tpc that is narrow
xs1 <- seq(15,40, length.out=100) # temperatures for the tpc
tpc1 <- performance_mu(xs = xs1, # Temperatures for the tpc
                         shape1 = 2, # When shape1 is larger than shape2, the curve will skew right
                         shape2 = 3, # When shape2 is larger than shape2, the curve will skew right
                         stretch = 0.5, # Dictates the maximum expected value of the response trait
                         x_min = 15, # Location along the environmental axis left of the optimum where the response trait falls to 0
                         x_max = 40) # Location along the environmental axis right of the optimum where the response trait falls to 0

# build a tpc that is broad
xs2 <- seq(10,45, length.out=100) # temperatures for the tpc
tpc2 <- performance_mu(xs = xs2, 
                          shape1 = 2, 
                          shape2 = 3, 
                          stretch = 0.5, 
                          x_min = 10, 
                          x_max = 45)

# make tpcs into dataframes containing temperature and performance
tpc1df <- data.frame(x= xs1, y=tpc1)
tpc2df <- data.frame(x= xs2, y=tpc2)

ex_tpc_breadth <- ggplot() +
  labs(x=expression(paste("Temperature")), y="Performance", tag="B") + 
  scale_x_continuous(limits=c(5,50), expand = c(0, 0), breaks = NULL) +
  scale_y_continuous(limits=c(0,1), expand = c(0, 0), breaks = NULL) +
  geom_line(data=tpc1df, aes(x, y), lwd = 2, color="red") +
  geom_line(data=tpc2df, aes(x, y), lwd = 2, color="blue") +
  guides(fill=FALSE, color=FALSE) + theme_bw() + 
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=13), strip.background=element_rect(colour=NA,fill=NA), 
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5))
ex_tpc_breadth


####################################   
# Hypothesis: optima will differ
####################################   

# build a tpc that has low optimum
xs1 <- seq(10,35, length.out=100) # temperatures for the tpc
tpc1 <- performance_mu(xs = xs1, # Temperatures for the tpc
                       shape1 = 2, # When shape1 is larger than shape2, the curve will skew right
                       shape2 = 3, # When shape2 is larger than shape2, the curve will skew right
                       stretch = 0.5, # Dictates the maximum expected value of the response trait
                       x_min = 10, # Location along the environmental axis left of the optimum where the response trait falls to 0
                       x_max = 35) # Location along the environmental axis right of the optimum where the response trait falls to 0

# build a tpc that has high optimum
xs2 <- seq(20,45, length.out=100) # temperatures for the tpc
tpc2 <- performance_mu(xs = xs2, 
                       shape1 = 2, 
                       shape2 = 3, 
                       stretch = 0.5, 
                       x_min = 20, 
                       x_max = 45)

# make tpcs into dataframes containing temperature and performance
tpc1df <- data.frame(x= xs1, y=tpc1)
tpc2df <- data.frame(x= xs2, y=tpc2)

ex_tpc_opt <- ggplot() +
  labs(x=expression(paste("Temperature")), y="Performance", tag="C") + 
  scale_x_continuous(limits=c(5,50), expand = c(0, 0), breaks = NULL) +
  scale_y_continuous(limits=c(0,1), expand = c(0, 0), breaks = NULL) +
  geom_line(data=tpc1df, aes(x, y), lwd = 2, color="red") +
  geom_line(data=tpc2df, aes(x, y), lwd = 2, color="blue") +
  guides(fill=FALSE, color=FALSE) + theme_bw() + 
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=13), strip.background=element_rect(colour=NA,fill=NA), 
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5))
ex_tpc_opt


####################################   
# Hypothesis: specialist-generalist tradeoff
####################################   

# build a tpc that is narrow and tall
xs1 <- seq(15,40, length.out=100) # temperatures for the tpc
tpc1 <- performance_mu(xs = xs1, # Temperatures for the tpc
                       shape1 = 2, # When shape1 is larger than shape2, the curve will skew right
                       shape2 = 3, # When shape2 is larger than shape2, the curve will skew right
                       stretch = 0.5, # Dictates the maximum expected value of the response trait
                       x_min = 15, # Location along the environmental axis left of the optimum where the response trait falls to 0
                       x_max = 40) # Location along the environmental axis right of the optimum where the response trait falls to 0

# build a tpc that is broad and short
xs2 <- seq(10,45, length.out=100) # temperatures for the tpc
tpc2 <- performance_mu(xs = xs2, 
                       shape1 = 2, 
                       shape2 = 3, 
                       stretch = 0.25, 
                       x_min = 10, 
                       x_max = 45)

# make tpcs into dataframes containing temperature and performance
tpc1df <- data.frame(x= xs1, y=tpc1)
tpc2df <- data.frame(x= xs2, y=tpc2)

ex_tpc_sg <- ggplot() +
  labs(x=expression(paste("Temperature")), y="Performance", tag="D") + 
  scale_x_continuous(limits=c(5,50), expand = c(0, 0), breaks = NULL) +
  scale_y_continuous(limits=c(0,1), expand = c(0, 0), breaks = NULL) +
  geom_line(data=tpc1df, aes(x, y), lwd = 2, color="red") +
  geom_line(data=tpc2df, aes(x, y), lwd = 2, color="blue") +
  guides(fill=FALSE, color=FALSE) + theme_bw() + 
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=13), strip.background=element_rect(colour=NA,fill=NA), 
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5))
ex_tpc_sg



####################################   
# Export plot
####################################   

png("Figures/Fig1-TPC-hypotheses.png", width = 6, height = 6, units = 'in', res = 300)
grid.arrange(ex_tpc,
             ex_tpc_breadth,
             ex_tpc_opt,
             ex_tpc_sg,
             ncol = 2, nrow = 2)
dev.off()


#this figure will later be edited together with clines hypotheses (below) to create one hypothesis figure

##################################################################################################################################Second set of hypotheses (CLINES)######################################################################################################################################

ExampleClines <- read_csv("Raw-data/ExampleClines.csv")

#Thermal Optima/Limits with LAT

p1<-ggplot()+ geom_smooth(data=ExampleClines %>% dplyr::filter(Range=="Native"), aes(x=Latitude, y=Topt, group=Range), method= "glm", se=FALSE, color="blue", size=2.5) + geom_smooth(data=ExampleClines %>% dplyr::filter(Range=="Invasive"), aes(x=Latitude, y=Topt, group=Range), method= "glm", se=FALSE, color="red", size=2.5, linetype="dashed")+ scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL) +theme_bw()+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=12), strip.background=element_rect(colour=NA,fill=NA), 
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5))

#Thermal Optima/Limits with MAT
p2<-ggplot()+ geom_smooth(data=ExampleClines %>% dplyr::filter(Range=="Native"), aes(x=MAT, y=Topt, group=Range), method= "glm", se=FALSE, color="blue", size=2.5) + geom_smooth(data=ExampleClines %>% dplyr::filter(Range=="Invasive"), aes(x=MAT, y=Topt, group=Range), method= "glm", se=FALSE, color="red", size=2.5, linetype="dashed")+ 
  scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL)+
theme_bw()+
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=13), strip.background=element_rect(colour=NA,fill=NA), 
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5))

#Thermal Breadth with LAT
p3<-ggplot()+ geom_smooth(data=ExampleClines %>% dplyr::filter(Range=="Native"), aes(x=Latitude, y=TB_PP, group=Range), method= "glm", se=FALSE, color="blue", size=2.5) + geom_smooth(data=ExampleClines %>% dplyr::filter(Range=="Invasive"), aes(x=Latitude, y=TB_PP, group=Range), method= "glm", se=FALSE, color="red", size=2.5, linetype="dashed")+
scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL)+
theme_bw()+
theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=13), strip.background=element_rect(colour=NA,fill=NA), 
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5))


#Thermal Breadth with Temperature Seasonality
p4<-ggplot()+ geom_smooth(data=ExampleClines %>% dplyr::filter(Range=="Native"), aes(x=Seasonality, y=TB_PP, group=Range), method= "glm", se=FALSE, color="blue") + geom_smooth(data=ExampleClines %>% dplyr::filter(Range=="Invasive"), aes(x=Seasonality, y=TB_PP, group=Range), method= "glm", se=FALSE, color="red")+
scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL)+
theme_bw()+
theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=13), strip.background=element_rect(colour=NA,fill=NA), 
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5))

#Put together
grid.arrange(p1,p2,p3, nrow=1)

#export as image 900 x 300--Fig1-Clines-hypotheses
