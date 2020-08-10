

#### PROJECT: Mimulus guttatus TPC project
#### PURPOSE: Plot TPCs for each of the M. guttatus populations
#### and compare tpc parameters between native and invasive groups.


####
#### install/load packages, specify settings
####

# install necessary packages
#devtools::install_github("silastittes/performr", local = FALSE) 

# load necessary packages
library(rstan)
library(devtools)
library(performr)
library(tidyverse)
library(ggridges)
library(cowplot)
library(ggpubr)



############
############ 
# Read in model output so that we can plot tpcs 
# [, -1] means that we read in without the first column, which we don't need
############
############ 
tidy_perf_pops <- read.csv("TPC-outputs/tidy_perf_pops_subset.csv")[,-1] # draws from the model
creds_pops <- read.csv("TPC-outputs/creds_pops_subset.csv")[,-1] # tpcs for each group with credible intervals
gutDat <- read.csv("Processed-data/gutDat_subset.csv")[,-1] # family-averaged data used for the model
mean_df <- read.csv("TPC-outputs/mean_df_subset.csv")[,-1] # average parameters for each population
mean_df_ci <- read.csv("TPC-outputs/mean_df_ci_subset.csv")[,-1] # 95% credible intervals for population-level parameters 


#######################################################################################################################Adding renamed populations to gutDat for graphing######################################################################################################################

#Note for explanation: I ended up renaming my populations later on for analysis purposes (wanted to structure population numbering based on latitude; I did this in Excel by adding a column "pop2", and here I added these numbers to my TPC graphs)

gutDat$pop <- c(rep(NA,length(gutDat$Population))) 
tidy_perf_pops$pop <- c(rep(NA,length(tidy_perf_pops$Population))) 
creds_pops$pop <- c(rep(NA,length(creds_pops$Population))) 
mean_df$pop <- c(rep(NA,length(mean_df$Population))) 
mean_df_ci$pop <- c(rep(NA,length(mean_df_ci$Population))) 

data <- read.csv("Processed-data/mean_df_subsetwithclim.csv")

for(i in 1:dim(gutDat)[1]){
  gutDat$pop[i] <- as.character(data$pop2[which(data$Population==gutDat$Population[i])])}

#for(i in 1:dim(tidy_perf_pops)[1]){
  #tidy_perf_pops$pop[i] <- as.character(data$pop2[which(data$Population==tidy_perf_pops$Population[i])])}

for(i in 1:dim(creds_pops)[1]){
  creds_pops$pop[i] <- as.character(data$pop2[which(data$Population==creds_pops$Population[i])])}

for(i in 1:dim(mean_df)[1]){
  mean_df$pop[i] <- as.character(data$pop2[which(data$Population==mean_df$Population[i])])}

for(i in 1:dim(mean_df_ci)[1]){ mean_df_ci$pop[i] <- as.character(data$pop2[which(data$Population==mean_df_ci$Population[i])])
}


gutDat$pop <- as.factor(gutDat$pop)
gutDat$pop <- factor(gutDat$pop, levels(gutDat$pop)[ c(1,6:13,2:5,14,24:31,15:23)])


tidy_perf_pops$pop <- as.factor(tidy_perf_pops$pop)
tidy_perf_pops$pop <- factor(tidy_perf_pops$pop, levels(tidy_perf_pops$pop)[ c(1,6:13,2:5,14,24:31,15:23)])

creds_pops$pop <- as.factor(creds_pops$pop)
creds_pops$pop <- factor(creds_pops$pop, levels(creds_pops$pop)[ c(1,6:13,2:5,14,24:31,15:23)])

mean_df$pop <- as.factor(mean_df$pop)
mean_df$pop <- factor(mean_df$pop, levels(mean_df$pop)[ c(1,6:13,2:5,14,24:31,15:23)])

mean_df_ci$pop <- as.factor(mean_df_ci$pop)
mean_df_ci$pop <- factor(mean_df_ci$pop, levels(mean_df_ci$pop)[ c(1,6:13,2:5,14,24:31,15:23)])
############ 
# Write supplementary table showing mean and CI for tpc parameters 
mean_ci_table <- mean_df %>% 
  right_join(mean_df_ci, by=c("Population"))
mean_ci_table <- as.data.frame(mean_ci_table)
mean_ci_table <- mean_ci_table[,c("Population", "Range.x",
                                  "maximaBT", "maximaBT_lci", "maximaBT_uci",
                                  "B50", "B50_lci", "B50_uci",
                                  "breadthBT", "breadthBT_lci", "breadthBT_uci",
                                  "x_minBT", "x_minBT_lci", "x_minBT_uci",
                                  "x_maxBT", "x_maxBT_lci", "x_maxBT_uci",
                                  "max_RGR", "max_RGR_lci", "max_RGR_uci",
                                  "area", "area_lci", "area_uci")]
mean_ci_table <- mean_ci_table %>% mutate_at(vars(-c(Population,Range.x)), funs(round(., 2)))
mean_ci_table$ToptCI <- paste(mean_ci_table$maximaBT, " [", mean_ci_table$maximaBT_lci, ", ", mean_ci_table$maximaBT_uci,"]", sep="")
mean_ci_table$B50CI <- paste(mean_ci_table$B50, " [", mean_ci_table$B50_lci, ", ", mean_ci_table$B50_uci,"]", sep="")
mean_ci_table$breadthCI <- paste(mean_ci_table$breadthBT, " [", mean_ci_table$breadthBT_lci, ", ", mean_ci_table$breadthBT_uci,"]", sep="")
mean_ci_table$x_minCI <- paste(mean_ci_table$x_minBT, " [", mean_ci_table$x_minBT_lci, ", ", mean_ci_table$x_minBT_uci,"]", sep="")
mean_ci_table$x_maxCI <- paste(mean_ci_table$x_maxBT, " [", mean_ci_table$x_maxBT_lci, ", ", mean_ci_table$x_maxBT_uci,"]", sep="")
mean_ci_table$max_RGRCI <- paste(mean_ci_table$max_RGR, " [", mean_ci_table$max_RGR_lci, ", ", mean_ci_table$max_RGR_uci,"]", sep="")
mean_ci_table$areaCI <- paste(mean_ci_table$area, " [", mean_ci_table$area_lci, ", ", mean_ci_table$area_uci,"]", sep="")
mean_ci_table$Range <- mean_ci_table$Range.x
mean_ci_table <- mean_ci_table[,c("Population",
                                  "Range",
                                  "ToptCI", 
                                  "B50CI", 
                                  "breadthCI",
                                  "x_minCI", 
                                  "x_maxCI", 
                                  "max_RGRCI", 
                                  "areaCI")]
mean_ci_table <- mean_ci_table[c(1,6:13,2:5,30:31,14:29),]
write.csv(mean_ci_table, "TPC-outputs/mean_ci_table_subset.csv")





# Make a plot that shows the tpc's for native (blue) and invasive (red) cohorts, 
# with 95% confidence intervals, for each population, including  
# thermal optima (vertical lines), 
# upper and lower thermal limits (notches on x axis),
# breadth (horizontal lines inside of curves), and 
# performance maxima (horizontal lines above curves)


plwd <- 0.5 # parameter lwd
pla <- 1 # parameter line alpha
tpclwd <- 1.5 # tpc lwd
tpca <- 1 # tpc line alpha

bayesFit_pops2 <- ggplot(data = filter(creds_pops, level == 95)) +
  geom_ribbon(aes(x = x, ymin = upper, ymax = lower, fill=Range), alpha = 0.3, inherit.aes = F) +
  geom_point(data = gutDat, position=position_jitter(w = 0.5), shape=21, alpha = 0.35,
             aes(daytemp, RGR, group=pop, color=Range), inherit.aes = F, size=1.25) + # data for family averages at each temperature
  labs(x = expression(paste("Temperature (°C)")), 
       y = expression(paste("RGR (cm cm"^-1, " day"^-1,")"))) + 
  scale_fill_manual(values=c("red", "blue")) + # fill for Range (invasive=red, native=blue)
  geom_line(aes(x, mu, group=pop, color=Range), inherit.aes = F, lwd = tpclwd, alpha=tpca) + # tpc for each group
  lemon::facet_rep_wrap(~pop, nrow=7, ncol=8, repeat.tick.labels = FALSE) +  # panel for each pop
  theme_bw() +
  theme(axis.text=element_text(size=11), axis.title=element_text(size=13),
        legend.title=element_blank(), legend.text = element_text(size = 10), legend.background = element_blank(), legend.position = c(20, 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5),
        strip.background = element_rect(colour=NA, fill=NA), strip.text.x = element_text(size=13, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_segment(data=mean_df, aes(x=maximaBT, y=max_RGR, xend=maximaBT, yend=0,group=pop, color=Range), 
               alpha=pla, lwd=plwd, inherit.aes=FALSE, lty=1) + # vertical lines for thermal optima
  scale_color_manual(values=c( "red", "blue"), labels = c("Invasive", "Native"))  +
  geom_segment(data=mean_df, aes(x=B50_low, y=max_RGR*0.5, xend=B50_high, yend=max_RGR*0.5, group=pop, color=Range), 
               alpha=pla, lwd=plwd, inherit.aes=FALSE, lty=1) + # horizontal line for B50 
  geom_segment(data=mean_df, aes(x=maximaBT, y=max_RGR, xend=0, yend=max_RGR, group=pop, color=Range), 
               alpha=1, lwd=plwd, inherit.aes=FALSE, lty=1) + # horizontal line for performance maximum
  geom_segment(data=mean_df, aes(x=x_minBT, y=0.15, xend=x_minBT, yend=0, group=pop, color=Range), 
               alpha=pla, lwd=plwd, inherit.aes=FALSE, lty=1) + # lower thermal limit
  geom_segment(data=mean_df, aes(x=x_maxBT, y=0.15, xend=x_maxBT, yend=0, group=pop, color=Range), 
               alpha=pla, lwd=plwd, inherit.aes=FALSE, lty=1) + # upper thermal limit
  scale_x_continuous(limits=c(0,50), expand = c(0, 0)) +
  scale_y_continuous(limits=c(-0.01,3.2), expand = c(0, 0), breaks = seq(0,3,by=0.5), minor_breaks = waiver()) +
  guides(fill=FALSE)
bayesFit_pops3 <-  lemon::reposition_legend(bayesFit_pops2, 'top left', panel = 'panel-1-1') # put the legend in the first panel

pdf("Figures/Curves by pop 2_subset.pdf", height=8, width=12)
figure <- ggarrange(bayesFit_pops3, ncol = 1, nrow = 1)
figure
dev.off()



# Make a two-panel plot shows the tpc's for invasive (red) and native (blue)  cohorts, 
# including  thermal optima (dotted vertical lines), 
# breadth (horizontal lines),  

plwd <- 0.25 # parameter lwd
pla <- 0.5 # parameter line alpha
tpclwd <- 0.5 # tpc lwd
tpca <- 0.75 # tpc line alpha

range_names <- list(
  'I'="Invasive",
  'N'="Native")
range_labeller <- function(variable,value){
  return(range_names[value])
}


bayesFit_pops5 <- ggplot(data = filter(creds_pops, level == 95)) +
  geom_point(data = gutDat, position=position_jitter(w = 0.5), shape=21, alpha = 0.35,
             aes(daytemp, RGR, group=pop, color=Range), inherit.aes = F, size=1.25) + # data for family averages at each temperature
  labs(x = expression(paste("Temperature (°C)")), 
       y = expression(paste("RGR (cm cm"^-1, " day"^-1,")"))) + 
  scale_fill_manual(values=c("red", "blue")) + # fill for Range (invasive=red, native=blue)
  geom_line(aes(x, mu, group=pop, color=Range), inherit.aes = F, lwd = tpclwd, alpha=tpca) + # tpc for each group
  facet_wrap(~Range, nrow=1, ncol=2, labeller=range_labeller) +  # panel for each range
  theme_bw() +
  theme(axis.text=element_text(size=11), axis.title=element_text(size=13), 
        legend.title=element_blank(), legend.text = element_text(size = 10), legend.background = element_blank(), legend.position = c(20, 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5),
        strip.background = element_rect(colour=NA, fill=NA), strip.text.x = element_text(size=13, colour="black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.spacing = unit(1, "lines"),
        plot.margin = margin(0,0.5,0,0.2, "cm")) +
  geom_segment(data=mean_df, aes(x=maximaBT, y=max_RGR, xend=maximaBT, yend=0,group=pop, color=Range), 
               alpha=pla, lwd=plwd, inherit.aes=FALSE, lty=1) + # vertical lines for thermal optima
  scale_color_manual(values=c( "red", "blue"))  +
  geom_segment(data=mean_df, aes(x=B50_low, y=max_RGR*0.5, xend=B50_high, yend=max_RGR*0.5, group=pop, color=Range), 
               alpha=pla, lwd=plwd, inherit.aes=FALSE, lty=1) + # horizontal line for B50 
  scale_x_continuous(limits=c(0,50), expand = c(0, 0)) +
  scale_y_continuous(limits=c(-0.01,max(gutDat$RGR)+0.1), expand = c(0, 0), breaks = seq(0,3,by=0.5), minor_breaks = waiver()) +
  guides(fill=FALSE)


##Fig. 3 in manuscript:
pdf("Figures/Manuscript-figs/Fig3.pdf", height=3.5, width=8)
ggarrange(bayesFit_pops5, ncol = 1, nrow = 1)
dev.off()
