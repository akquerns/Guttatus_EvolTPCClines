
#### PROJECT: Guttatus TPC project
#### PURPOSE: Plot pairwise comparisons between ranges, 
####          and among all populations
#### AUTHOR: Rachel Wooliver


###########################
# load packages
###########################
library(reshape2)
library(ggplot2)




###### 
###### PAIRWISE COMPARISONS BETWEEN NATIVE AND INVASIVE GROUPS
###### 
tidy_perf_pops <- read.csv("TPC-outputs/tidy_perf_pops_subset.csv")[,-1] # draws from the model

ndraws <- max(tidy_perf_pops$draw)

tidy_range <- data.frame(draw=rep(1:ndraws,2),
                         range=rep(c("I","N"), each=ndraws),
                         x_minBT=rep(NA, ndraws*2),
                         x_maxBT=rep(NA, ndraws*2),
                         maximaBT=rep(NA, ndraws*2),
                         breadthBT=rep(NA, ndraws*2),
                         B50=rep(NA, ndraws*2),
                         max_RGR=rep(NA, ndraws*2),
                         area=rep(NA,ndraws*2)) 

# fill out invasive & native averages
for(i in 1:dim(tidy_range)[1]){
  # invasive averages:
  tidy_range$x_minBT[which(tidy_range$draw==i & tidy_range$range=="I")] <- mean(tidy_perf_pops$x_minBT[which(tidy_perf_pops$Range=="I" & tidy_perf_pops$draw==i)])
  tidy_range$x_maxBT[which(tidy_range$draw==i & tidy_range$range=="I")] <- mean(tidy_perf_pops$x_maxBT[which(tidy_perf_pops$Range=="I" & tidy_perf_pops$draw==i)])
  tidy_range$maximaBT[which(tidy_range$draw==i & tidy_range$range=="I")] <- mean(tidy_perf_pops$maximaBT[which(tidy_perf_pops$Range=="I" & tidy_perf_pops$draw==i)])
  tidy_range$breadthBT[which(tidy_range$draw==i & tidy_range$range=="I")] <- mean(tidy_perf_pops$breadthBT[which(tidy_perf_pops$Range=="I" & tidy_perf_pops$draw==i)])
  tidy_range$B50[which(tidy_range$draw==i & tidy_range$range=="I")] <- mean(tidy_perf_pops$B50[which(tidy_perf_pops$Range=="I" & tidy_perf_pops$draw==i)])
  tidy_range$max_RGR[which(tidy_range$draw==i & tidy_range$range=="I")] <- mean(tidy_perf_pops$max_RGR[which(tidy_perf_pops$Range=="I" & tidy_perf_pops$draw==i)])
  tidy_range$area[which(tidy_range$draw==i & tidy_range$range=="I")] <- mean(tidy_perf_pops$area[which(tidy_perf_pops$Range=="I" & tidy_perf_pops$draw==i)])
  
  # native averages:
  tidy_range$x_minBT[which(tidy_range$draw==i & tidy_range$range=="N")] <- mean(tidy_perf_pops$x_minBT[which(tidy_perf_pops$Range=="N" & tidy_perf_pops$draw==i)])
  tidy_range$x_maxBT[which(tidy_range$draw==i & tidy_range$range=="N")] <- mean(tidy_perf_pops$x_maxBT[which(tidy_perf_pops$Range=="N" & tidy_perf_pops$draw==i)])
  tidy_range$maximaBT[which(tidy_range$draw==i & tidy_range$range=="N")] <- mean(tidy_perf_pops$maximaBT[which(tidy_perf_pops$Range=="N" & tidy_perf_pops$draw==i)])
  tidy_range$breadthBT[which(tidy_range$draw==i & tidy_range$range=="N")] <- mean(tidy_perf_pops$breadthBT[which(tidy_perf_pops$Range=="N" & tidy_perf_pops$draw==i)])
  tidy_range$B50[which(tidy_range$draw==i & tidy_range$range=="N")] <- mean(tidy_perf_pops$B50[which(tidy_perf_pops$Range=="N" & tidy_perf_pops$draw==i)])
  tidy_range$max_RGR[which(tidy_range$draw==i & tidy_range$range=="N")] <- mean(tidy_perf_pops$max_RGR[which(tidy_perf_pops$Range=="N" & tidy_perf_pops$draw==i)])
  tidy_range$area[which(tidy_range$draw==i & tidy_range$range=="N")] <- mean(tidy_perf_pops$area[which(tidy_perf_pops$Range=="N" & tidy_perf_pops$draw==i)])
}

# now calculate comparisons: native - invasive (positive means that native is greater than invasive, and vice versa)
range_comps <- data.frame(x_minBT=rep(NA, ndraws),
                          x_maxBT=rep(NA, ndraws),
                          maximaBT=rep(NA, ndraws),
                          breadthBT=rep(NA, ndraws),
                          B50=rep(NA, ndraws),
                          max_RGR=rep(NA, ndraws),
                          area=rep(NA,ndraws))


for(i in 1:dim(range_comps)[1]){
  range_comps$x_minBT[i] <- tidy_range$x_minBT[which(tidy_range$draw==i & tidy_range$range=="N")] - tidy_range$x_minBT[which(tidy_range$draw==i & tidy_range$range=="I")]
  range_comps$x_maxBT[i] <- tidy_range$x_maxBT[which(tidy_range$draw==i & tidy_range$range=="N")] - tidy_range$x_maxBT[which(tidy_range$draw==i & tidy_range$range=="I")]
  range_comps$maximaBT[i] <- tidy_range$maximaBT[which(tidy_range$draw==i & tidy_range$range=="N")] - tidy_range$maximaBT[which(tidy_range$draw==i & tidy_range$range=="I")]
  range_comps$breadthBT[i] <- tidy_range$breadthBT[which(tidy_range$draw==i & tidy_range$range=="N")] - tidy_range$breadthBT[which(tidy_range$draw==i & tidy_range$range=="I")]
  range_comps$B50[i] <- tidy_range$B50[which(tidy_range$draw==i & tidy_range$range=="N")] - tidy_range$B50[which(tidy_range$draw==i & tidy_range$range=="I")]
  range_comps$max_RGR[i] <- tidy_range$max_RGR[which(tidy_range$draw==i & tidy_range$range=="N")] - tidy_range$max_RGR[which(tidy_range$draw==i & tidy_range$range=="I")]
  range_comps$area[i] <- tidy_range$area[which(tidy_range$draw==i & tidy_range$range=="N")] - tidy_range$area[which(tidy_range$draw==i & tidy_range$range=="I")]
}

write.csv(range_comps, "TPC-outputs/Range-comparisons.csv")


###### 
###### RANGE PAIRWISE COMPARISON TABLE FOR ALL PARAMETERS
###### 

range_comps <- read.csv("TPC-outputs/Range-comparisons.csv")


## calculate mean difference in each parameter between native and invasive
means <- round(c(mean(range_comps$x_minBT), 
                 mean(range_comps$x_maxBT), 
                 mean(range_comps$maximaBT), 
                 mean(range_comps$breadthBT), 
                 mean(range_comps$B50), 
                 mean(range_comps$max_RGR),
                 mean(range_comps$area)),3)
## calculate credible interval for the differences
lowers <- as.vector(round(c(quantile(range_comps$x_minBT, probs = c(0.025)), 
                            quantile(range_comps$x_maxBT, probs = c(0.025)), 
                            quantile(range_comps$maximaBT, probs = c(0.025)), 
                            quantile(range_comps$breadthBT, probs = c(0.025)), 
                            quantile(range_comps$B50, probs = c(0.025)), 
                            quantile(range_comps$max_RGR, probs = c(0.025)),
                            quantile(range_comps$area, probs = c(0.025))),3))
uppers <- as.vector(round(c(quantile(range_comps$x_minBT, probs = c(0.975)), 
                            quantile(range_comps$x_maxBT, probs = c(0.975)), 
                            quantile(range_comps$maximaBT, probs = c(0.975)), 
                            quantile(range_comps$breadthBT, probs = c(0.975)), 
                            quantile(range_comps$B50, probs = c(0.975)), 
                            quantile(range_comps$max_RGR, probs = c(0.975)),
                            quantile(range_comps$area, probs = c(0.975))),3))
## paste together for a table
pwc <- paste(means, " [", lowers, ", ", uppers, "]", sep="")

## now export to csv
pwctable <- data.frame(Parameter=c("Lower thermal limit (°C)",
                                   "Upper thermal limit (°C)",
                                   "Thermal optimum (°C)",
                                   "Critical breadth (°C)",
                                   "B50 (°C)",
                                   "Performance maximum (cm/cm/day)",
                                   "Area"),
                       Pairwise.comparison=pwc)
pwctable <- pwctable[c(1,2,3,6,4,5,7),]
write.csv(pwctable, "TPC-outputs/Range-comparison-table.csv")


## visualize differences

density_x_minBT <- range_comps %>% 
  ggplot(aes(x_minBT)) +
  geom_vline(xintercept=0, size=2) +
  geom_density(fill="orange", alpha=0.1) +
  geom_point(aes(x=mean(range_comps$x_minBT), y=0), color="purple", shape=17, size=5) +
  xlab("Lower thermal limit (°C)") + 
  ylab("Density") +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


density_x_maxBT <- range_comps %>% 
  ggplot(aes(x_maxBT)) +
  geom_vline(xintercept=0, size=2) +
  geom_density(fill="orange", alpha=0.1) +
  geom_point(aes(x=mean(range_comps$x_maxBT), y=0), color="purple", shape=17, size=5) +
  xlab("Upper thermal limit (°C)") + 
  ylab("Density") +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

density_maximaBT <- range_comps %>% 
  ggplot(aes(maximaBT)) +
  geom_vline(xintercept=0, size=2) +
  geom_density(fill="orange", alpha=0.1) +
  geom_point(aes(x=mean(range_comps$maximaBT), y=0), color="purple", shape=17, size=5) +
  xlab("Thermal optimum (°C)") + 
  ylab("Density") +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

density_breadthBT <- range_comps %>% 
  ggplot(aes(breadthBT)) +
  geom_vline(xintercept=0, size=2) +
  geom_density(fill="orange", alpha=0.1) +
  geom_point(aes(x=mean(range_comps$breadthBT), y=0), color="purple", shape=17, size=5) +
  xlab("Critical breadth (°C)") + 
  ylab("Density") +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

density_B50 <- range_comps %>% 
  ggplot(aes(B50)) +
  geom_vline(xintercept=0, size=2) +
  geom_density(fill="orange", alpha=0.1) +
  geom_point(aes(x=mean(range_comps$B50), y=0), color="purple", shape=17, size=5) +
  xlab("B50 (°C)") + 
  ylab("Density") +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

density_max_RGR <- range_comps %>% 
  ggplot(aes(max_RGR)) +
  geom_vline(xintercept=0, size=2) +
  geom_density(fill="orange", alpha=0.1) +
  geom_point(aes(x=mean(range_comps$max_RGR), y=0), color="purple", shape=17, size=5) +
  xlab("Performance maximum (cm/cm/day)") + 
  ylab("Density") +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

density_area <- range_comps %>% 
  ggplot(aes(area)) +
  geom_vline(xintercept=0, size=2) +
  geom_density(fill="orange", alpha=0.1) +
  geom_point(aes(x=mean(range_comps$area), y=0), color="purple", shape=17, size=5) +
  xlab("Area") + 
  ylab("Density") +
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=12, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))



pdf("Figures/Manuscript-figs/Supp-Range-comparisons.pdf", height=10, width=10)
ggarrange(density_x_minBT, density_x_maxBT,
          density_maximaBT, density_max_RGR,
          density_breadthBT, density_B50,
          density_area,
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 3, nrow = 3)
dev.off()










###### 
###### PAIRWISE COMPARISONS BETWEEN POPULATIONS
###### 
# The pairwise comparison datasets that we will read in have values representing posterior  
# averages of the column group minus the row group. * denotes probability of one group 
# having a smaller parameter value than another is less than 0.025 or greater than 0.975, 
# and ** denotes probability of one group having a smaller parameter value than anoter is 
# less than 0.01 or greater than 0.99  

###### params (character vector of the performance parameters) 
params <- c("stretch", "maxima", "maximaBT", "breadth", "breadthBT", 
            "x_min", "x_minBT", "x_max", "x_maxBT", "area", 
            "B50_low", "B50_high", "B50", "max_RGR")
###### level (the probability of difference threshold. the table will print 
###### a "*" if the probability of one group having a smaller parameter value 
###### than another is less than 0.05 or greater than 0.95, and two "**" if it 
###### above the level value specified  -- default is 0.99. The values in the 
###### table are the posterior average of the column group minus the row group.)
hi <- 0.99
lo <- 1 - hi

###### pairwise comparison function

pairwise_probs_groups_av <- function(stan_df = stan_df, group, params, level = 0.99){
  group <- enquo(group)
  params %>% walk(function(param){
    
    var_df <- stan_df %>%
      dplyr::select(!!group, param, draw) %>%
      spread(!!group, param) %>%
      dplyr::select(-draw)
    
    spp_names <- names(var_df)
    
    comp_mat <- 
      seq_along(var_df) %>%
      map(~{
        seq_along(var_df) %>%
          map_chr(function(x){
            c1 <- var_df[[x]] 
            c2 <- var_df[[.x]] 
            comp_p <- mean(c1 > c2)
            mean_diff <- round(mean(c1 - c2), 3)
            
            case_when(
              (comp_p > hi | comp_p < lo) & mean_diff != 0 ~ str_glue("{mean_diff}**"),
              (comp_p > 0.975 | comp_p < 0.025) & mean_diff != 0 ~ str_glue("{mean_diff}*"), ### RCW altered this from ST's code that originally used the 0.95/0.05 thresholds
              TRUE ~ str_glue("{mean_diff}")
            )
            
          }) %>%
          rbind()
      }) %>%
      do.call(rbind, .)
    
    dimnames(comp_mat) <- list(spp_names, spp_names)
    
    write.csv(
      x = comp_mat, 
      quote = F, 
      row.names = T,
      file = str_glue("TPC-outputs/Pairwise-tables_subset/{param}_pairwise.csv") ### RCW altered this from ST's code that originally used the 0.95/0.05 thresholds (original file did not have the "_newthresh")
    )
  })
}

pairwise_probs_groups_av(stan_df = tidy_perf_pops, 
                         group = Population, 
                         params = params)



###############################
# Plotting parameter density plots
###############################

density_maximaBT <- tidy_perf_pops %>% 
  ggplot(aes(maximaBT, group = Population, fill = Range, colour=Range)) +
  geom_density(alpha=0.1) +
  scale_fill_manual(values=c("red", "blue")) +
  scale_colour_manual(values=c("red", "blue")) +
  facet_wrap(~Range, ncol=1) +
  xlab("Thermal optimum") + 
  ylab("Density") +
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=20, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


density_x_minBT <- tidy_perf_pops %>% 
  ggplot(aes(x_minBT, group = Population, fill = Range, colour=Range)) +
  geom_density(alpha=0.1) +
  scale_fill_manual(values=c("red", "blue")) +
  scale_colour_manual(values=c("red", "blue")) +
  facet_wrap(~Range, ncol=1) +
  xlab("Lower thermal limit") + 
  ylab("Density") +
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=20, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


density_x_maxBT <- tidy_perf_pops %>% 
  ggplot(aes(x_maxBT, group = Population, fill = Range, colour=Range)) +
  geom_density(alpha=0.1) +
  scale_fill_manual(values=c("red", "blue")) +
  scale_colour_manual(values=c("red", "blue")) +
  facet_wrap(~Range, ncol=1) +
  xlab("Upper thermal limit") + 
  ylab("Density") +
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=20, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


density_breadthBT <- tidy_perf_pops %>% 
  ggplot(aes(breadthBT, group = Population, fill = Range, colour=Range)) +
  geom_density(alpha=0.1) +
  scale_fill_manual(values=c("red", "blue")) +
  scale_colour_manual(values=c("red", "blue")) +
  facet_wrap(~Range, ncol=1) +
  xlab("Critical breadth") + 
  ylab("Density") +
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=20, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


density_B50 <- tidy_perf_pops %>% 
  ggplot(aes(B50, group = Population, fill = Range, colour=Range)) +
  geom_density(alpha=0.1) +
  scale_fill_manual(values=c("red", "blue")) +
  scale_colour_manual(values=c("red", "blue")) +
  facet_wrap(~Range, ncol=1) +
  xlab("B50") + 
  ylab("Density") +
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=20, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


density_max_RGR <- tidy_perf_pops %>% 
  ggplot(aes(max_RGR, group = Population, fill = Range, colour=Range)) +
  geom_density(alpha=0.1) +
  scale_fill_manual(values=c("red", "blue")) +
  scale_colour_manual(values=c("red", "blue")) +
  facet_wrap(~Range, ncol=1) +
  xlab("Performance maximum") + 
  ylab("Density") +
  theme(axis.text=element_text(size=18),axis.title=element_text(size=20),legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        strip.background = element_rect(colour="black", fill=NA), 
        strip.text.x = element_text(size=20, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))




##### 
##### area:
##### 

# read in data and order populations
dat <- read.csv("TPC-outputs/Pairwise-tables_subset/area_pairwise.csv", 
                row.names=1)[c(1,6:13,2:5,30:31,14:29), c(1,6:13,2:5,30:31,14:29)]
dat <- dat[c(31:1), c(31:1)]

# rename populations according to Aleah's new population names in 1-17bioclim_pops.csv
newpops <- read.csv("Raw-data/1-17bioclim_pops.csv")
dat2 <- dat

# create a dataset with the row names of dat in order
newnames <- data.frame(rows.old = rownames(dat2),
                       rows.new = rep("NA", length(rownames(dat2))))

# fill in newnames with the new pop names
for(i in 1:dim(newnames)[1]){
  newnames$rows.new[i] <- newpops$ID3[which(newpops$ID2==newnames$rows.old[i])]
}

# replace row and column names in dat
colnames(dat2) <- newnames$rows.new
rownames(dat2) <- newnames$rows.new

# now reorder dat2
dat3 <- dat2[c(order(rownames(dat2))), c(order(rownames(dat2)))]
dat4 <- dat3[c(1,6:13,2:5,14,24:31,15:23), c(1,6:13,2:5,14,24:31,15:23)]
dat5 <- dat4[c(31:1), c(31:1)]

# create a matrix the same size as the pairwise comparison table, but containing 
# 0's for non-significant comparisons and 1's for significant comparisons
aster <- matrix(NA, nrow=31, ncol=31)
for(i in 1:dim(aster)[1]){
  vals <- dat5[,i]
  for(j in 1:dim(aster)[1]){
    val <- as.numeric(as.character(vals[j]))
    if(is.na(val)==FALSE){aster[j,i] <- 0} else{aster[j,i] <- 1}
  }
}



# create a matrix the same size as the pairwise comparison table, but containing 
# numeric values (no asterisks)
nums <- matrix(NA, nrow=31, ncol=31)
for(i in 1:dim(nums)[1]){
  nums[,i] <- as.numeric(stringr::str_remove_all(as.character(dat5[,i]), "[*]")) * (-1)
}
rownames(nums) <- rownames(dat5)
colnames(nums) <- colnames(dat5)



# plot heatmap with asterisks for significant comparisons
# datasets: aster (0=nonsignif/1=signif matrix), and nums (values of dat5 without asterisks)
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(nums)

# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Now print asterisks onto heatmap. First get the melted upper triangle of aster
upper_tri_aster <- get_upper_tri(aster)
melted_cormat_aster <- melt(upper_tri_aster, na.rm = TRUE)
melted_cormat_aster$value[which(melted_cormat_aster$value==0)] <- NA
melted_cormat_aster$value[which(melted_cormat_aster$value==1)] <- "*"
melted_cormat$aster <- melted_cormat_aster$value


# Set limits of values
limits <- c(min(melted_cormat$value),max(melted_cormat$value))

# Heatmap, where red means y group is greater than x group
ggheatmapArea <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "black")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(limits), 
                       space = "Lab", 
                       name="Pairwise difference") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+
  geom_text(aes(Var2, Var1, label=aster), color = "black", shape=8) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.65),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))






################ Export plot
# Heatmaps indicate pairwise difference between groups,
# where red means x group is greater than y group 
# and, blue means x group is less than y group




library(ggpubr)
theme_set(theme_minimal())
pdf("Figures/Manuscript-figs/Supp-Pairwise-comparison-area_new-pop-names.pdf", height=7, width=7)
figure <- ggarrange(ggheatmapArea)
figure
dev.off()
 







