

#### PROJECT: Mimulus guttatus TPC project
#### PURPOSE: Create TPCs for each of the M. guttatus populations
#### using performr model (Tittes et al. 2019)

#### This is the subsetted R code for Aleah's thermal performance dataset 
#### as of 3 April, 2020. In this code, we average RGR data at the 
#### family level at each temperature to model a tpc for each population. 

## Code was written based on a performr vignette by Silas Tittes: https://silastittes.github.io/performr/
## plus extra functions that he has helped us to write
## performr is a package implements a probabilistic Bayesian hierarchical model (using Stan) 
## to predict tolerance/performance curves for a set of input taxa. The model is 
## described in Tittes et al. (2019). 






####
#### install/load packages, specify settings
####

# install necessary packages (run on Linux capable of using performr)
#devtools::install_github("silastittes/performr", local = FALSE) 
#install.packages(c("rstan", "tidyverse", "cowplot", "ggpubr"))

# load necessary packages
library(rstan)
library(devtools)
library(performr)
library(tidyverse)
library(ggridges)
library(cowplot)
library(ggpubr)
library(plotly)

# specify settings 
theme_set(theme_cowplot())
options(mc.cores = parallel::detectCores())
extract <- rstan::extract





####
#### Read in the raw, individual level data
####
RawDat1<- read.csv("Processed-data/RGRcalcs.csv")



#### total number of populations: 31 (18 native, 13 non-native)
length(unique(RawDat1$Population))

#### total number of families: 93 out of 95 total
length(unique(RawDat1$Family))
#### total number of temperatures: 6
length(unique(RawDat1$daytemp))





####
#### Visualize individual level data
####

dat <- RawDat1 
dat$RGR1 <- dat$RGRTOT1x
dat$Family <- as.factor(dat$Family)

# plot by population, with each line representing average RGR across temps
plot1 <- ggplot(dat, aes(x = daytemp, y = RGR1, group = Family)) +
  geom_point(aes(x=daytemp,y=RGR1,fill=Family),
             position = position_jitter(width=0.7), 
             size=1.5, shape=21, alpha=0.5) +
  guides(fill="none") +
  scale_y_continuous(name="RGR", limits=c(-0.1, max(na.omit(dat$RGR1)))) + 
  scale_x_continuous(name="Temperature", limits=c(0,50), breaks=seq(0,50,by=10)) + 
  theme_classic() +
  scale_fill_manual(values=rainbow(length(unique(dat$Family)))) + 
  facet_wrap(~Population, ncol=4) +
  theme(legend.position="none", panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  stat_summary(fun.y = mean, geom = "line", lwd=0.25, aes(color=Family)) +
  scale_color_manual(values=rainbow(length(unique(dat$Family))))
ggplotly(plot1)





####
#### Now average at the family level and visualize
####

# Create reference dataset of families and which population they're in
popfam <- data.frame(Family=levels(dat$Family),
                     Population=rep(NA, length(levels(dat$Family))))
for(i in 1:length(levels(dat$Family))){
  popfam$Population[i] <- unique(as.character(dat$Population[which(dat$Family==popfam$Family[i])]))
}

# average at the family level for each temp
# first remove temp=10 from the dataset (will remove this once we get the full dataset)
#dat <- dat[-which(dat$daytemp==10),]
famDat <- dat %>% 
  group_by(daytemp, Family) %>% 
  dplyr::summarize(RGR = mean(RGR1, na.rm=TRUE))

# add population to the family level dataset using our reference dataset (popfam)
famDat$Population <- rep(NA, dim(famDat)[1])
for(i in 1:dim(famDat)[1]){
  famDat$Population[i] <- unique(as.character(popfam$Population[which(popfam$Family==famDat$Family[i])]))
}
# then export the family level dataset so that we have it
write.csv(famDat, file="Processed-data/famDat_subset.csv")
famDat <- read.csv(file="Processed-data/famDat_subset.csv")
famDat$Family <- as.factor(famDat$Family)

plot2 <-ggplot(famDat, aes(x = daytemp, y = RGR, group = Family)) +
  geom_point(aes(x=daytemp,y=RGR,fill=Family),
             position = position_jitter(width=0.7), 
             size=1.5, shape=21, alpha=0.5) +
  guides(fill="none") +
  scale_y_continuous(name="RGR", limits=c(min(na.omit(famDat$RGR)), max(na.omit(famDat$RGR)))) + 
  scale_x_continuous(name="Temperature", limits=c(0,50), breaks=seq(0,50,by=10)) + 
  theme_classic() +
  scale_fill_manual(values=rainbow(length(unique(famDat$Family)))) + 
  facet_wrap(~Population, ncol=4) +
  theme(legend.position="none", panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  stat_summary(fun.y = mean, geom = "line", lwd=0.25, aes(color=Family)) +
  scale_color_manual(values=rainbow(length(unique(famDat$Family))))
ggplotly(plot2)




####
#### Transform family-level data and give each population a unique integer from 1 to 31
####

# export mean rgr and temp so that we can back-transform later
meansTot <- famDat %>% 
  dplyr::summarize(RGR = mean(RGR, na.rm=TRUE),
                   Temp = mean(daytemp, na.rm=TRUE))
write.csv(meansTot, "Processed-data/Means-Tot_subset.csv")

# now transform data: we will scale RGR by the grand mean, and center temperature around zero
gutDatMod <- read.csv("Processed-data/famDat_subset.csv") %>% 
  select(daytemp, Family, RGR, Population) %>% 
  drop_na() %>% 
  mutate(daytemp = daytemp - mean(daytemp),
         RGR = RGR / mean(RGR)
  )

# make a reference dataframe for population and the integer that it will be assigned in the model
popdat <- data.frame(PopulationI = unique(as.integer(gutDatMod$Population)),
                     Population = unique(gutDatMod$Population))
write.csv(popdat, file="Processed-data/popdat_subset.csv")

# convert population to integer for the model
gutDatMod$PopulationI <- rep(NA, dim(gutDatMod)[1])
for(i in 1:dim(gutDatMod)[1]){
  gutDatMod$PopulationI[i] <- popdat$PopulationI[which(popdat$Population==gutDatMod$Population[i])]
}

### gutDatMod is now ready to be put into the model
write.csv(gutDatMod, file="Processed-data/gutDatMod_subset.csv")
gutDatMod <- read.csv(file="Processed-data/gutDatMod_subset.csv")
str(gutDatMod)
# daytemp: our x variable that is centered around zero (numeric)
# RGR: our y variable that is scaled by the grand mean (numeric)
# PopulationI: our grouping variable that represents population (integer)


####
#### visualize our data for the model
####
plot3<- ggplot(gutDatMod, aes(x = daytemp, y = RGR, color=Population)) +
  geom_point(aes(x=daytemp,y=RGR, color=Population),
             position = position_jitter(width=0.7), 
             size=1.5, shape=21) +
  guides(fill="none") +
  scale_y_continuous(name="RGR") + 
  scale_x_continuous(name="Temperature") + 
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2)) +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  stat_summary(fun.y = mean, geom = "line", lwd=0.25, aes(color=Population)) +
  facet_wrap(~Population, ncol=4)
ggplotly(plot3)


# quantify mean/variation in temperature and RGR for each population
gutDatMod %>% 
  group_by(Population) %>% 
  summarise(mean_temp = mean(daytemp),
            var_tem = var(daytemp),
            mean_gr = mean(RGR),
            var_gr = var(RGR))




################ 
#### the model
### WARNING: THIS MODEL REQUIRES LOTS OF TIME AND COMPUTATIONAL POWER
################
perf_out <-
  performr::stan_performance(
    df = gutDatMod,
    response = RGR,
    treatment = daytemp,
    group_ids = PopulationI,
    seed = 1234, 
    max_treedepth = 15,
    adapt_delta = 0.95,
    file_id =  "TPC-outputs/Model/stan_pops_subset",
    iter = 4000
  )


################


####
#### read in the model output and look at model diagnostics
####


model_pops <- rstan::read_stan_csv(c("TPC-outputs/Model/stan_pops_subset.samples_1.csv", 
                                     "TPC-outputs/Model/stan_pops_subset.samples_2.csv",
                                     "TPC-outputs/Model/stan_pops_subset.samples_3.csv", 
                                     "TPC-outputs/Model/stan_pops_subset.samples_4.csv"))

## rhat should be very close to 1, and neff should be close to the number of 
## posterior samples
sumtab <- round(rstan::summary(model_pops)$summary, digits=2)
write.csv(sumtab, file="TPC-outputs/model-summary_subset.csv")




############ 
# Generate a tidy data frame with all the parameters, plus some 
# valuable derived parameters, like the optimum, area, and breadth for each species. 
# We will use this data frame for other tasks below as well.

head(
  tidy_perf_pops <- 
    performr::perform_df(
      model_pops, 
      species_order = c(1:31)
    ) 
)
### remember that parameters within tidy_perf_pops are still transformed so we will need to back-transform them 
# species: integers representing our different groups (populations in this case)
# x_min: location along the environmental axis left of the optimum where the response trait falls to 0 
# draw: the step within the chain 
# x_max: location along the environmental axis right of the optimum where the response trait falls to 0 
# shape1: first of the two parameters that modifies curve asymmetry; when shape1 is larger than shape2 , the curve will skew right
# shape2: second parameter that modifies curve asymmetry; when shape2 is larger than shape1 , the curve will skew left
# stretch: the maximum expected value of the response trait (maximum RGR)
# nu: variance?
# maxima: location along the environmental axis left of the optimum where the response trait is maximized (thermal optimum)
# breadth: x_max - x_min (critical breadth)
# area: area under the curve
# special: ??


############ 
# Calculate performance maximum for each iteration of the model
# To get the actual RGR max value, it should just be the value of the Kumaraswamy-ish 
# function at the optimum.  The values and scale will differ, but it should be very 
# correlated with the value of stretch. If you want to calculate it, the easiest 
# would be to use performr::performance_mu(). 
# This mutate() function seems to work to add in the variable for max_RGR in-place.
# Sadly, kinda slow, but do() adds a performance bar, so that's fun! 

tidy_perf_pops %<>%
  ungroup() %>% 
  mutate(idx = 1:n()) %>% 
  group_by(idx) %>% 
  do({
    mutate(.data = ., max_RGR = performance_mu(xs = .$maxima, .$shape1, .$shape2, .$stretch, .$x_min, .$x_max))
  })


##########
# To the tidyframe dataset, add: 

# population column
tidy_perf_pops$Population <- rep(NA, dim(tidy_perf_pops)[1])
for(i in 1:dim(tidy_perf_pops)[1]){
  tidy_perf_pops$Population[i] <- as.character(popdat$Population[which(popdat$PopulationI==tidy_perf_pops$species[i])])
}
tidy_perf_pops$Population <- as.factor(tidy_perf_pops$Population)
table(tidy_perf_pops$Population, tidy_perf_pops$species) # check

# range column
tidy_perf_pops$Range <- rep(NA, dim(tidy_perf_pops)[1])
tidy_perf_pops$Range[which(tidy_perf_pops$species %in% as.character(c(1:13)))] <- "I"
tidy_perf_pops$Range[which(tidy_perf_pops$species %in% as.character(c(14:31)))] <- "N"
tidy_perf_pops$Range <- as.factor(tidy_perf_pops$Range)
table(tidy_perf_pops$Range, tidy_perf_pops$Population) # check






############ 
# Bayesian p-value: 
# A Bayesian p-value of 0.5 suggests the model generates data that look like the true data.

# The function: bayes_p()
# Arguments include:
# stan_df -- the tidy stan data frame
# raw_df -- the data frame used as input to performr
# raw_group -- the column name that contains the groups the model was fit with *in quotes*
# raw_treatment -- the column name that contains the treatment variable the model was fit with *in quotes* 
# raw_response -- the column name that contains the response variable the model was fit with *in quotes* 
bayes_p <- function(stan_df, raw_df, raw_group, raw_treatment, raw_response, ndraw = nrow(stan_df)){
  
  1:ndraw %>% 
    map_df(~{
      draw_i <- stan_df[.x, ]
      tidy_spp <- stan_df$species[.x]
      df_i <- raw_df[raw_df[[raw_group]] == tidy_spp,]
      
      
      mus <- performance_mu(xs = df_i[[raw_treatment]], 
                            shape1 = draw_i$shape1, 
                            shape2 = draw_i$shape2, 
                            stretch = draw_i$stretch, 
                            x_min = draw_i$x_min,
                            x_max = draw_i$x_max
      )
      
      pseudo <- posterior_predict(x = df_i[[raw_treatment]], draw_i)  
      
      data.frame(
        ssq_obs = sum((mus - df_i[[raw_response]])^2),
        ssq_pseudo = sum((mus - pseudo$trait)^2),
        species = tidy_spp
      )
      
    })
}


# function use
ssq_df <- bayes_p(
  stan_df = tidy_perf_pops, 
  raw_df = gutDatMod, 
  raw_treatment = "daytemp", 
  raw_response = "RGR", 
  raw_group = "PopulationI")

# add a Population column to ssq_df
ssq_df$Population <- rep(NA, dim(ssq_df)[1])
for(i in 1:dim(ssq_df)[1]){
  ssq_df$Population[i] <- as.character(popdat$Population[which(popdat$PopulationI==ssq_df$species[i])])
}
ssq_df$Population <- as.factor(ssq_df$Population)
table(ssq_df$Population, ssq_df$species) # check

# write ssq_df to csv
write.csv(ssq_df, "TPC-outputs/Model/ssq_df_subset.csv") 
ssq_df <- read.csv("TPC-outputs/Model/ssq_df_subset.csv") 
View(ssq_df)


# compute bayesian p value
ssq_df %>% 
  summarise(b_pval=mean(ssq_obs > ssq_pseudo))
################################################# 
# overall bayesian p value = 0.5326331
################################################# 




# compute bayesian p value for each Population
ssq_group <- ssq_df %>% 
  dplyr::group_by(Population) %>%
  summarise(b_pval=mean(ssq_obs > ssq_pseudo))
ssq_group <- as.data.frame(ssq_group)


# in ssq_group, merge population names with their p values
ssq_group$ps <- round(ssq_group$b_pval,2)
ssq_group$Pop_ps <- paste(ssq_group$Population," (",ssq_group$ps,")", sep="")
# then write the file so we have it saved
write.csv(ssq_group, "TPC-outputs/p-values-group_subset.csv")  


# in ssq_df dataset, create a column of populations with their associated p value
ssq_df$Pop_ps <- rep(NA, dim(ssq_df)[1])
for(i in 1:dim(ssq_df)[1]){
  ssq_df$Pop_ps[i] <- as.character(ssq_group$Pop_ps[which(ssq_group$Population==ssq_df$Population[i])])
}
ssq_df$Pop_ps <- as.factor(ssq_df$Pop_ps)


# subsample to make a figure
subsamp <-  ssq_df%>%
  group_by(Population)%>%
  sample_frac(size = 0.25, replace = F)


# figure of predicted vs. observed ssq, colored by population
ssq_plot <- ggplot(subsamp, aes(y=log(ssq_pseudo), x=log(ssq_obs), color=Pop_ps)) +
  geom_point(alpha=0.05) +
  geom_abline(intercept=0,slope=1,linetype="dashed") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  xlab("Log observed ssq") +
  ylab("Log posterior predictive ssq") +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=18),
        panel.border = element_rect(colour = "black", fill=NA, size=1.5),
        strip.background = element_rect(colour=NA, fill=NA), strip.text.x = element_text(size=18, face="bold"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#pdf("Figures/ssq_av_subset.pdf", height=5, width=6.5)
ssq_plot
#dev.off()










############ 
# CALCULATE B50
################
# THE FUNCTION:

optimum_breadth <- 
  function(par_df, prop_max = 0.5, n_grid = 100){
    1:nrow(par_df) %>% map_df(function(i){
      xs = seq(par_df$x_min[i],
               par_df$x_max[i],
               length.out = n_grid)
      
      sim_mu <- performr::performance_mu(
        xs = xs, 
        shape1 = par_df$shape1[i],
        shape2 = par_df$shape2[i],
        stretch = par_df$stretch[i],
        x_min = par_df$x_min[i],
        x_max = par_df$x_max[i]
      )
      
      max_y <- which.max(sim_mu)
      prop_max_y <- sim_mu[max_y] * prop_max
      idx_max_low <- which.min((sim_mu[1:(max_y-1)] - prop_max_y)^2)
      idx_max_high <- which.min((sim_mu[max_y:length(sim_mu)] - prop_max_y)^2) + max_y
      tibble("opt_breadth_low" = xs[idx_max_low],
             "opt_breadth_high" = xs[idx_max_high], 
             "opt_breadth" = xs[idx_max_high] - xs[idx_max_low])
    })
  }

#########
# USAGE #
#########
# par_df: the tidy dataframe
# prop_max: threshold to calculate breadth from
# The higher prop_max, the narrower the output breadth will be as the interval is moving nearer to the optimum. 
# increase n_grid for more precise approximation
tidy_perf_pops %<>% 
  ungroup() %>% 
  bind_cols(
    optimum_breadth(par_df = tidy_perf_pops, 
                    prop_max = 0.5, 
                    n_grid = 100)
  )
# For each posterior draw, the function reports: 
# opt_breadth_low: lower limit of the calculated breadth
# opt_breadth_high: upper limit of the calculated breadth
# opt_breadth: difference between low and high, i.e., the breadth 









############ making creds_pops dataframe
# The next step is to generate prediction intervals using the predict_interval() 
# function. The function generates posterior quantiles for each set of posterior 
# draws specified (x_draws), and averages over the quantiles. The argument p can 
# takes a vector of credible levels, which can be modified to consider other and/or 
# additional levels.


############ 
# We can access the posterior draws using rstan::extract(), which produces a list 
# containing draws for each parameter of the model.
draws_pops <- rstan::extract(model_pops)
ndraws_pops <- length(draws_pops$lp__)


############ 
# set up sequence from smallest to largest x 
x_seq_pops = seq(min(draws_pops$x_min),
                 max(draws_pops$x_max),
                 length.out = 100)

############ 
# sub-sample draws randomly
poly_draws_pops <- sample(1:ndraws_pops, 100)

############ 
# function to calculate creds_pops
predict_interval <- function (x, spp, par_df, x_draws, p){
  if (missing(x)) {
    x <- seq(min(par_df$x_min), max(par_df$x_max), length.out = 100)
  }
  sub_df <- par_df %>% filter(draw %in% x_draws)
  
  p %>% map_df(~{
    posterior_quantile(x = x, par_df = sub_df, p = .x) %>%
      group_by(species, x) %>%
      summarise_all(.funs = mean) %>%
      mutate(level = .x) %>%
      arrange(x) %>%
      dplyr::select(-draw) %>%
      mutate(level = round(.x * 100, 0))
  }) %>%
    arrange(species, x)
}

############ 
# calculate creds_pops
head(
  creds_pops <- 
    predict_interval(
      x = x_seq_pops,
      spp = species,
      par_df = tidy_perf_pops,
      x_draws = poly_draws_pops,
      p = c(0.95, 0.5))
)
# Notice the output of predict_interval() also produces a “mu” column, which 
# contains the mean prediction of the curve for each input species.






############
############
## For all of our dataframes, back-transform parameters and add column for population and range
############
############


############ 
# tidy_perf_pops: posterior draws
############
tidy_perf_pops$x_minBT <- tidy_perf_pops$x_min + meansTot$Temp 
tidy_perf_pops$x_maxBT <- tidy_perf_pops$x_max + meansTot$Temp
tidy_perf_pops$stretchBT <- tidy_perf_pops$stretch * meansTot$RGR
tidy_perf_pops$maximaBT <- tidy_perf_pops$maxima + meansTot$Temp
tidy_perf_pops$breadthBT <- tidy_perf_pops$x_maxBT-tidy_perf_pops$x_minBT
tidy_perf_pops$B50_low <- tidy_perf_pops$opt_breadth_low + meansTot$Temp 
tidy_perf_pops$B50_high <- tidy_perf_pops$opt_breadth_high + meansTot$Temp 
tidy_perf_pops$B50 <- tidy_perf_pops$B50_high - tidy_perf_pops$B50_low 
tidy_perf_pops$max_RGR <- tidy_perf_pops$max_RGR * meansTot$RGR
# x_minBT: lower thermal limit
# x_maxBT: upper thermal limit
# stretchBT: performance maximum
# maximaBT: thermal optimum
# breadthBT: cricial breadth
# B50_low: lower limit of B50
# B50_high: upper limit of B50
# B50: span of temperatures across which at least 50% of maximum RGR is achieved



# write to csv
write.csv(tidy_perf_pops, "TPC-outputs/tidy_perf_pops_subset.csv")


############ 
# creds_pops: tpcs and credible intervals
############
creds_pops$x <- creds_pops$x + meansTot$Temp
creds_pops$mu <- creds_pops$mu * meansTot$RGR
creds_pops$upper <- creds_pops$upper * meansTot$RGR
creds_pops$lower <- creds_pops$lower * meansTot$RGR
creds_pops$Population <- as.character(rep("I1", dim(creds_pops)[1]))
for(i in 1:dim(creds_pops)[1]){
  creds_pops$Population[i] <- as.character(popdat$Population[which(as.integer(creds_pops$species[i])==popdat$PopulationI)])
}
creds_pops$Population <- as.factor(creds_pops$Population)
table(creds_pops$species, creds_pops$Population) # check

# range column
creds_pops$Range <- rep(NA, dim(creds_pops)[1])
creds_pops$Range[which(creds_pops$species %in% as.character(c(1:13)))] <- "I"
creds_pops$Range[which(creds_pops$species %in% as.character(c(14:31)))] <- "N"
creds_pops$Range <- as.factor(creds_pops$Range)
table(creds_pops$Range, creds_pops$Population) # check

# write to csv
write.csv(creds_pops, "TPC-outputs/creds_pops_subset.csv")


############ 
# gutDat: family-averaged data used for the model
############
gutDat <- gutDatMod
gutDat$daytemp <- gutDat$daytemp + meansTot$Temp
gutDat$RGR <- gutDat$RGR * meansTot$RGR

# range column
gutDat$Range <- rep(NA, dim(gutDat)[1])
gutDat$Range[which(gutDat$PopulationI %in% as.integer(c(1:13)))] <- "I"
gutDat$Range[which(gutDat$PopulationI %in% as.integer(c(14:31)))] <- "N"
gutDat$Range <- as.factor(gutDat$Range)
table(gutDat$Range, gutDat$Population) # check

# write to csv
write.csv(gutDat, "Processed-data/gutDat_subset.csv")







############
############ 
# get mean values of each parameter and 95% credible intervals for the ones we're interested in
############
############ 
mean_df <- tidy_perf_pops %>% 
  group_by(Population) %>% 
  summarise_if(is.numeric, .funs = c(mean))
mean_df$Range <- rep("I", dim(mean_df)[1])
mean_df$Range[14:31] <- "N"
mean_df$Range <- as.factor(mean_df$Range)


write.csv(mean_df, "TPC-outputs/mean_df_subset.csv")

mean_df_ci <- tidy_perf_pops %>% 
  group_by(Population) %>% 
  summarise(maximaBT_lci = quantile(maximaBT, probs=c(0.025)),
            maximaBT_uci = quantile(maximaBT, probs=c(0.975)),
            B50_lci = quantile(B50, probs=c(0.025)),
            B50_uci = quantile(B50, probs=c(0.975)),
            B50_low_lci = quantile(B50_low, probs=c(0.025)),
            B50_low_uci = quantile(B50_low, probs=c(0.975)),
            B50_high_lci = quantile(B50_high, probs=c(0.025)),
            B50_high_uci = quantile(B50_high, probs=c(0.975)),
            breadthBT_lci = quantile(breadthBT, probs=c(0.025)),
            breadthBT_uci = quantile(breadthBT, probs=c(0.975)),
            x_minBT_lci = quantile(x_minBT, probs=c(0.025)),
            x_minBT_uci = quantile(x_minBT, probs=c(0.975)),
            x_maxBT_lci = quantile(x_maxBT, probs=c(0.025)),
            x_maxBT_uci = quantile(x_maxBT, probs=c(0.975)),
            max_RGR_lci = quantile(max_RGR, probs=c(0.025)),
            max_RGR_uci = quantile(max_RGR, probs=c(0.975)),
            area_lci = quantile(area, probs=c(0.025)),
            area_uci = quantile(area, probs=c(0.975)))
mean_df_ci$Range <- rep("I", dim(mean_df_ci)[1])
mean_df_ci$Range[14:31] <- "N"
mean_df_ci$Range <- as.factor(mean_df_ci$Range)

write.csv(mean_df_ci, "TPC-outputs/mean_df_ci_subset.csv")








