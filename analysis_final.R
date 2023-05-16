###################################################################################
###############################    MODELS    ######################################
###################################################################################

# This file loads a cleaned data with 3 alternatives and runs models for Table 3 
# Then, it creates graphs of the WTP distributions. 


####################################################
########   Check for and load Packages   ###########
####################################################

## Clear worksace
rm(list = ls())
gc()

## This function will check if a package is installed, and if not, install it
pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}
#install.packages("tidyverse")
packages <- c("gmnl","mlogit","data.table",'haven') ## you can add more packages here
lapply(packages, pkgTest)
library(devtools)
#install_github("dgrtwo/broom")
library(broom)
library(tidyverse)

data <- read.csv("cleaned_data_for_R.csv")

# Create some dummy variables for attributes
#rename(data, respondent_id=id)
data$choice <- data$c1
data$comp1 <- ifelse(data$compensation == 1, 1, 0)
data$comp2 <- ifelse(data$compensation == 2, 1, 0)
data$voty<- ifelse(data$vot == 1, 1, 0)
data$npopvoty <- data$npop*data$voty
data$npop2voty <- data$npop2*data$voty

################################################################################
#####################   Load and generate dataframes ###########################
################################################################################
## Create mlogit data
d <-  mlogit.data(data,
                  id.var='id',
                  chid.var = 'id_card',
                  choice='choice',
                  shape='long',
                  alt.var='n')
#opposite=c('ncost'))

# subset data to yes votes
votedy=subset(data,vot==1)
dy <-  mlogit.data(votedy,
                   id.var='id',
                   chid.var = 'id_card',
                   choice='choice',
                   shape='long',
                   alt.var='n')
# subset data to no votes
votedn=subset(data,vot==2)
dn <-  mlogit.data(votedn,
                   id.var='id',
                   chid.var = 'id_card',
                   choice='choice',
                   shape='long',
                   alt.var='n')

################################################################################
#############################  Model Runs  #####################################
################################################################################

# Make sure data and program is working
null_model <- mlogit(choice ~ asc | 0, data=d)
summary(null_model)

################################################################################
# Full data set with correlated random parameters and survey weights
# variables with n in front are normalized variables. All other variables are 
# dummy variables. All parameters are assumed to have normal distributions. 
################################################################################
ideal <-  gmnl(choice ~ ncost + npop + npop2 + comp1 + comp2 + cost_sharing + nlivestock + ngove + hunt + asc | 0 ,
               data=d,
               ranp=c(ncost = 'n', npop = 'n', comp1 = 'n', comp2 = 'n', npop2 = 'n', nlivestock = 'n', hunt = 'n', ngove = 'n', cost_sharing = 'n', asc = 'n'),
               model='mixl',
               panel=TRUE,
               correlation=TRUE,
               seed=42,
               R = 50, 
               weights = weight,
               print.level = 2)
summary(ideal)

# estimate wtp 
library(msm)
estvar<-vcov(ideal)
estmean<-coef(ideal)
# x1 is the first coefficient in the model. This first line provides se of ncost
se<-deltamethod(~ x1, estmean,estvar,ses=TRUE)

# standard deviations of each variable
sdcost <- sd(data$cost)
sdpop <- sd(data$pop)
sdpop2 <- sd(data$pop2)
sdcost_sharing <- sd(data$cost_sharing)
sdlivestock <- sd(data$livestock)
sdgove <- sd(data$gove)
sdhunt <- sd(data$hunt)

# get wtp for each attribute
# pop
((estmean[2]/sdpop)+(estmean[3]/sdpop2)*400)/(-estmean[1]/sdcost)*200
# comp1 
estmean[4]/(-estmean[1]/sdcost)
# comp2 
estmean[5]/(-estmean[1]/sdcost)
# cost_sharing
(estmean[6]/sdcost_sharing)/(-estmean[1]/sdcost)
# livestock
(estmean[7]/sdlivestock)/(-estmean[1]/sdcost)
# gove
(estmean[8]/sdgove)/(-estmean[1]/sdcost)
# hunt
(estmean[9])/(-estmean[1]/sdcost)


#standard errors of wtp for each variable
# pop
deltamethod(~ ((x2/sdpop)+(x3/sdpop2)*400)/(-x1/sdcost)*200, estmean,estvar,ses=TRUE)
# comp1 
deltamethod(~ x4/(-x1/sdcost), estmean,estvar,ses=TRUE)
# comp2 
deltamethod(~ x5/(-x1/sdcost), estmean,estvar,ses=TRUE)
# cost_sharing
deltamethod(~ (x6/sdcost_sharing)/(-x1/sdcost), estmean,estvar,ses=TRUE)
# livestock
deltamethod(~ (x7/sdlivestock)/(-x1/sdcost), estmean,estvar,ses=TRUE)
# gove
deltamethod(~ (x8/sdgove)/(-x1/sdcost), estmean,estvar,ses=TRUE)
# hunt
deltamethod(~ (x9)/(-x1/sdcost), estmean,estvar,ses=TRUE)

################################################################################
# This next part saves the random parameters for each respondent in a vector 
# that can be used to create WTP for each individual. 
################################################################################
# Save random parameters on population, population squared, and cost for each individual 
param <- effect.gmnl(ideal,par = "npop", effect = "ce")
npop_coeff <- param$mean
npop <- data.frame(npop_coeff)
write.csv(npop, "npop_coef.csv")
npop <- mutate(npop, id = rownames(npop))
rm(param)

param <- effect.gmnl(ideal, par = "npop2", effect = "ce")
npop2_coeff <- param$mean
npop2 <- data.frame(npop2_coeff)
write.csv(npop2, "npop2_coef.csv")
npop2 <- mutate(npop2, id = rownames(npop2))
rm(param)

param <- effect.gmnl(ideal, par = "ncost", effect = "ce")
ncost_coeff <- param$mean
ncost <- data.frame(ncost_coeff)
write.csv(ncost, "ncost_coef.csv")
ncost <- mutate(ncost, id = rownames(ncost))
rm(param)
# Merge m, m2, m3
all <- merge(npop,npop2,by="id")
all2 <- merge(all,ncost,by="id")
# wtp
# get the cost coefficient
cost_coeff <- summary(ideal)$coefficients["ncost"]
all$wtp <- (all$npop_coef/227.2321+all$npop2_coef/139213.5*400)/(-cost_coeff/73.4816)*200
quantile(all$wtp, c(.01,.99))
hist(all$wtp[all$wtp>-78.21465 & all$wtp <87.81276 ], main = "WTP" ,)
summary(all$wtp)

# This is used later
write.csv(all, "wtp_with_weights.csv")

################################################################################
# Ideal weighted on Yes voters
################################################################################
idealy <-  gmnl(choice ~ ncost + npop + npop2 + comp1 + comp2 + cost_sharing + nlivestock + ngove + hunt + asc | 0 ,
                data=dy,
                ranp=c(ncost = 'n', npop = 'n', comp1 = 'n', comp2 = 'n', npop2 = 'n', nlivestock = 'n', hunt = 'n', ngove = 'n', cost_sharing = 'n', asc = 'n'),
                model='mixl',
                panel=TRUE,
                correlation=TRUE,
                seed=42,
                R = 50, 
                weights = weight,
                print.level = 2)
summary(idealy)
wtp.gmnl(idealy, wrt="ncost")

estvar<-vcov(idealy)
estmean<-coef(idealy)

# get wtp for each variable
# pop
((estmean[2]/sdpop)+(estmean[3]/sdpop2)*400)/(-estmean[1]/sdcost)*200
# comp1 
estmean[4]/(-estmean[1]/sdcost)
# comp2 
estmean[5]/(-estmean[1]/sdcost)
# cost_sharing
(estmean[6]/sdcost_sharing)/(-estmean[1]/sdcost)
# livestock
(estmean[7]/sdlivestock)/(-estmean[1]/sdcost)
# gove
(estmean[8]/sdgove)/(-estmean[1]/sdcost)
# hunt
(estmean[9])/(-estmean[1]/sdcost)


#standard errors of wtp for each variable
# pop
deltamethod(~ ((x2/sdpop)+(x3/sdpop2)*400)/(-x1/sdcost)*200, estmean,estvar,ses=TRUE)
# comp1 
deltamethod(~ x4/(-x1/sdcost), estmean,estvar,ses=TRUE)
# comp2 
deltamethod(~ x5/(-x1/sdcost), estmean,estvar,ses=TRUE)
# cost_sharing
deltamethod(~ (x6/sdcost_sharing)/(-x1/sdcost), estmean,estvar,ses=TRUE)
# livestock
deltamethod(~ (x7/sdlivestock)/(-x1/sdcost), estmean,estvar,ses=TRUE)
# gove
deltamethod(~ (x8/sdgove)/(-x1/sdcost), estmean,estvar,ses=TRUE)
# hunt
deltamethod(~ (x9)/(-x1/sdcost), estmean,estvar,ses=TRUE)

param <- effect.gmnl(idealy,par = "npop", effect = "ce")
npop_coeff <- param$mean
npop <- data.frame(npop_coeff)
#write.csv(npop, "D:/OneDrive - Colostate/Wolf_CV/final/npop_coef_no_weight.csv")
npop <- mutate(npop, id = rownames(npop))
rm(param)

param <- effect.gmnl(idealy, par = "npop2", effect = "ce")
npop2_coeff <- param$mean
npop2 <- data.frame(npop2_coeff)
#write.csv(npop2, "D:/OneDrive - Colostate/Wolf_CV/final/npop2_coef_no_weight.csv")
npop2 <- mutate(npop2, id = rownames(npop2))
rm(param)

param <- effect.gmnl(idealy, par = "ncost", effect = "ce")
ncost_coeff <- param$mean
ncost <- data.frame(ncost_coeff)
#write.csv(ncost, "D:/OneDrive - Colostate/Wolf_CV/final/ncost_coef_no_weight.csv")
ncost <- mutate(ncost, id = rownames(ncost))
rm(param)
# Merge m, m2, m3
ally <- merge(npop,npop2,by="id")
all2 <- merge(all,ncost,by="id")
# wtp
# get the cost coefficient
cost_coeff <- summary(idealy)$coefficients["ncost"]
ally$wtp <- (ally$npop_coef/227.2321+ally$npop2_coef/139213.5*400)/(-cost_coeff/73.4816)*200
quantile(ally$wtp, c(.01,.99))
hist(ally$wtp[ally$wtp>-61 & ally$wtp <160], main = "WTP" ,)
summary(all$wtp)

################################################################################
# Ideal weighted on No voters
################################################################################
idealn <-  gmnl(choice ~ ncost + npop + npop2 + comp1 + comp2 + cost_sharing + nlivestock + ngove + hunt + asc | 0 ,
                data=dn,
                ranp=c(ncost = 'n', npop = 'n', comp1 = 'n', comp2 = 'n', npop2 = 'n', nlivestock = 'n', hunt = 'n', ngove = 'n', cost_sharing = 'n', asc = 'n'),
                model='mixl',
                panel=TRUE,
                correlation=TRUE,
                seed=42,
                R = 50, 
                weights = weight,
                print.level = 2)
summary(idealn)
wtp.gmnl(idealn, wrt="ncost")

estvar<-vcov(idealn)
estmean<-coef(idealn)

# get wtp for each variable
# pop
((estmean[2]/sdpop)+(estmean[3]/sdpop2)*400)/(-estmean[1]/sdcost)*200
# comp1 
estmean[4]/(-estmean[1]/sdcost)
# comp2 
estmean[5]/(-estmean[1]/sdcost)
# cost_sharing
(estmean[6]/sdcost_sharing)/(-estmean[1]/sdcost)
# livestock
(estmean[7]/sdlivestock)/(-estmean[1]/sdcost)
# gove
(estmean[8]/sdgove)/(-estmean[1]/sdcost)
# hunt
(estmean[9])/(-estmean[1]/sdcost)


#standard errors of wtp for each variable
# pop
deltamethod(~ ((x2/sdpop)+(x3/sdpop2)*400)/(-x1/sdcost)*200, estmean,estvar,ses=TRUE)
# comp1 
deltamethod(~ x4/(-x1/sdcost), estmean,estvar,ses=TRUE)
# comp2 
deltamethod(~ x5/(-x1/sdcost), estmean,estvar,ses=TRUE)
# cost_sharing
deltamethod(~ (x6/sdcost_sharing)/(-x1/sdcost), estmean,estvar,ses=TRUE)
# livestock
deltamethod(~ (x7/sdlivestock)/(-x1/sdcost), estmean,estvar,ses=TRUE)
# gove
deltamethod(~ (x8/sdgove)/(-x1/sdcost), estmean,estvar,ses=TRUE)
# hunt
deltamethod(~ (x9)/(-x1/sdcost), estmean,estvar,ses=TRUE)

param <- effect.gmnl(idealn,par = "npop", effect = "ce")
npop_coeff <- param$mean
npop <- data.frame(npop_coeff)
#write.csv(npop, "D:/OneDrive - Colostate/Wolf_CV/final/npop_coef_no_weight.csv")
npop <- mutate(npop, id = rownames(npop))
rm(param)

param <- effect.gmnl(idealn, par = "npop2", effect = "ce")
npop2_coeff <- param$mean
npop2 <- data.frame(npop2_coeff)
#write.csv(npop2, "D:/OneDrive - Colostate/Wolf_CV/final/npop2_coef_no_weight.csv")
npop2 <- mutate(npop2, id = rownames(npop2))
rm(param)

param <- effect.gmnl(idealn, par = "ncost", effect = "ce")
ncost_coeff <- param$mean
ncost <- data.frame(ncost_coeff)
#write.csv(ncost, "D:/OneDrive - Colostate/Wolf_CV/final/ncost_coef_no_weight.csv")
ncost <- mutate(ncost, id = rownames(ncost))
rm(param)
# Merge m, m2, m3
alln <- merge(npop,npop2,by="id")
#all2 <- merge(all,ncost,by="id")
# wtp
# get the cost coefficient
cost_coeff <- summary(idealn)$coefficients["ncost"]
alln$wtpn <- (alln$npop_coef/227.2321+alln$npop2_coef/139213.5*400)/(-cost_coeff/73.4816)*200
quantile(alln$wtpn, c(.01,.99))
hist(alln$wtp[alln$wtpn>-66 & alln$wtpn <62], main = "WTP" ,)
summary(alln$wtpn)

################################################################################
# Tables
################################################################################
install.packages('grep')
install.packages('pacman')
library(pacman)
library(tidyverse)
#p_load(tidyverse,modelsummary,broom,gmnl)
library(modelsummary)
library(broom)
library(gmnl)
vc <- vcov(ideal)
vcy <- vcov(idealy)
vcn <- vcov(idealn)
vci <- vcov(ideal_inter)

test = summary(ideal)

ideal_tidy <- data.frame(term=names(ideal$coefficients),
                         estimate=ideal$coefficients,
                         std.error=sqrt(diag(vc)),
                         p.value = test$CoefTable[,4])
test = summary(idealy)

idealy_tidy <- data.frame(term=names(idealy$coefficients),
                          estimate=idealy$coefficients,
                          std.error=sqrt(diag(vcy)),
                          p.value = test$CoefTable[,4])
test = summary(idealn)

idealn_tidy <- data.frame(term=names(idealn$coefficients),
                          estimate=idealn$coefficients,
                          std.error=sqrt(diag(vcn)),
                          p.value = test$CoefTable[,4])

gl <- data.frame(
  stat1 = "blah",
  stat2 = "blah blah")

mod1 <- list(tidy = ideal_tidy ,
             glance = gl)
mod2 <- list(tidy = idealy_tidy ,
             glance = gl)
mod3 <- list(tidy = idealn_tidy ,
             glance = gl)

class(mod1) <- "modelsummary_list"
class(mod2) <- "modelsummary_list"
class(mod3) <- "modelsummary_list"


# in the text file, replace - in front of standard deviations with ="( and end of each with )" 
modelsummary(list("mod1" = mod1, "mod2" = mod2, "mod3" = mod3), stars = c('*' = .1, '**' = .05, '***' = 0.01), output = "D:/OneDrive - Colostate/Wolf_CV/final/output/table.txt")


tidy.gmnl <- function(x, ...) {
  s <- summary(x, ...)
  ret <- data.frame(
    term = row.names(s$CoefTable),
    estimate = s$CoefTable[,1],
    conf.low = s$CoefTable[,2],
    conf.high = s$CoefTable[,3])
  ret
}
tidy.gmnl(ideal)
modelsummary(ideal, statistic = 'conf.int')
################################################################################
# plots 
################################################################################
library(ggplot2)
ggplot(ally, aes(x=wtp))+ geom_histogram(binwidth = 1)
# WTP for population of 200 among yes voters
yes_wtp=subset(ally,wtp>-61 & wtp <160)
my_binwidth <- 5    
p<-ggplot(yes_wtp, aes(x=wtp)) + 
  geom_density(color="black", fill="white") + 
  xlab("mWTP for 200 Wolves (Yes Voters)") + 
  ylab("Density")
p
ggsave("wtp_yes.png",p)


# plot asc from everyone
param <- effect.gmnl(ideal,par = "asc", effect = "ce")
asc_coeff <- param$mean
asc <- data.frame(asc_coeff)
a_plot <-ggplot(asc, aes(x=asc_coeff)) + 
  geom_density(color="black", fill="white") + 
  xlab("ASC") + 
  ylab("Density")
a_plot 
ggsave("asc.png",a_plot )

# Plot livestock wtp and compensation wtp from everyone
param <- effect.gmnl(ideal,par = "nlivestock", effect = "ce")
nlive_coeff <- param$mean
nlive <- data.frame(nlive_coeff)
nlive <- mutate(nlive, id = rownames(nlive))
rm(param)

# get the wtp
nlive$wtp <- (nlive$nlive_coeff/sdlivestock)/(-cost_coeff/73.4816)
live_plot <-ggplot(nlive, aes(x=wtp)) + 
  geom_density(color="black", fill="white") + 
  xlab("mWTP for Livestock Losses") + 
  ylab("Density")
live_plot 
ggsave("livestock.png",live_plot )


# Plot compensation wtp from everyone
param <- effect.gmnl(ideal,par = "comp1", effect = "ce")
comp1_coeff <- param$mean
comp1 <- data.frame(comp1_coeff)
comp1 <- mutate(comp1, id = rownames(comp1))
rm(param)

# get the wtp
comp1$wtp <- (comp1$comp1_coeff)/(-cost_coeff/73.4816)
comp1_plot <-ggplot(comp1, aes(x=wtp)) + 
  geom_density(color="black", fill="white") + 
  xlab("mWTP for 100% Compensation") + 
  ylab("Density")
comp1_plot 
ggsave("comp1.png",comp1_plot )


