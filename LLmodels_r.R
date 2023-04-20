# Reading the dataset in R
lake = read.csv('E:\\Lake Level\\Lake.csv', header = TRUE)
 # Installing/calling packages/libraries
 library(jtools)
 library(ISLR)
 library(MASS)

# Setting the working directory
 getwd()

setwd("E:/Lake Level/Output")

          
# Setting the random seedto make the results reproducible
 
set.seed(1)

# Correlation analysis
# Cor with p-values for the whole dataset

   #install.packages("Hmisc")
   library(Hmisc)
 
lake = (lake[,2:9])
 
rp_values = rcorr(as.matrix(lake))
rp_values

# Combination of correlation coef. r and correlation test rho

#install.packages("correlation")

library(correlation)

# Correlation Matrix Auto Method
  
rpvalues = correlation::correlation(lake, include_factors = TRUE, method = "auto")

rpvalues

# Creating a report of the correlation test
#install.packages("remotes")
#install.packages("report")
#remotes::install_github("easystats/report")
library(report)
# LL_R and LL_G
LLRG = cor.test(lake$LL_R, lake$LL_G)
LLRG
report(LLRG)

# LL_R and climate variables

# ET
LLRET = cor.test(lake$LL_R, lake$ET)
report(LLRET)

# SH
LLRSH = cor.test(lake$LL_R, lake$SH)
report(LLRSH)

# ST
LLRST = cor.test(lake$LL_R, lake$ST)
report(LLRST)

# AT
LLRAT = cor.test(lake$LL_R, lake$AT)
report(LLRAT)

# P
LLRP = cor.test(lake$LL_R, lake$P)
report(LLRP)

# SM
LLRSM = cor.test(lake$LL_R, lake$SM)
report(LLRSM)


#############LL_G and climate variables##################
 # LL_G and climate variables

# ET
LLGET = cor.test(lake$LL_G, lake$ET)
report(LLGET)

# SH
LLGSH = cor.test(lake$LL_G, lake$SH)
report(LLGSH)

# ST
LLGST = cor.test(lake$LL_G, lake$ST)
report(LLGST)

# AT
LLGAT = cor.test(lake$LL_G, lake$AT)
report(LLGAT)

# P
LLGP = cor.test(lake$LL_G, lake$P)
report(LLGP)

# SM
LLGSM = cor.test(lake$LL_G, lake$SM)
report(LLGSM)

# Time series plot
lake_t = (lake[,2:9])
fix(lake_t)
tslake = ts(lake_t, frequency = 12)
plot.ts(tslake)

# Exploring the dataset 

summary(lake) # Summary

#### Cross-correlation analysis##########

######CCF P and LL_R####################

ccf(lake$P, lake$LL_R)

# ccf values
ccfvalues = ccf(lake$P, lake$LL_R)

ccfvalues

#### Plotting the lagged r values
install.packages("astsa")
library('astsa')

lag2.plot(lake$P, lake$LL_R, 5, pch = 20, cex=3, lwl = 2)

##########CCF P and LL_G#################

ccf(lake$P, lake$LL_G)

#ccfvalues

ccfvalues = ccf(lake$P, lake$LL_G)

ccfvalues

ccfvalues = ccf(lake$P, lake$LL_G)

ccfvalues

## Plotting the lagged r values

lag2.plot(lake$P, lake$LL_G, 6, pch = 20, cex=3, lwl = 2)

#######AT and LL#############


######CCF AT and LL_R####################

ccf(lake$AT, lake$LL_R)

# ccf values
ccfvalues = ccf(lake$AT, lake$LL_R)

ccfvalues

#### Plotting the lagged r values

lag2.plot(lake$AT, lake$LL_R, 7, pch = 20, cex=3, lwl = 2)

##########CCF AT and LL_G#################

ccf(lake$AT, lake$LL_G)

#ccfvalues

ccfvalues = ccf(lake$AT, lake$LL_G)

ccfvalues


## Plotting the lagged r values

lag2.plot(lake$AT, lake$LL_G, 8, pch = 20, cex=3, lwl = 2)

##########################################


#######AT and LL#############


######CCF AT and LL_R####################

ccf(lake$AT, lake$LL_R)

# ccf values
ccfvalues = ccf(lake$AT, lake$LL_R)

ccfvalues

#### Plotting the lagged r values

lag2.plot(lake$AT, lake$LL_R, 7, pch = 20, cex=3, lwl = 2)

##########CCF AT and LL_G#################

ccf(lake$AT, lake$LL_G)

#ccfvalues

ccfvalues = ccf(lake$AT, lake$LL_G)

ccfvalues


## Plotting the lagged r values

lag2.plot(lake$AT, lake$LL_G)
            
            
#######SM and LL#############
          
          
          ######CCF SM and LL_R####################
          
ccf(lake$SM, lake$LL_R)
          
          # ccf values
ccfvalues = ccf(lake$SM, lake$LL_R)
          
ccfvalues
          
          #### Plotting the lagged r values
          
lag2.plot(lake$SM, lake$LL_R, 4, pch = 20, cex=3, lwl = 2)

          
          ##########CCF SM and LL_G#################
          
ccf(lake$SM, lake$LL_G)
          
          #ccfvalues
ccfvalues = ccf(lake$SM, lake$LL_G)
          
          ccfvalues
          
          
          ## Plotting the lagged r values
          
          lag2.plot(lake$SM, lake$LL_G, 5, pch = 20, cex=3, lwl = 2)
          
          

