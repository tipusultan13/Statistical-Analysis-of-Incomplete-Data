####################################################
## Statistical Analysis of Incomplete Data
## Group: 07
## Topic: 03.a
## Names: Tipu Sultan, Tahomina Akter Ety, Lara Krell
#####################################################


###################
## Data
###################


#### Real data set ####

library(readxl)
top_subscribers <- read.csv("topSubscribed.csv")
# 1000 observation and 3 variables
data_real<-top_subscribers[1:1000,3:5]
# Removing the commas in the data set
views <- as.numeric(gsub(",", "", data_real$Video.Views))
counts <- as.numeric(gsub(",", "", data_real$Video.Count))
subscribers <- as.numeric(gsub(",", "", data_real$Subscribers))
data_real <- data.frame(subscribers,views,counts)
# As the data values are gigantic, normalization is required to ease the calculation
# There are some zeros in the data, so added a small value 1. 
data_real <- log(data_real+1)
data_real <- as.data.frame(scale(data_real))

#### Simulated dataset ####

set.seed(2500)
# 1000 Observations:
n1 <- 1000
# 3 variables
x1 <- rnorm(n1, 5, 2)
# x2 depending on the values of x1
x2 <- 4 - 0.5 * x1 + rnorm(n1, 0, 4)
# x3 depending on the values of x1 und x2
x3 <- 3 + 0.5 * x1 + 0.3 * x2 + rnorm(n1, 0, 2)
data_simulated <- data.frame(x1, x2, x3)
# Normalize the data
data_simulated <- as.data.frame(scale(data_simulated))


#####################
## MCAR Missing Data
#####################


#### Sample, Real Data ####

p_mis <- 0.30
data_real_mcar <- data_real
# Indicator for which values are set to missing exactly p_mis % missing values
mis_real_mcar <- sample(1:1000, p_mis * 1000, replace = FALSE)    
data_real_mcar[mis_real_mcar, 3] <- NA
summary(data_real_mcar)

#### Sample, Simulated Data ####

data_simulated_mcar <- data_simulated
# Indicator for which values are set to missing exactly p_mis % missing values
mis_simulated_mcar <- sample(1:1000, p_mis * 1000, replace = FALSE)    
data_simulated_mcar[mis_simulated_mcar, 3] <- NA
summary(data_simulated_mcar)


#####################
## MAR Missing Data
#####################


#### Deterministic, Real Data ####

data_real_mar_d <- data_real
q_views <- qnorm(.3, 16, 7)
# This creates a logical vector which indicates when the "true value" of subscribers is below the theoretical quantile.
mis_mar_d <- views < q_views 
data_real_mar_d$counts[mis_mar_d] <- NA
summary(data_real_mar_d)

#### Deterministic, Simulated Data ####

data_simulated_mar_d <- data_simulated
q_x2 <- qnorm(.3, 1.5, 4)
# This creates a logical vector which indicates when the "true value" of subscribers is below the theoretical quantile.
mis_mar_d <- x2 < q_x2 
data_simulated_mar_d$x3[mis_mar_d] <- NA
summary(data_simulated_mar_d)


#####################
## MNAR Missing Data
#####################


#### Probabilistic, Linear Regression model, Real data ####

data_real_mnar_p <- data_real
# model missingness by logistic regression (probit):
# the missing of a value now also depends on counts itself
z_miss_mnar_p <-0.5 + 1 * subscribers - 0.7 * views - 5 * counts + rnorm(1000, 0, 3)
mis_mnar_p <- z_miss_mnar_p < quantile(z_miss_mnar_p, p_mis)
data_real_mnar_p$counts[mis_mnar_p] <- NA
summary(data_real_mnar_p)

#### Probabilistic, Linear Regression model, Real data ####

data_simulated_mnar_p <- data_simulated
# model missingness by logistic regression (probit):
# the missing of a value now also depends on counts itself
z_miss_mnar_p <-0.5 + 1 * x1 - 0.7 * x2 - 5 * x3 + rnorm(1000, 0, 3)
mis_mnar_p <- z_miss_mnar_p < quantile(z_miss_mnar_p, p_mis)
data_simulated_mnar_p$x3[mis_mnar_p] <- NA
summary(data_simulated_mnar_p)


#############################
## Visualize the Missing Data
#############################


library(VIM)
library("dplyr")
library("ggplot2")
library("gridExtra")
par(mfrow=c(2,3)) 

matrixplot(data_real_mcar, sortby = c('counts'), main = "MCAR on Real Data")
matrixplot(data_simulated_mcar, sortby = c('x3'), main = "MCAR on Simulated Data")

matrixplot(data_real_mar_d, sortby = c('counts'), main = "MAR on Real Data")
matrixplot(data_simulated_mar_d, sortby = c('x3'), main = "MAR on simulated Data")

matrixplot(data_real_mnar_p, sortby = c('counts'), main = "MNAR on Real Data")
matrixplot(data_simulated_mnar_p, sortby = c('x3'), main = "MNAR on simulated Data")

# Some other plot that can be mentioned
par(mfrow=c(1,1)) 
aggr(data_real_mcar,numbers = TRUE, prop = c(TRUE, FALSE)) # similar for all the data. 
pbox(data_real_mcar[, c('views', "counts")]) # similar for all the data. 


###################################################
## Impute the Data Through 'missForest' and 'mice' 
###################################################


#### missForest ####


library(missForest)

data_real_mcar_MF_imp <- missForest(data_real_mcar, maxiter = 1000)
data_real_mcar_MF_imp <- data.frame(data_real_mcar_MF_imp['ximp'])

data_simulated_mcar_MF_imp <- missForest(data_simulated_mcar, maxiter = 1000)
data_simulated_mcar_MF_imp <- data.frame(data_simulated_mcar_MF_imp['ximp'])

data_real_mar_d_MF_imp <- missForest(data_real_mar_d, maxiter = 1000)
data_real_mar_d_MF_imp <- data.frame(data_real_mar_d_MF_imp['ximp'])

data_simulated_mar_d_MF_imp <- missForest(data_simulated_mar_d, maxiter = 1000)
data_simulated_mar_d_MF_imp <- data.frame(data_simulated_mar_d_MF_imp['ximp'])

data_real_mnar_p_MF_imp <- missForest(data_real_mnar_p, maxiter = 1000)
data_real_mnar_p_MF_imp <- data.frame(data_real_mnar_p_MF_imp['ximp'])

data_simulated_mnar_p_MF_imp <- missForest(data_simulated_mnar_p, maxiter = 1000)
data_simulated_mnar_p_MF_imp <- data.frame(data_simulated_mnar_p_MF_imp['ximp'])


#### mice(FCS) ####


library(mice)

# choose to impute 3 dataset.
data_real_mcar_mice_imp <- mice(data_real_mcar, m = 3, maxit = 1000, method = 'pmm') 
summary(data_real_mcar)
data_real_mcar_mice_imp$imp$counts
# took the 3rd data set because it is closer to the mean of the prior data.
data_real_mcar_mice_imp <- complete(data_real_mcar_mice_imp,3)
data_real_mcar_mice_imp

# choose to impute 3 dataset.
data_simulated_mcar_mice_imp <- mice(data_simulated_mcar, m = 3, maxit = 1000, method = 'pmm') 
summary(data_simulated_mcar)
data_simulated_mcar_mice_imp$imp$x3
# took the 3rd data set because it is closer to the mean of the prior data.
data_simulated_mcar_mice_imp <- complete(data_simulated_mcar_mice_imp,3)
data_simulated_mcar_mice_imp

# choose to impute 3 dataset.
data_real_mar_d_mice_imp <- mice(data_real_mar_d, m = 3, maxit = 1000, method = 'pmm') 
summary(data_real_mar_d)
data_real_mar_d_mice_imp$imp$counts
# took the 1st data set because it is closer to the mean of the prior data.
data_real_mar_d_mice_imp <- complete(data_real_mar_d_mice_imp,1)
data_real_mar_d_mice_imp

# choose to impute 3 dataset.
data_simulated_mar_d_mice_imp <- mice(data_simulated_mar_d, m = 3, maxit = 1000, method = 'pmm') 
summary(data_simulated_mar_d)
data_simulated_mar_d_mice_imp$imp$x3
# took the 3rd data set because it is closer to the mean of the prior data.
data_simulated_mar_d_mice_imp <- complete(data_simulated_mar_d_mice_imp,3)
data_simulated_mar_d_mice_imp

# choose to impute 3 dataset.
data_real_mnar_p_mice_imp <- mice(data_real_mnar_p, m = 3, maxit = 1000, method = 'pmm') 
summary(data_real_mnar_p)
data_real_mnar_p_mice_imp$imp$counts
# took the 2nd data set because it is closer to the mean of the prior data.
data_real_mnar_p_mice_imp <- complete(data_real_mnar_p_mice_imp,2)
data_real_mnar_p_mice_imp

# choose to impute 3 dataset.
data_simulated_mnar_p_mice_imp <- mice(data_simulated_mnar_p, m = 3, maxit = 1000, method = 'pmm')
summary(data_simulated_mnar_p)
data_simulated_mnar_p_mice_imp$imp$x3
# took the 3rd data set because it is closer to the mean of the prior data.
data_simulated_mnar_p_mice_imp <- complete(data_simulated_mnar_p_mice_imp,3)
data_simulated_mnar_p_mice_imp


#############################################################
## Finding the Bias, RMSE, and Coverage of the Imputed Data
#############################################################


############
## Bias
############


library(Metrics)

bias(data_real$counts, data_real_mcar_mice_imp$counts)
bias(data_real$counts, data_real_mcar_MF_imp$ximp.counts)

bias(data_simulated$x3, data_simulated_mcar_mice_imp$x3)
bias(data_simulated$x3, data_simulated_mcar_MF_imp$ximp.x3)

bias(data_real$counts, data_real_mar_d_mice_imp$counts)
bias(data_real$counts, data_real_mar_d_MF_imp$ximp.counts)

bias(data_simulated$x3, data_simulated_mar_d_mice_imp$x3)
bias(data_simulated$x3, data_simulated_mar_d_MF_imp$ximp.x3)

bias(data_real$counts, data_real_mnar_p_mice_imp$counts)
bias(data_real$counts, data_real_mnar_p_MF_imp$ximp.counts)

bias(data_simulated$x3, data_simulated_mnar_p_mice_imp$x3)
bias(data_simulated$x3, data_simulated_mnar_p_MF_imp$ximp.x3)


############
## RMSE
############


rmse(data_real$counts, data_real_mcar_mice_imp$counts)
rmse(data_real$counts, data_real_mcar_MF_imp$ximp.counts)

rmse(data_simulated$x3, data_simulated_mcar_mice_imp$x3)
rmse(data_simulated$x3, data_simulated_mcar_MF_imp$ximp.x3)

rmse(data_real$counts, data_real_mar_d_mice_imp$counts)
rmse(data_real$counts, data_real_mar_d_MF_imp$ximp.counts)

rmse(data_simulated$x3, data_simulated_mar_d_mice_imp$x3)
rmse(data_simulated$x3, data_simulated_mar_d_MF_imp$ximp.x3)

rmse(data_real$counts, data_real_mnar_p_mice_imp$counts)
rmse(data_real$counts, data_real_mnar_p_MF_imp$ximp.counts)

rmse(data_simulated$x3, data_simulated_mnar_p_mice_imp$x3)
rmse(data_simulated$x3, data_simulated_mnar_p_MF_imp$ximp.x3)


############
## Coverage
############


library(Rmisc)
library(dplyr)

# MCAR Real Data for mice

ci_1 <- CI(data_real_mcar_mice_imp$counts, ci=0.99)
ci_1
x1 <-sum(between(data_real_mcar_mice_imp$counts,-0.07428541,09452901))
mcar_real_coverage_mice <- x1/length(data_real_mcar_mice_imp$counts)
mcar_real_coverage_mice

# MCAR Real Data for missForest

ci_2 <- CI(data_real_mcar_MF_imp$ximp.counts, ci=0.99)
ci_2
x2 <-sum(between(data_real_mcar_MF_imp$ximp.counts,-0.072943708,075881749))
mcar_real_coverage_MF <- x2/length(data_real_mcar_MF_imp$ximp.counts)
mcar_real_coverage_MF

# MCAR Simulated Data for mice

ci_3 <- CI(data_simulated_mcar_mice_imp$x3, ci=0.99)
ci_3
x3 <-sum(between(data_simulated_mcar_mice_imp$x3,-0.087547781, 0.077118653))
mcar_simulated_coverage_mice <- x3/length(data_simulated_mcar_mice_imp$x3)
mcar_simulated_coverage_mice

# MCAR simulated Data for missForest

ci_4 <- CI(data_simulated_mcar_MF_imp$ximp.x3, ci=0.99)
ci_4
x4 <-sum(between(data_simulated_mcar_MF_imp$ximp.x3,-0.064420793,0.080348386))
mcar_simulated_coverage_MF <- x4/length(data_simulated_mcar_MF_imp$ximp.x3)
mcar_simulated_coverage_MF

# MAR Real Data for mice

ci_5 <- CI(data_real_mar_d_mice_imp$counts, ci=0.99)
ci_5
x5 <-sum(between(data_real_mar_d_mice_imp$counts,-0.073487711, 0.086542124))
mar_real_coverage_mice <- x5/length(data_real_mar_d_mice_imp$counts)
mar_real_coverage_mice

# MAR Real Data for missForest

ci_6 <- CI(data_real_mar_d_MF_imp$ximp.counts, ci=0.99)
ci_6
x6 <-sum(between(data_real_mar_d_MF_imp$ximp.counts,-0.06128338, 0.09470773))
mar_real_coverage_MF <- x6/length(data_real_mar_d_MF_imp$ximp.counts)
mar_real_coverage_MF

# MAR Simulated Data for mice

ci_7 <- CI(data_simulated_mar_d_mice_imp$x3, ci=0.99)
ci_7
x7 <-sum(between(data_simulated_mar_d_mice_imp$x3,-0.04262043, 0.11449408))
mar_simulated_coverage_mice <- x7/length(data_simulated_mar_d_mice_imp$x3)
mar_simulated_coverage_mice

# MAR Simulated Data for missForest

ci_8 <- CI(data_simulated_mar_d_MF_imp$ximp.x3, ci=0.99)
ci_8
x8 <-sum(between(data_simulated_mar_d_MF_imp$ximp.x3,-0.15900142, -0.01085564))
mar_simulated_coverage_MF <- x8/length(data_simulated_mar_d_MF_imp$ximp.x3)
mar_simulated_coverage_MF

# MNAR Real Data for mice

ci_9 <- CI(data_real_mnar_p_mice_imp$counts, ci=0.99)
ci_9
x9 <-sum(between(data_real_mnar_p_mice_imp$counts,-0.2330143, -0.0833662))
mnar_real_coverage_mice <- x9/length(data_real_mnar_p_mice_imp$counts)
mnar_real_coverage_mice

# MNAR Real Data for missForest

ci_10 <- CI(data_real_mnar_p_MF_imp$ximp.counts, ci=0.99)
ci_10
x10 <-sum(between(data_real_mnar_p_MF_imp$ximp.counts,-0.20403670, -0.07215378))
mnar_real_coverage_MF <- x10/length(data_real_mnar_p_MF_imp$ximp.counts)
mnar_real_coverage_MF

# MNAR Simulated Data for mice

ci_11 <- CI(data_simulated_mnar_p_mice_imp$x3, ci=0.99)
ci_11
x11 <-sum(between(data_simulated_mnar_p_mice_imp$x3,-0.4198535, -0.2992539))
mnar_simulated_coverage_mice <- x11/length(data_simulated_mnar_p_mice_imp$x3)
mnar_simulated_coverage_mice

# MNAR Simulated Data for missForest

ci_12 <- CI(data_simulated_mnar_p_MF_imp$ximp.x3, ci=0.99)
ci_12
x12 <-sum(between(data_simulated_mnar_p_MF_imp$ximp.x3,-0.4616484, -0.3528773))
mnar_simulated_coverage_MF <- x12/length(data_simulated_mnar_p_MF_imp$ximp.x3)
mnar_simulated_coverage_MF











