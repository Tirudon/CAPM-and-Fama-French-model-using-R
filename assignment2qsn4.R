# Assignment 2 - Question 4 (by Sriniwas Mahapatro)

#==============================================
# Install the necessary packages
#==============================================

install.packages("installr")
library(installr)
updateR()


library(lmtest)
library(rmarkdown)
library(knitr)
library(readxl)
library(dplyr)
library(lme4)

# set working directory
setwd("C:/Users/51950005/Desktop/Term III/FFE/assignment")

# import data file containing rf,rm-rf, size, BM, and portfolio returns
NewData <- data.frame(read.csv(file = 'FFEQ4.csv', header = TRUE))

head (NewData)

summary(NewData)

names(NewData)

# calculating risk premium of portfolios, after adjusting for Rf

NewData$SMALL.LoBM <- NewData$SMALL.LoBM - NewData$Mkt.RF
NewData$ME1.BM2 <- NewData$ME1.BM2 - NewData$Mkt.RF
NewData$ME1.BM3 <- NewData$ME1.BM3 - NewData$Mkt.RF
NewData$ME1.BM4 <- NewData$ME1.BM4 - NewData$Mkt.RF
NewData$SMALL.HiBM <- NewData$SMALL.HiBM - NewData$Mkt.RF

NewData$ME2.BM1 <- NewData$ME2.BM1 - NewData$Mkt.RF
NewData$ME2.BM2 <- NewData$ME2.BM2 - NewData$Mkt.RF
NewData$ME2.BM3 <- NewData$ME2.BM3 - NewData$Mkt.RF
NewData$ME2.BM4 <- NewData$ME2.BM4 - NewData$Mkt.RF
NewData$ME2.BM5 <- NewData$ME2.BM5 - NewData$Mkt.RF

NewData$ME3.BM1 <- NewData$ME3.BM1 - NewData$Mkt.RF
NewData$ME3.BM2 <- NewData$ME3.BM2 - NewData$Mkt.RF
NewData$ME3.BM3 <- NewData$ME3.BM3 - NewData$Mkt.RF
NewData$ME3.BM4 <- NewData$ME3.BM4 - NewData$Mkt.RF
NewData$ME3.BM5 <- NewData$ME3.BM5 - NewData$Mkt.RF

NewData$ME4.BM1 <- NewData$ME4.BM1 - NewData$Mkt.RF
NewData$ME4.BM2 <- NewData$ME4.BM2 - NewData$Mkt.RF
NewData$ME4.BM3 <- NewData$ME4.BM3 - NewData$Mkt.RF
NewData$ME4.BM4 <- NewData$ME4.BM4 - NewData$Mkt.RF
NewData$ME4.BM5 <- NewData$ME4.BM5 - NewData$Mkt.RF

NewData$BIG.LoBM <- NewData$BIG.LoBM - NewData$Mkt.RF
NewData$ME5.BM2 <- NewData$ME5.BM2 - NewData$Mkt.RF
NewData$ME5.BM3 <- NewData$ME5.BM3 - NewData$Mkt.RF
NewData$ME5.BM4 <- NewData$ME5.BM4 - NewData$Mkt.RF
NewData$BIG.HiBM <- NewData$BIG.HiBM - NewData$Mkt.RF

head(NewData)

meanreturn <- colMeans(NewData)

meanreturn
# Lowest average Return is in ME2.BM1 (0.00847)
# Highest average Return is in portfolio SMALL.HiBM (0.03379)

#==================================================================================
# T test for difference of average returns for the best and the worst portfolio
#==================================================================================

res <- t.test(NewData$ME2.BM1 , NewData$SMALL.HiBM)

res

# T test results
# Welch Two Sample t-test
# data:  NewData$ME2.BM1 and NewData$SMALL.HiBM
# t = -2.8988, df = 48802, p-value = 0.003748
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
# -0.042421609 -0.008196226
# sample estimates:
#  mean of x   mean of y 
# 0.008477089 0.033786007 

# so the average return on both portfolios are significantly different

# =================================================
# CAPM regression for best and worst portfolio
# =================================================

CAPM.ME2.BM1 <- lm(ME2.BM1 ~ Mkt.RF, data=NewData)
summary(CAPM.ME2.BM1)
CAPM.ME2.BM1$coefficients[2]
# CAPM beta of ME2.BM1 is -0.021458. Alpha is 0.009

CAPM.SMALL.HiBM <- lm(SMALL.HiBM ~ Mkt.RF, data=NewData)
summary(CAPM.SMALL.HiBM)
# CAPM beta of SMALL.HiBM is -0.119421. Alpha is 0.037

# =================================================
# Fama French regression for best and worst portfolio
# =================================================

FF.ME2.BM1 <- lm(ME2.BM1 ~ Mkt.RF + SMB + HML, data=NewData)
summary(FF.ME2.BM1)
x = FF.ME2.BM1$coefficients
x["Mkt.RF"]
# FF beta of ME2.BM1 are 0.106987, 1.061596, and -0.299211. and Alpha is 0.00495

FF.SMALL.HiBM <- lm(SMALL.HiBM ~ Mkt.RF + SMB + HML, data=NewData)
summary(FF.SMALL.HiBM)
# FF beta of SMALL.HiBM is -0.078744, 1.085190, and 0.672288. And alpha is 0.0213

# Inference
# Although alphas are significnat and positive in both CAPM and FF models, but the alpha in FF mdoels is significantly lower


#=========================================================  
# CAPM alphas and betas for all portfolios
#=========================================================

# CAPM alphas for all portfolios
storage.capmalpha<- rep(NA,25)
storage.capmbeta<- rep(NA,25)

  for (i in 6:30){
    outcome=colnames(NewData)[i]
    exposure=colnames(NewData)[2]
    model <- lm(get(outcome) ~ get(exposure), data=NewData)
    storage.capmalpha[i] <- model$coefficient[1]
    storage.capmbeta[i] <- model$coefficient[2]
}

storage.capmalpha
storage.capmalpha <- storage.capmalpha[c(6:30)]

storage.capmalpha # vector of alphas of 25 portfolios

storage.capmbeta
storage.capmbeta <- storage.capmbeta[c(6:30)]
storage.capmbeta # vector of betas of 25 portfolios

# parameters of CAPM regressions for each portfolios
#-------------------------------------------------



CAPMparameters <- data.frame("portfolio"=portfolio_names, "alpha"=storage.capmalpha, "beta"=storage.capmbeta)
CAPMparameters <- CAPMparameters[c(6:30),]




CAPMparameters[1,2]

names(CAPMparameters)

library(xlsx)
write.csv(CAPMparameters, "CAPMparameters.csv")

# scatter plot of excess returns vs beta for CAPM
plot(storage.capmbeta,storage.capmalpha)

#==================================================================
# F-F alphas and betas for all portfolios
#=========================================================

storage.ffalpha<- rep(NA,25)
storage.ffbeta <- rep(NA,25)
storage.ffbetasize <- rep(NA,25)
storage.ffbetaBM <- rep(NA,25)

for (i in 6:30){
  outcome=colnames(NewData)[i]
  exposure=colnames(NewData)[2]
  exposuresize=colnames(NewData)[3]
  exposurebm=colnames(NewData)[4]
  model <- lm(get(outcome) ~ get(exposure) + get(exposuresize) + get(exposurebm), data=NewData)
  storage.ffalpha[i] <- model$coefficient[1]
  storage.ffbeta[i] <- model$coefficient[2]
  storage.ffbetasize[i] <- model$coefficient[3]
  storage.ffbetaBM[i] <- model$coefficient[4]
}

storage.ffalpha <- storage.ffalpha[c(6:30)]
storage.ffbeta <- storage.ffbeta[c(6:30)]
storage.ffbetasize <- storage.ffbetasize[c(6:30)]
storage.ffbetaBM <- storage.ffbetaBM[c(6:30)]


# parameters of FF regressions for each portfolios

xyz <- colnames(NewData)
xyz <- xyz[c(6:30)]
FFparameters <- data.frame("portfolio"=xyz, "alpha"=storage.ffalpha, "mktfactorbeta"=storage.ffbeta, "sizefactorbeta"=storage.ffbetasize, "valuefactorbeta"=storage.ffbetaBM)
FFparameters

write.csv(FFparameters,"FFparameters.csv")
# scatter plot of excess returns vs beta for CAPM
plot(storage.ffbeta,storage.ffalpha)

#Observation:: The excess returns are higher in CAPM as compared to Fama French model

#==============================================================
#Average Predicted returns using FF model
#==============================================================

names(FFparameters)

FFparameters[1] # reading column 2
FFparameters$alpha # reading column 2
FFparameters[6,] # reading 6th row
FFparameters[[2]] # second column
FFparameters[,2] # second column

# FFparameters is data frame with alpha, beta for 25 portfolios
#--------------------------------------------------------------

factor_loadings <- NewData[1:4]
factor_loadings[1,2]

return <- data.frame(NewData)

# factor_loadings is data frame with factors datewise for 24683 days

#======================================================================

NewData$SMALL.LoBM_r <- 0

NewData$SMALL.LoBM_r

NewData$SMALL.LoBM_r[1] <- FFparameters$alpha[1] + FFparameters$mktfactorbeta[1]*NewData$Mkt.RF[1] + FFparameters$sizefactorbeta[1]*NewData$SMB[1] + FFparameters$valuefactorbeta[1]*NewData$HML[1]

for (j in 1:1){
  x <- FFparameters$portfolio[j]
  for (i in 1:24683){
    NewData$x[i] <- FFparameters$alpha[1] + FFparameters$mktfactorbeta[1]*NewData$Mkt.RF[i] + FFparameters$sizefactorbeta[1]*NewData$SMB[i] + FFparameters$valuefactorbeta[1]*NewData$HML[i]
  
}
#----------------------------------------------------

expreturn <- data.frame(matrix(,nrow=34683,ncol=0))
storage.ptf <- data.frame(matrix(,nrow=34683,ncol=0))

storage.ptf<- rep(NA,24683)
for (i in 1:24683){
  storage.ptf[[i]] = ptf1[2] + ptf1[3]*factor_loadings[i,2] + ptf1[4]*factor_loadings[i,3] + ptf1[5]*factor_loadings[i,4]
  
}

storage.ptf[1,1]

#-----------------
iterations = 34683
variables = 1
output <- matrix(ncol=variables, nrow=iterations)
for (j in 1:variables){
for (i in 1:iterations){
  output[i,] <- ptf1[2] + ptf1[3]*factor_loadings[i,2] + ptf1[4]*factor_loadings[i,3] + ptf1[5]*factor_loadings[i,4]
}}

output[1,]

output <- data.frame(output)



#---------------------------
storage.ptf(,1)
storage.ptf <- type.convert(storage.ptf,vector)
NewData1 <- cbind(NewData,storage.ptf)

class(storage.ptf)
summary(storage.ptf)

expreturn <- data.frame(matrix(,nrow=34683,ncol=0))

expreturn <- cbind(expreturn, storage.ptf)

portfolio_paremeters[[1]]
portfolio_parameters$X6
portfolio_parameters[2,2]

portfolios <- as.vector(portfolio_names)
portfolios[5]
factor_loadings <- NewData[1:4]

periods = nrow(factor_loadings)
periods #24683 incluidng header

for (i in 1:25){
  ptf = portfolio_parameters
}

pred_returns <- data.frame(parameters)

# refer spreadhsheet for average predicted returns

portfolio_names <- as.vector(FFparameters[1])
portfolio_alpha <- as.vector(FFparameters[2])
portfolio_mktbeta <- as.vector(FFparameters[3])
portfolio_sizebeta <- as.vector(FFparameters[4])
portfolio_valuebeta <- as.vector(FFparameters[5])