#Installing the needed libraries

install.packages("xlsx")
library("xlsx")

install.packages("psych")
library(psych)

install.packages("car")
library(car)

install.packages("MASS")
library(MASS)


#1.1. Read data + taking some overview of the data
sandphedge <- read.xlsx("SANDPHEDGE.XLSX", 1)
summary(sandphedge)

capm<-read.xlsx("CAPM.xlsx",1)
summary(capm)

#1.2. changing the price data to logarithmic returns
spot_ret<-100*(diff(log(sandphedge$Spot)))
fut_ret<-100*(diff(log(sandphedge$Futures)))

#1.3.Sample statistics

sample_spot<-describe(spot_ret)
sample_fut<-describe(fut_ret)

sample_spot
sample_fut

#interesting enough kurtosis of spot returns distribution is almost double
#to future returns

#tried to experiement with reporting functions, poor results
#write.table(sample_spot, file = "sumstats.txt", sep = ",", quote = FALSE, row.names = F)

#1.4.OLS regression 

model_1 <- lm(spot_ret~fut_ret) 
summary(model_1)
anova(model_1)
#future returns seems to have poor explanatory power to spot returns as its 
#sum Sq (from anova) and r^2 is low. The probability to fut_ret coefficient to
#be 0 is 35,8%
resid<-(model_1$residuals) #saving residuals to resid
head(resid)
cor(resid,spot_ret) #comparing the variance of spot_returns and ols residuals

#correlation is very close to 1. It has something to do with the models poor
#performance (model is unable to explain spot returns), as correlation measures
#linear co-movements of two variables.

#1.6 Regression for levels (using price data)

spot<-sandphedge$Spot
future<-(sandphedge$Futures)
model_2<-lm(spot~future)
summary(model_2)
anova(model_2)
#hinnoilla suoritetun OLS regression tuloksena saadaan, että futurihinnoilla on
selitysvoimaa spottihintoihin (tilastollisesti merkittävä) ja selittäjänä
se pystyy selittämään 96% spottihintojen vaihtelusta (r^2 = 0,96)


#1.7 Wald test for H_0: beta = 1

linearHypothesis(model_1, hypothesis.matrix = c(0,1), rhs=1,
test=c("F", "Chisq"), vcov.=NULL, white.adjust=FALSE)
#This is the first time I am using or even hearing about wald test. Wald
#test is for testing hypothesis jointly on multiple parameters, seems to 
#work fine and it's usefull. 


#2.1 Read data, already done previously

head(capm)
#2.2 Transformation to log returns and calculation of Excess returns
capm$E_MKT<-c(NA,diff(log(capm$SANDP)))-capm$USTB3M/12
capm$E_FORD<-c(NA,diff(log(capm$FORD)))-capm$USTB3M/12
summary(capm$E_FORD)
summary(capm$E_MKT)

#From the summary statistics I anticipiate that beta is very close to 1 (return
#means are so close to each other

#2.3 examine visually
jpeg('visual.jpg')
par(mfrow=c(1,1))
plot(capm$E_FORD,capm$E_MKT,xlab="E_FORD",ylab="E_MKT",col=3)
dev.off()

#2.4 CAPM regression, ford returns as dependent variable

model_4 <- lm(capm$E_FORD~capm$E_MKT)
summary(model_4)
#Beta is actually a bit more than one (1,17) and R^2 is relatively high 64%
#meaning that market returns are able to explain 64% of ford's return variation

#wald for H_0: beta = 1
linearHypothesis(model_4, hypothesis.matrix = c(0,1), rhs=1,
test=c("F", "Chisq"), vcov.=NULL, white.adjust=FALSE)
confint(model_4)
#wald test shows that probability to beta to be 1 is 2%. As and bonus
#I ran confint which shows that beta coefficient lies between 1.02-1.31
#with 95% probability


#3A F-test and compare it to coefficient's t-ratio, joint test
linearHypothesis(model_4,c("(Intercept) = 1", "capm$E_MKT = 1"))
#joint test shows that it's highly improbable that intercept and beta is jointly 1 

#3B #reading macro data and overviewing it
macro <-read.xlsx("MACRO.xlsx",1)
head(macro)

#3B.1 transforming the variables
macro$dspread <- c(NA,diff(macro$BAA.AAA.SPREAD))
macro$dcredit <-c(NA,diff(macro$CONSUMER.CREDIT))
macro$dprod <-c(NA,diff(macro$Industrial.production))
macro$rMKT <- c(100*log(macro$SANDP))
macro$rmicrosoft <- c(100*log(macro$Microsoft))
macro$dmoney<-c(NA,diff(macro$M1MONEY.SUPPLY))
macro$inflation <- c(100*log(macro$CPI))
macro$term <- c(macro$USTB10Y-macro$USTB3M)

macro$dinflation <-c(NA,diff(macro$inflation))
macro$mustb3m <-c(macro$USTB3M/12)
macro$rterm <- c(NA,diff(macro$term))
#Excess returns for microsoft
macro$er_microsoft <- c(macro$rmicrosoft-macro$mustb3m)
#excess for markets
macro$er_mkt <-c(macro$rMKT-macro$mustb3m)

#Trying to clean data from NA values

macro1<-na.omit(macro)
summary(macro1) #took a while but got it!

#3B.2 Multivariable regression
model_5 <- lm(macro$er_microsoft~macro$er_mkt+macro$dprod+
	macro$dcredit+macro$dinflation+macro$dmoney+macro$dspread+macro$rterm)
summary(model_5)
anova(model_5)
#from the regression summary it seems that dinflation, dspread and rterms are poor
#explanatory variables for microsoft's excess returns and this can be seen more clearly
#from anova table

#3B.3 Test if DPROD, DCREDIT AND DSPREAD are jointly zero
linearHypothesis(model_5,c("macro$dprod = 0","macro$dcredit=0","macro$dspread=0"))
confint(model_5)
#the wald test shows that jointly coefficients are most likely not 0. Confint
#tells the same story. Only dcredit's 95% confidence interval includes the zero

#3C.1 Backwards stepwise procedure (using MASS -package)
step <- stepAIC(model_5, direction="backward") #backwards stepwise
step$anova
#Anova table sums the stepwise procedure results. The procedure concludes that
#most suitable variables to explain microsoft's excess returs are: er_mkt, dprod
#dcredit and dmoney. I am not sure what was the criteria stepAIC used, but
#the result is that it dropped the variables with clearly lowest sum of squares.

step2 <- stepAIC(model_5, direction="both") #"full" stepwise
step2$anova

#the full stepwise prosedure ends up in the same conclusion than backward prosedure.
