# <- for assignment related or for explaining what I am doing
## <- for Learning diary and notes

#loading the data
library("xlsx")



# fasdfafasdasd
exrate.df <- read.xlsx("exrate.xlsx",1)
var <- read.xlsx("var.xlsx",1)
#modifying the data 
exlnrate.df <- (exrate.df ["lnex"])
# deleting lnex as it's not needed (did this befo)
exrate.df$lnex <- NULL 

#Transforming data.frame to time series for future
exrate.ts <- ts (exrate.df, start=c(1), end=c(2355), frequency=1) # Yearly Data
exlnrate.ts <- ts (exlnrate.df, start=c(1), end=c(2355), frequency=1) # Yearly Data
head(exlnrate.ts)
#3.1 Graph a scatterplot LEX vs. LEX(-1). Explain


#Creating lagged term
install.packages("DataCombine")
library(DataCombine)
exlnrate.df <- slide(exlnrate.df, "lnex", NewVar = "exlnLag1", slideBy = -1)
#3.1plotting
library(ggplot2)
qplot(exlnrate.df$lnex, exlnrate.df$exlnLag1)
##The plot shows that lnex and lnex[-1] are strongly positively correlated
## not sure what the idea of this was. Of course the lagged values are inline with present values when plotted
##and correlation is nearly 1

#3.2 Compute correlogram of dollar/euro exchange rate. Explain.
acfRes <- acf(exrate.ex) # autocorrelation
##correlogram shows if lagged observations have influence on current state. In the plot
##if the autocorrelation value crosses blue dash line, it means specific lag is significally
##correlated with current series. In this case, every lag is correlated with current series.

#3.3 Do unit root tests of the dollar/euro exchange rate. Explain.
library(tseries)
#turning exrate.df to vector including only exchange rates
exrate.ex <- as.vector(exrate.df [,1])
class(exrate.ex)

adf.test(exrate.ex) # p-value < 0.05 indicates the TS is stationary
##Our Exchange rate is not stationary

#3.4  Test whether dollar/euro exchange rate is trend stationary? Explain.
plot(y=exrate.df$ex, x=exrate.df$time, ylab= "dollar/euro exchange rate",
     xlab= "time")
kpss.test(exrate.ex,null = c("Trend"), lshort = TRUE) #Null hypothesis is that x is level or trend stationary

          ##the exchange rate is not level or trend stationary. This can be also seen
##from the scatterplot where exchange rate form a upward sloping scatterplot.
##From mere plot we can see that exchange rate has an positive trend

#3.5 What your results tell you about exchange rates behavior? Explain.

##Summary: From our lagged plot we can see that lagged values of logarithmic exchange rate is highly correlated
##with present value. This finding is confirmed with autocorrelation function which shows that
##every of tested lagged terms affects on current values and the effect is persistent even for highly lagged values
#.Now we can doubt that exchange rates aren't stationary. This is tested with Augmented Dickey-Fuller test
## in which the null is that series is not stationary. The ADF -test comfirms that we are dealing
##with non-stationary series, and next we can test what kind of non-stationary we are dealing with.
##this is done with kpss.test (Kwiatkowski-Phillips-Schmidt-Shin). The result shows that our data
##exhibits trend non-stationarity.

#yrit�n huvikseen muokata datan station��riksi

exrate.exln <- 100*(diff(log(exrate.df$ex)))
library(forecast)
plot(exrate.exln, type="l")

adf.test(exrate.exln) # p-value < 0.05 indicates the TS is stationary
##Now adf test shows our series is stationary
kpss.test(exrate.exln, null = c("Trend"), lshort = TRUE)
##and kpss indicates that the data is trend stationary (but only with p-value of 0.06), let's
##see if we can improve that by fitting a linear model to it.
n <- length(exrate.exln)
trmodel <- lm(exrate.exln[-n] ~ exrate.exln[-1] )
plot (resid(trmodel), type="l")
fitted.exln <- resid(trmodel)

kpss.test(fitted.exln, null = c("Trend"), lshort = TRUE)
##the p-value actually decreased because of linear model. How about adding lagged values?

trmodel1 <- lm(exrate.exln[-n] ~ exrate.exln[-1] +exrate.exln[-2]+exrate.exln[-3]  )
fitted.exln1 <- resid(trmodel1)
plot(fitted.exln1, type="l")
##doesn't actually affect on data. The residual plot stays the same.

#4.1 i) Test each of the three time series for stationarity, explaining the test(s) you use.
##tests: adf, pdf, kpss

#first transforming var to time series

var.ts <- ts (var, start=c(1960), frequency=4) # Quarterly Data

##Unloading columns to separate time series
fed.ts <- var.ts[, 2] 
inflation.ts <- var.ts[,3]
unrate.ts <- var.ts[,5]

#tests for stationarity
plot(fed.ts) #time series of federal funds rate
plot(inflation.ts) #inflation
plot(unrate.ts) #unemployment rate

acf(fed.ts) #Autocorrelation functions shows that lagged values affect on present values
acf(inflation.ts)
acf(unrate.ts)

adf.test(fed.ts) #Null: series has unit root -> reject the null (is stationary)
adf.test(inflation.ts) #Null: series has unit root -> result in line with null
adf.test(unrate.ts) #Null: series has unit root -> result in line with null
adf.test?
kpss.test(fed.ts ,null = c("Trend"), lshort = TRUE) #Null hypothesis is that x is level or trend stationary (rejected)
kpss.test(inflation.ts ,null = c("Trend"), lshort = TRUE) #Null hypothesis is that x is level or trend stationary (rejected)
kpss.test(unrate.ts ,null = c("Trend"), lshort = TRUE) #Null hypothesis is that x is level or trend stationary (rejected)

Box.test(fed.ts, type="Ljung-Box") #computes if time series is independent (using the lag value of 1)
Box.test(inflation.ts, type="Ljung-Box")
Box.test(unrate.ts, type="Ljung-Box")
##1.First I plot the data. There we get a first look at data's characteristics (seasonolity, trends, stationarity)
##2.After plotting, I carry out Augmented Dickey-Fuller test which tests if unit root is 1 ( tests AR(1) = 1, if that's is the case
##the data is unstationary as lagged values also dictate the process)
##3.Next I test for trend stationarity with Kwiatkowski-Phillips-Schmidt-Shin -test
##4. Lastly I test if time series are independent with Ljung-box -test (a bit same as adf.test)

#4.2 After testing stationarity, develop suitable ARMA model for each time series.
library(forecast)
arima.fed <- auto.arima(fed.ts, max.d=0 ) #ARMA (2,3)
summary(arima.fed)
plot(resid(arima.fed))
arima.inf <- auto.arima(inflation.ts, max.d=0, seasonal = FALSE)
summary(arima.inf) #ARMA (1,1)
inf.arima <- resid(arima.inf)
plot(inf.arima)
arima.un <- auto.arima(unrate.ts, max.d=0, seasonal = FALSE)
summary(arima.un) #ARMA (2,0)
plot(resid(arima.un))
kpss.test(resid(arima.un))
##found a automated function to search best fitting ARIMA 
##(arma if d is set to 0 and seasonal is disabled)
##it uses stepwise procedure to find best fit for its p,q parameters
#4.3 Estimate pairwise VAR models, that is, VAR between inflation and unemployment rate,
#between inflation and federal funds rate and between unemployment rate and federal
#funds rates. 
#VAR between inflation and unemployment rate  

##In order to use VAR -model parameters needs to be stationary and from the 4.2 we
##find that inflation and unemployment -parameters are not

fed.ts <- var.ts[, 2] 
inflation.ts <- var.ts[,3]
unrate.ts <- var.ts[,5]

inflation.ts <- (diff(log(inflation.ts)))
arima.inf <- auto.arima(inflation.ts, max.d=0, seasonal = FALSE)
summary(arima.inf) #ARMA (0,1)
inf.arima.resid <- resid(arima.inf)
plot(inf.arima.resid)

plot(inflation.ts) ##seems good
summary(inflation.ts) ##Now the problem is NA -values resulted from log diff. For assignment I will just
##omit those, but in different settings some smoothing might be more proper way.
inflation.ts <- na.remove(inf.arima.resid)
plot(inflation.ts)


adf.test(inflation.ts) #adf.test shows that log-diff of inflation is stationary now
pp.test(inflation.ts) #phillips-perron also indicates that that series is stationary
kpss.test(inflation.ts, null =c("Trend"), lshort=TRUE) #kpss rejects the null (data is not trend stationary)
##This results must come from autoregressive process being very close to 1 (but enough below to adf reject the null)
plot(decompose(inflation.ts))

##is higher order of differencing needed?
ndiffs(inflation.ts) ##seems not.
nsdiffs(inflation.ts)##neither is seasonal differencing
##Let's try with ARMA (1,1) residuals.
kpss.test(inf.arima, null=c("Trend")) ##Nope, still non stationary
##I Give up. Can't get it stationary.

##How about unemployment rates?
plot(unrate.ts)
detrend.unrate <- diff(log(unrate.ts))
pp.test(detrend.unrate) #due to pp.test unemployment is stationary
kpss.test(detrend.unrate, null =c("Trend") ) #unstationary
ndiffs(detrend.unrate) #0 differences needed
nsdiffs(detrend.unrate) #0 seasonal differences needed
plot(decompose(detrend.unrate))
##Maybe moving average smoothing helps?
trend_unemp = ma(detrend.unrate, order = 4, centre = T)
plot(trend_unemp)
unrate1.ts <- detrend.unrate - trend_unemp
plot(unrate1.ts) ##atleast it looks better ( more stationary)
kpss.test(unrate1.ts) #Still stationary is rejected


var.data <- ts.union(inflation.ts, unrate1.ts)
var_est <- VAR(y = var.data, p = 2)
summary(VAR_est)

var.data1 <- ts.union(inflation.ts, fed.ts)
var_est1 <- VAR(y = var.data1, p = 2, ic = c("AIC"))
summary(VAR_est1)

var.data2 <- ts.union(unrate1.ts, fed.ts)
var_est2 <- VAR(y = var.data2, p = 2,lag.max= 10, ic = c("AIC"))
summary(var_est2)


#4.4  Now, estimate a VAR model for three variables

var.data3 <- ts.union(unrate.ts, fed.ts, inflation.ts)
##Decided to go with VAR(2) based on AIC values
var_est3 <- VAR (y = var.data3, p = 2, ic = c("AIC"))
summary(var_est3)
#Granger- and instantaneous causality tests
causality(var_est3,cause = "unrate1.ts")
?causality
##Granger causality H0: unrate.ts do not Granger-cause fed.ts inflation.t (rejected)
##Instant: H0: No instantaneous causality between: unrate.ts and fed.ts inflation.ts (rejected)
causality(var_est3, cause = "fed.ts")
##Granger causality H0: fed.ts do not Granger-cause unrate.ts inflation.ts (can't reject)
##instant: H0: No instantaneous causality between: fed.ts and unrate.ts inflation.ts (rejected)
causality(var_est3, cause = "inflation.ts")
##Granger causality H0: inflation.ts do not Granger-cause unrate.ts fed.ts (rejected)
##H0: No instantaneous causality between: inflation.ts and unrate.ts fed.ts (can't reject)

#impulse responses with 95% confidence intervals. Impact is inflicted to one variable and response
#is for all variables
?irf
x <- irf(var_est3, impulse ="inflation.ts")
plot(x)
print(x)
##Impulse inflicted to inflation causes 1 day lagged response to fed.ts and minor linearly
## positive effect on unrate.ts. The impulse's effect decays slowly on inflation
y <- irf(var_est3, impulse ="fed.ts")
plot(y)
print(y)
##Impulse inflicted to fed.ts causes minor positive effect on inflation, although it's fairly
##persistent over 10 days. To unrate.ts, impact on fed.ts causes minor positive impact
z <- irf(var_est3, impulse ="unrate.ts")
plot(z)
print(z)
##Impulse to unrate.ts causes negative (convex) impact on fed.ts. Affect on 
##inflation is also negative.

#variance decomposition
x1 <- fevd(var_est3, n.ahead=30)
plot(x1)


##Interpretation on assingment document. Too long to write it here. 
