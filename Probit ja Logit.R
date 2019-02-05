
## For learning diary and intepretation
# For question or explaining what I am trying to do.



#Activating the libraries
install.packages("normtest")
library ("normtest")
library("xlsx")

#marginal effect
install.packages("mfx") 
library(mfx)

#wald test
install.packages("aod")
library(aod)


#data splitting
install.packages("caTools")
library(caTools)

#Epi for logit and probit statistics
install.packages("Epi")
library("Epi")

#For robust SE
install.packages("estimatr")
library("estimatr")
#Again for robust SE
install.packages("lmtest")
library(lmtest)
install.packages("sandwich")
library (sandwich)

#reading the data
macro <-read.xlsx("macro.xlsx",1)
msc <-read.xlsx("msc.xlsx",1)

summary(macro)
summary(msc)

#OLS to estimate the model where dependent variable is ermsoft and
#explanatory variables are ersandp dprod dcredit dinflation dmoney dspread rterm.

fit <- lm(macro$ermsoft~macro$ersandp+macro$dprod+ #ols regression
	macro$dcredit+macro$dinflation+macro$dmoney+macro$dspread+macro$rterm)
resid<-(fit$residuals^2) #saving residuals to resid
n <- length(resid)
summary(fit)

#1.1 Test for heteroskedasticity of error terms. (If homoscedasticity assumption does not seem
#to hold, use Huber-White standard error estimates in subsequent estimations.)

#Graphically:
plot(fit,3)

##This plot tells us if the residuals are spread equally along the ranges of predictors.
##Horizontal straight line would indicate homoscesdicity, but this is not the case in our data.


#1.2 Test for autocorrelation of residuals
#Graphically:
head(macro)
plot(Y=resid, x=macro$date)
#Statistically
fit_residuals <- lm(resid[-n] ~resid[-1]) #testing autocorrelation of residuals by regressing residuals with lagged residual values
summary(fit_residuals)
confint(fit_residuals)

##The autocorrelation is not statistically significant as probability of resid[-1] coefficient being zero is ~13% 

#1.3 Test for the normality of error terms.

#visualization of residuals using QQ-plot and histogram
qqnorm(resid(fit))
hist(resid, main="Histogram for residuals")
#Jargue-Bera test for residual normality, nrepl = number of monte carlo simulations
jb.norm.test(resid, nrepl=2000) 

##Error terms distribution shows quite a heavy left tail, and thus is not normally distributed.
##This can be seen graphically from qq-plot and histogram + p-value of Jargue-Bera is not statistically
##significant so we reject the null hypothesis that data residuals are normally distributed.

#1.4 Check for the possible multicollinearity.
macro1<-na.omit(macro)
head(macro1)
cor(macro1[,5:11])

##In a ideal case the independent variables would have zero correlation between them, so ceteris paribus
##interperation is valid. This is not a case in our data, but it's nothing that alarming as correlations
##are low.

#1.5 Test for the linearity of the model.
plot(fit, 1)

##Ideally residual plot would not show any pattern, meaning that the red line would be horizontal line
##close to 0, this is not the case now.

#1.6 Test for the parameter stability using some of the tests presented at the lecture.
#Graphical overview
plot(y=macro$ersandp,x=macro$date, type ="l", main="excess returns of markets over time", xlab= "Time", ylab="ersandp")
#subsetting data into 2 separate variables for Chow test

variables <- macro[c(5,6,7,8,9,10,11,19)]
summary(variables)
head(macro)
macro1 <- variables[1:163,]
macro2 <- variables[164:326,] 


fit <- lm(ermsoft~ersandp+dprod+ 
	dcredit+dinflation+dmoney+dspread+rterm,data=macro)

#sub regression 1
fit1 <- lm(ermsoft~ersandp+dprod+dcredit+dinflation+dmoney+dspread+rterm, data=macro1)

#sub regression 2
fit2 <- lm(ermsoft~ersandp+dprod+dcredit+dinflation+dmoney+dspread+rterm, data=macro2)

#Storing the residuals

RSS_r <- sum(fit$resid^2)
RSS_r1 <- sum(fit1$resid^2)
RSS_r2 <- sum(fit2$resid^2)


K = fit$rank

#computing the chow test
numerator = (RSS_r - RSS_r1 + RSS_r2) *(nrow(macro) - 2*K)
denominator = (SSR_r1 + SSR_r2) * K 
chow=numerator/denominator
chow

chow_p = 1-pf(chow, K, (nrow(macro) - 2*K))
chow_p

##The parameter stability test was something new for me. It's kind of intuitive when you think of it now
##and it's great that we are practicing to use it. I ran into some problems during this section
##as I could not get any of the premade functions working. With help of lecture notes and internet
##got it solved eventually. 

#2.1 Estimate a linear probability model of Fail on a constant, Age, English, Female, Work
experience, A-Grade, Below-B-Grade, PG-Grade and the year dummies. Use robust standard
errors.

str(msc$Fail)

#Creating training data for models (for evaluating purposes) 60% for training, rest for testing
sample = sample.split(msc$Fail, SplitRatio = .60)
train = subset(msc, sample == TRUE)
test  = subset(msc, sample == FALSE)

lm_MSC <- lm_robust(Fail~ Age + English + Female + Work.Experience + Agrade + BelowBGrade + PG.Degree +
	Year2004 + Year2005 + Year2006 + Year2007, data = train, se_type = "stata")

summary(fit3)



##lm_robust for robust standard errors. It uses HC1(?) variation which is presumably stata default.
##No idea what the function does, I need to check that later. On top of that seems that package
##doesn't include logit/probit models
##Now I got it! Robust standard errors are important because of hypothesis tests and other
##diagnostics. When we are dealing with heteroskedastic data SE

#2.2 Estimate a probit model using the same dependent and independent variables as in i). Use
robust standard errors.

probit_MSC <- glm(Fail~ Age + English + Female + Work.Experience + Agrade + BelowBGrade + PG.Degree +
	Year2004 + Year2005 + Year2006 + Year2007, 
                  family = binomial(link = "probit"), 
                  data = train)

coeftest(probit_MSC, vcov. = vcovHC, type = "HC1")
ci.lin(probit_MSC, Exp=TRUE)

#2.3 Estimate a logit model using the same dependent and independent variables as in i). Use
robust standard errors.

logit_MSC <- glm(Fail~ Age + English + Female + Work.Experience + Agrade + BelowBGrade + PG.Degree +
	Year2004 + Year2005 + Year2006 + Year2007, 
                  family = binomial(link = "logit"), 
                  data = train)

coeftest(failprobit, vcov. = vcovHC, type = "HC1")
##It's suprising to see that only Work.Experience, Agrade, Year2006 and Year2005 are statistically
##significant with 0.05 confidence interval

#2.4 Interpret the above three models, compare the performance of them, and decide which is
the best. Check on the adequacy of probit and logit models e.g. computing the fractions fails
correctly predicted.

##This was something that I was not able to solve. Spent good 6 hours working with different packages
##and reading syntax. What I tried to do was to train linear, probit and logit models with
##training data, and then use the trained models to predict test data. Then my idea was to collect
##results in single matrix per regression to see how much each got right and wrong. 

#Linear Model
predict(lm_MSC, newdata=test, type="prob")
pred = predict(lm_MSC, newdata=test)
accuracy <- table(pred, test[,"Fail"])
sum(diag(accuracy))/sum(accuracy)

#Probit
predict(probit_MSC, newdata=test, type="response")
pred = predict(probit_MSC, newdata=test, type="response")
summary(pred)
accuracy <- table(pred, test[,"Fail"])
sum(diag(accuracy))/sum(accuracy)



#Logit
pred = predict(logit_MSC, newdata=test, type="response")
accuracy <- table(pred, test[,"Fail"])
accuracy
sum(diag(accuracy))/sum(accuracy)



#2.5  Compute the marginal effects for the above logit and probit models for probability of MSc
failure. Interpret the effects of e.g. age and gender.


logitmfx(Fail ~ Age+Female, data=msc)
##kun ikä lisääntyy yhdellä vuodella, vaikutus epäonnistumiseen kasvaa 0,1% ja vaikutus binääri muuttuja
##Female:n saadessa arvon nollasta ykköseen (muuttuu mieheksi) on -0,05 eli todennäköisyys epäonnistumiseen
##pienenee hieman

probitmfx(Fail ~ Age+Female, data=msc)
#tulokset lähes samoja

#2.6 Re-run the probit and logit regressions above with all the other variables plus the country
dummy variables. Set up the regression so that the UK becomes the reference point against
which the failure rate in other countries is measured (the UK is Country7). Is there evidence
that any countries have significantly higher or lower probabilities of failure than the UK,
holding all other factors in the model constant?

#uk is country 7
logit_MSC_country <- glm(Fail~ Age + English + Female + Work.Experience + Agrade + BelowBGrade + PG.Degree +
	Year2004 + Year2005 + Year2006 + Year2007+
	Country1 + Country2 + Country3 + Country4 +
	Country5 +Country6 + Country7 + Country8 +
	Country9,
                  family = binomial(link = "logit"), 
                  data = msc)


coeftest(logit_MSC_country, vcov. = vcovHC, type = "HC1")
anova
##The coeftest shows that if you are canadian the probability of failing increases by 20%

probit_MSC_country <- glm(Fail~ Age + English + Female + Work.Experience + Agrade + BelowBGrade + PG.Degree +
	Year2004 + Year2005 + Year2006 + Year2007+
	Country1 + Country2 + Country3 + Country4 +
	Country5 +Country6 + Country7 + Country8 +
	Country9,
                  family = binomial(link = "probit"), 
                  data = msc)
coeftest(probit_MSC_country, vcov. = vcovHC, type = "HC1")

##By using probit method being canadian affects to getting failed by increased 9% and it's statistically
##significant while other countries influence is not

#2.7 Suppose that a fellow researcher suggests that there may be a nonlinear relationship
between the probability of failure and the age of the student. Estimate a probit model with
all the same variables as above plus an additional one to test this. Is there indeed any evidence
of such a nonlinear relationship?

msc$age_exp <- msc$Age^2

probit_MSC_exponential <- glm(Fail~ Age + age_exp + English + Female + Work.Experience + Agrade + BelowBGrade + PG.Degree +
	Year2004 + Year2005 + Year2006 + Year2007+
	Country1 + Country2 + Country3 + Country4 +
	Country5 +Country6 + Country7 + Country8 +
	Country9,
                  family = binomial(link = "probit"), 
                  data = msc1)

coeftest(probit_MSC_exponential, vcov. = vcovHC, type = "HC1")
confint(probit_MSC_exponential)

##The age_exp variable is statistically significant but has a very low coefficient value of 0.004.
##I would stick with the linear version of age variable.

