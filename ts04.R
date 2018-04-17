#31.3.2018
#review from last time
library(TSA)
data("rwalk")
plot(rwalk, type="o")
model1=lm(rwalk~time(rwalk))
summary(model1)

#load new data (temperatures)
data("tempdub")
plot(tempdub, type='o')

#indicator for the seasonality
month=season(tempdub)
#this is a factor at the regression - quality variable
color=factor(c('r','r','g','b'), levels=c('r','g','b'))
table(color)

#estimation of data with regression without intercept
model2=lm(tempdub~month-1)
model2
summary(model2)

#estimation of data with regression with intercept
model3=lm(tempdub~month)
model3
summary(model3)
#we have multicolinearity at the predictors when we use the intercept
#therefore we should not use it

plot(residuals(model2))
plot(rstandard(model2))

plot(acf(rstandard(model2)))
#this shows us that we do not have correlation between the residuals

#fit time series model
plot(ts(fitted(model2),freq=12,start=c(1964,1)),type="l")
points(tempdub)

#another model for seasonality (cos trend); see lecture notes week 5
h=harmonic(tempdub,1)
h
model4=lm(tempdub~h)
model4
summary(model4)

plot(rstandard(model4))
plot(acf(rstandard(model4)))
#some exceedances for the confidence bounds of the autocorrelations
#so model2 is better

plot(ts(fitted(model4),freq=12,start=c(1964,1)),type="l")
points(tempdub)
#a bit less points off the model

