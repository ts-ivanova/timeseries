# week7

# simulate MA(1) process
theta=0.9
sim1=arima.sim(model=list(ma=theta),n=1000)
plot(sim1, type='o')
acf(sim1)
y=acf(sim1)
y

# theoretical correlation r
r=theta/(1+theta^2)
# r is 0.4972376 very close to the sample correlaton with lag 1 y[1]=0.494

# sample value for theta
theta1=(1+sqrt(1-4*((0.494)^2)))/(2*0.494)
theta2=(1-sqrt(1-4*((0.494)^2)))/(2*0.494)
# theta should not be > 1 and most probably is not negative

# estimating the param
# the function gives the estimator, standard error and the intercept
# standard error should not be more than estimator/10
# if it is => bad estimator
model1=arima(sim1, order=c(0,0,1))
model1

# changing the method of estimation
?arima
model1=arima(sim1, order=c(0,0,1), method="ML")
model1

# loading Canadian rabbit data
library(TSA)
data(hare)
hare
plot(hare, type='o')
?hare


plot(hare, x=zlag(hare, 2))
# correlation for lag 1 can be seen at the plot

# acf and pacf
acf(hare)
pacf(hare)

# getting rid of the dependencies
acf(log(hare))
pacf(log(hare))

  # Box Cox transformation of the data
BoxCox.ar(hare)
y=BoxCox.ar(hare)
y
# mle=0.5 (for lambda)

# try it like this
acf((sqrt(hare)-1)/0.5)
pacf((sqrt(hare)-1)/0.5)

# and compare to the originals
acf(hare)
pacf(hare)
# not significant  improvement of the acf and pacf functions
# the pacf is good both times

# pacf afther lag 2 or 3 gets into the confidence interval
# therefore we will trq to fit AR(2) or AR(3) to the transformed data
# AR(2)=ARIMA(2,0,0)
model2=arima(sqrt(hare), order=c(2,0,0))
model2
# big standard error for the second coef
# therefore phi2 is probably 0
# but it is contraintuitive to the pacf function before

acf(residuals(model2))
pacf(residuals(model2))
# residuals are white noise
# good results => good model (even though the large standard error for phi2)


# AR(1)=ARIMA(1,0,0)
model3=arima(sqrt(hare), order=c(1,0,0))
model3
# big standard error

acf(residuals(model3))
pacf(residuals(model3))
# correlations out of the confidence interval => bad model

# !!!!!!!!!! the correlations between the residuals are the most informative 


# AR(3)=ARIMA(3,0,0)
model4=arima(sqrt(hare), order=c(3,0,0))
model4
# big standard errors

acf(residuals(model4))
pacf(residuals(model4))
# correlations in the confidence interval => good model


# AR(3)=ARIMA(3,0,0)
# but now we fix the second coefficient to 0
# we fit the other coefficients with regards to the fixed one
model5=arima(sqrt(hare), order=c(3,0,0), fixe=c(NA,0,NA,NA))
model5
# small standard errors
# AIC is the smallest of the above

acf(residuals(model5))
pacf(residuals(model5))
# correlations in the confidence interval => good model
# all of criterias give this model to be the best one :))
# further test for the residuals (from the previous seminars) may be performed

# now calcualte the forecest for 5 time steps
plot(model5,n.ahead = 5)

