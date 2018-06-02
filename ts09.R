library(tsa)

# SARIMA season ARIMA model
# passengers at one airport
data("airpass")
plot(airpass)
# have trend, sesonality

# difference opperation to remove seasonality for 12 months
plot(diff(log(airpass),12))

# two difference opperations to remove seasonality and the trend
plot(diff(diff(log(airpass),12)))

# acf and pacf
acf(diff(diff(log(airpass),12)))
pacf(diff(diff(log(airpass),12)))

# to longer period
acf(diff(diff(log(airpass),12)),48)
# some are out of confidence bounds due to second sin
# this transofrmation is not very good due to not perfect model for the seasonality with 12 months
# Box- Jenkins : the seasonality depends on the year
# so add another dependance
# acf => MA(1) or MA(3)

# give the above thoughts try the model:
model1=arima(airpass,order = c(0,1,1),seasonal = list(order=c(0,1,1), period=12))
acf(rstandard(model1))
# the model seems fine

model2=arima(log(airpass),order = c(0,1,1),seasonal = list(order=c(0,1,1), period=12))
acf(rstandard(model2))
# the model seems fine; better than the above one

# Box test
Box.test(rstandard(model2), lag=20)
# H0: correlation for lag 20 is 0
# large p value 0.7128 => we cannot reject the null hypothesis

# prediction
a=plot(model2, n.ahead = 24)
# seem good; not too big confidence bounds
# another check is the standard error of the predictions

# p+Ps=1+12=13; q+Qs=1+12=13; ARMA(13,13)
model3=arima(diff(diff(log(airpass)),12),order = c(13,0,13))
acf(rstandard(model3))
# very good autocorrelation function 

# ARMA(0,13)
model4=arima(diff(diff(log(airpass)),12),order = c(0,0,13))
acf(rstandard(model4))
# very good autocorrelation function 
# the supposition for ARMA process for the months reduces the number of parameters?



