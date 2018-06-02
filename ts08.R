# 19.06 (week8)

library(TSA)
data(color)
plot(color, type='o')
acf(color) # looks like MA(8) but we have very few data, so no.
pacf(color) # everything in conf int - ok; outside - 1st
# therefore the model could be AR(1)
eacf(color, ar.max=2, ma.max=2) # A suitable model would be either (0,1) or (1,0)
# (Nearest top left corner with element !=x.)
data(hare)
eacf(hare, ar.max=2, ma.max=2) # rabbits

model1=arima(color, order=c(1,0,0)) # we check how's AR(1) for a possible model.
model1 # aic = 216.15 
plot(rstandard(model1), type='p') # it should be around 0 and not correlated
# rstandard is Standardized Residual
acf(rstandard(model1)) # all are inside, therefore OK
pacf(rstandard(model1)) # all are inside, therefore OK
# arima() estimations; arima.sim() - simulations

model2=arima(color, order=c(0,0,1)) # MA(1)
model2 # aic = 219.88
acf(rstandard(model2)) 
pacf(rstandard(model2)) 

# We should probably choose model1 because of smaller aic.

model3=arima(color, order=c(10,0,0)) # kind of overfitting
model3
acf(rstandard(model3))
pacf(rstandard(model3))


# We continue with the best model of our choice, model1
qqnorm(rstandard(model1))
qqline(rstandard(model1))
# close to normal distribution
# not only graphical test, but analytical, too: shapiro.test
shapiro.test(rstandard(model1)) # p-value=0.6 -> Normal distribution (H_0)
runs(rstandard(model1)) # p-value=0.76 -> The random variables are i.i.d
Box.test(rstandard(model1), lag=5, type=c('Ljung-Box'), fitdf=0)
tsdiag(model1)

plot(model1, n.ahead = 5) # 5 predictions, but not good. seems better than the following:
plot(model2, n.ahead = 5) 


# Another example:
data("oil.price")
plot(oil.price)
# there is trend, obviously, so no stationarity.
acf(oil.price) # graph of a TS with trend.
pacf(oil.price) # can be deceiving and make us think the model is AR(1). But the example is not stationary. Better examine acf (for stationarity)!

plot(log(oil.price))
plot(diff(log(oil.price)))
acf(diff(log(oil.price))) # could be MA(1)
# by difflog we managed to make it look stationary.
pacf(diff(log(oil.price))) # AR(1)
# so either AR(1) or MA(1). But if it was ARMA, acf and pacf would not give us the order.
eacf(diff(log(oil.price)))
# we conclude it is most likely MA(1)

model1=arima((diff(log(oil.price))), order=c(0,0,1))
model1
acf(rstandard(model1))
pacf(rstandard(model1))
tsdiag(model1)
plot(model1, n.ahead = 5) # not good.

model2=arima(log(oil.price), order=c(0,1,1))
plot(model2, n.ahead = 5)

adf.test(oil.price) # H_0: z=1; H_1: z_i != 1
model1

test=armasubsets(y=oil.price, nar=0, nma=1)
plot(test)
