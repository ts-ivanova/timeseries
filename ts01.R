# Time series
# 24.2.2018

install.packages("TSA")



library("TSA")
data(tempdub)
plot(tempdub, type = "o")
acf(tempdub,50)
pacf(tempdub,50)
plot(diff(tempdub,lag=12), type='o')
acf(diff(tempdub,lag=12), 50) # not white noise
data(electricity)
plot(electricity,type="o")
acf(electricity, 100)
plot(diff((electricity)), type="o")
plot(diff(log(electricity)), type="o")
acf(diff(log(electricity)), 100)
acf(diff(diff(log(electricity)),12), 100) # removes both trend and seasonality

data(CREF)
plot(CREF) # volatility clustering
# not ARMA

plot(diff(log(CREF)), type="o") # ln(Xt/Xt-1) prices to returns procedure
acf(diff(log(CREF)))
pacf(diff(log(CREF))) # looks 'more stationary'
# no correlation, therefore white noise
# but we have clusters
acf(diff(log(CREF))^2) # the square is correlated
# therefore it is not just white noise.

qqnorm(diff(log(CREF)))
qqline(diff(log(CREF)), col=123) # not normal, heavier tails than the normal distr.

# For financial series - GARCH(p,q) models are used
# GARCH^2 looks like ARMA process
