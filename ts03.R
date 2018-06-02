# 24.3.2018
library(TSA)

# load and plot the data
data("rwalk")
plot(rwalk, type="o")
View(rwalk)

# Auto correlation function
acf(diff(rwalk))

# calculating the confidence interval taking into account that
# we have a white noise (WN) process which is ARMA(0,0)
acf(diff(rwalk), ci.type="ma")

# performing Box test to test the hypothesis if the process is WN
Box.test(rwalk, lag=1, type=c('Ljung-Box'))
# very small p-value even for lag 1
# therefore we reject the null hypothesis

# Box test on the diff process
Box.test(diff(rwalk), lag=7, type=c('Ljung-Box'))
# for lag 7 or greater we reject the null hypothesis of white noise
# but we have constructed the process so that it is WN
# therefore probably the rwalk data is not a Random walk

# generate data the same way as the last time (ts02)
# first we set the seed so that we have the same data with the coleagues
set.seed(12345)
Z=rnorm(200)
Y=cumsum(Z)
plot(Y)

# again do the operations as above on the new data
acf(Y)
acf(diff(Y))
# acf(diff(Y)) is at the confidence bounds

Box.test(diff(Y), lag=20, type=c('Ljung-Box'))
# very large p value therefore we cannot reject the hypothesis
# of white noise

# now we are going to perform a linear regression of time
model1=lm(Y~time(Y))
summary(model1)
plot(Y)
abline(model1, col='red')

# we plot the (student) standardized resiguals of the regression
plot(y=rstudent(model1), x=as.vector(time(Y)), type='o')
# the residuals do not seem to be iid(0)
# so the regression does not give a good estimate of our process

# check if the residuals rise with the nuber of the observation
plot(y=rstudent(model1), x=fitted(model1), type='p')
# in this case - no

# check if there is still trend at the residual process
acf(rstudent(model1))
# yes there is in our case

# Linear regression model with 2 predictors
model2=lm(Y~time(Y)+I(time(Y)^2))
plot(Y)
summary(model2)
plot(model2)
# R squared has raiced

# check if there is still trend at the residual process
acf(rstudent(model2))
# yes there is in our case

# again try with rwalk
model4=lm(rwalk~time(rwalk))
acf(rstudent(model4))
# a bit better now but still the diff process is even better :) 
acf(diff(rwalk))




