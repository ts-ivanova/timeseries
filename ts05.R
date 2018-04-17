#review from last time
library(TSA)
data("tempdub")
plot(tempdub, type="o")
month=season(tempdub)
#Y=month+epsilon
#estimatior of Y is the mean
#residual is Y-estimator of Y
model1=lm(tempdub~month-1)
summary(model1)
fitted(model1)
residuals(model1)
rstandard(model1)
#removing dependence at the standderdized residuals
rstudent(model1)

plot(residuals(model1),type="o")
plot(rstandard(model1),type="o")
plot(rstudent(model1),type="o")

#checking if the model is good with the acf function
acf(residuals(model1))
acf(rstandard(model1))

#normally dist residuals check
hist(rstudent(model1))
qqnorm(rstudent(model1))
qqline(rstudent(model1), col="red")

qqnorm(runif(100))
qqline(runif(100), col="red")
qqnorm(rcauchy(100))
qqline(rcauchy(100), col="red")

#tests for normality of residuals

#Shapirov test
shapiro.test(rstudent(model1))
#Large p-value => we cannot reject the null hypothesis of normal standardized residuals

#Kolmogorov-Smirnov test
ks.test(rstudent(model1), "pnorm")
#Large p-value => we cannot reject the null hypothesis of normal standardized residuals

#Box test
Box.test(rstudent(model1), lag=10)
#Large p-value => we cannot reject the null hypothesis of large sum of corelations of the observations with lag 10

#Runs test for iid observations
runs(rstudent(model1))
#Large p-value => we cannot reject the null hypothesis of iid observations


