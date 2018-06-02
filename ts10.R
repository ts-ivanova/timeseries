# week10

library(TSA)
data(CREF) # GARCH
transf = diff (log(CREF))*100

acf(transf)
Box.test(transf, 10) # H_0: WN; H_1: not WN
# seems like WN

acf(transf^2)
acf(abs(transf))
Box.test(transf^2, 2)
M=McLeod.Li.test(y=transf) # box test for data^2. H_0 heteroskedasticity VS H_1 no heteroskedasticity.
M
qqnorm(transf); qqline(transf, col=123)
qqnorm(rcauchy(100)); qqline(rcauchy(100)) # heavy tails

skewness(transf) # asymmetry (3 moment): E((X-EX)/sqrt(DX))^3
kurtosis(transf) # excess (4 moment)

# normality test: H_0 normal distr VS H1: not normal distr
jarque.bera.test(transf)
shapiro.test(transf)
ks.test(transf, y=rnorm)

pacf(transf)

# ARMA(0,0) | GARCH


acf(transf^2)
pacf((transf)^2)
eacf(transf^2) # could be GARCH(1,1)


model1=garch(x=transf, order=c(1,1))
summary(model1)

acf(residuals(model1))
ts(transf)

