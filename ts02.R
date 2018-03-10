#Time series
#10.3.2018

data=read.table("/home/tsveti/Documents/1_PS/2TS/R/data.txt", header=TRUE)
View(data)
data1ts=ts(data) #converts to time series
plot(data1ts)
data1ts=ts(data, start=2005, frequency=2)
plot(data1ts)
data1ts=ts(data, start=c(2005,3), frequency=12)
plot(data1ts)
data1ts
attach(data) #splits the data and allows access to X (becomes 1D)
?attach
z=ts(X)
plot(z)


#Random walk
#X_t = sum from k=1 to t of (Z_k), where Zt is WhiteNoise(0, sigma^2) from lectures 24.3.18
#Generate Z1 to Zn
#X1=Z1, X2=Z1+Z2, ..., Xt=Z_{t-1} + Zt

sigma2=0.25
R=rnorm(1000, 0, sigma2)
x=cumsum(R)
xts=ts(x)
plot(xts, col="purple")
plot(diff(xts)) #difference operation, back to initial R=rnorm
plot(R)
acf(diff(xts))
pacf(diff(xts))
acf(xts)
acf(diff(xts))
pacf(diff(xts)) #xts is random walk

#xts has a linear trendline, therefore: not stationary; it is ARIMA(p,d,q)
#d=0 => ARMA(p,q) ((infininite) linear combination of white noise)
#WN=ARMA(0,0)
#X_t is ARIMA(0,1,0)
