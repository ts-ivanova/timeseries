
library(TSA)

data(ma1.2.s)

plot(ma1.2.s, type='o') # moving average process MA(1),Theta=0.9, X_t = Z_t + Theta Z_{t-1}

y = acf(ma1.2.s) # rhohat = 0.424

rho_1 = 0.9/1.81 # theoretical; rho_2 = 0, rho_2hat =  0.113

acf(ma1.2.s, ci.type='ma') # confidence interval type is moving averge type

plot(y=ma1.2.s, x=zlag(ma1.2.s, 2))

sim = arima.sim(model=list(ma=c(0.9)), n=1000)

plot(sim, type='o') # generated similar model

acf(sim)

sim1=arima.sim(model=list(ar=0.9), n=1000) # could try -0.9

acf(sim1)

plot(y=sim1, x=zlag(sim1))

sim3=arima.sim(model=list(ma=c(-1, 0.6)), n=1000) # sim ma process

acf(sim3)

plot(y=sim3, x=zlag(sim3))

# correlations ...

sim4=arima.sim(model=list(ar=c(0.5, 0.25)), n=1000) #  ar(2)

acf(sim4)

plot(y=sim4, x=zlag(sim4))

# |z1,2|<1 => causal process

sim5=arima.sim(model=list(ar=c(1.5, -0.75)), n=1000)

acf(sim5)

plot(y=sim4, x=zlag(sim4))

# this has complex numbers as solutions of the quadratic equation, |z_complex|>1

# AR(2) is a stationary process (and has no seasonlity). But the correlation shows some form of seasonality, but it decays exponentionally, therefore no seasonality.
