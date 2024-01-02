#homework5
#4.3
#partb
polyroot(c(1,-0.5))
#model 2
polyroot(c(1,-1, 0.5))
polyroot(c(1,-1))
#partc
library(astsa)
par(mfrow=c(1,2))
plot(ARMAtoMA(ar = c(0.5), lag.max = 50), main = "Model 1 MA coefficients", ylab = "MA coefficients")
plot(ARMAtoAR(ar = c(0.5), lag.max = 50), main = "Model 1 AR coefficients", ylab = "AR coefficients")
plot(ARMAtoMA(ar = c(1, -0.5), ma = -1, lag.max = 50),      main = "Model 2 MA coefficients", ylab = "MA coefficients")
plot(ARMAtoAR(ar = c(1, -0.5), ma = -1, lag.max = 50),      main = "Model 2 AR coefficients", ylab = "AR coefficients")

#4.4
#part a
par(mfrow=c(3,2))
#arma(1,1)
acf11=ARMAacf(ar=0.6,ma=0.9 ,24)[-1]
pacf11=ARMAacf(ar=0.6,ma=0.9, 24, pacf=TRUE)
tsplot(acf11, type="h", xlab = "lag", ylim=c(-0.8,1), main = "acf arma(1,1)")
abline(h=0)
tsplot(pacf11, type="h", xlab = "lag", ylim=c(-0.8,1), main = "pacf arma(1,1)")
abline(h=0)
#arma(1,0)
acf10=ARMAacf(ar=0.6,ma=0 ,24)[-1]
pacf10=ARMAacf(ar=0.6,ma=0, 24, pacf=TRUE)
tsplot(acf10, type="h", xlab = "lag", ylim=c(-0.8,1), main = "acf arma(1,0)")
abline(h=0)
tsplot(pacf10, type="h", xlab = "lag", ylim=c(-0.8,1), main = "pacf arma(1,0)")
abline(h=0)
#arma(0,1)
acf01=ARMAacf(ar=0,ma=0.9 ,24)[-1]
pacf01=ARMAacf(ar=0,ma=0.9, 24, pacf=TRUE)
tsplot(acf01, type="h", xlab = "lag", ylim=c(-0.8,1), main = "acf arma(0,1)")
abline(h=0)
tsplot(pacf01, type="h", xlab = "lag", ylim=c(-0.8,1), main = "pacf arma(0,1)")
abline(h=0)
#partb
set.seed(100)
n=100
arma11 = arima.sim(model=list(order=c(1,0,1), ar=0.6, ma=0.9), n=n)
arma10 = arima.sim(model=list(order=c(1,0,0), ar=0.6), n=n)
arma01 = arima.sim(model=list(order=c(0,0,1), ma=0.9), n=n)
sampleacf11 = acf(arma11, lag.max = 24, plot = FALSE)$acf[-1]
sampleacf10 = acf(arma10, lag.max = 24, plot = FALSE)$acf[-1]
sampleacf01 = acf(arma01, lag.max = 24, plot = FALSE)$acf[-1]
samplepacf11 = pacf(arma11, lag.max = 24, plot = FALSE)$acf
samplepacf10 = pacf(arma10, lag.max = 24, plot = FALSE)$acf
samplepacf01 = pacf(arma01, lag.max = 24, plot = FALSE)$acf
par(mfrow=c(3,2))
tsplot(sampleacf11, type="h", xlab = "lag", ylim=c(-0.8,1), main = "sample acf arma(1,1)")
abline(h=0)
tsplot(samplepacf11, type="h", xlab = "lag", ylim=c(-0.8,1), main = "sample pacf arma(1,1)")
abline(h=0)
tsplot(sampleacf10, type="h", xlab = "lag", ylim=c(-0.8,1), main = "sample acf arma(1,0)")
abline(h=0)
tsplot(samplepacf10, type="h", xlab = "lag", ylim=c(-0.8,1), main = "sample pacf arma(1,0)")
abline(h=0)
tsplot(sampleacf01, type="h", xlab = "lag", ylim=c(-0.8,1), main = "sample acf arma(0,1)")
abline(h=0)
tsplot(samplepacf01, type="h", xlab = "lag", ylim=c(-0.8,1), main = "sample pacf arma(0,1)")
abline(h=0)

#partc (just copy part b)
set.seed(100)
n=500
arma11 = arima.sim(model=list(order=c(1,0,1), ar=0.6, ma=0.9), n=n)
arma10 = arima.sim(model=list(order=c(1,0,0), ar=0.6), n=n)
arma01 = arima.sim(model=list(order=c(0,0,1), ma=0.9), n=n)
sampleacf11 = acf(arma11, lag.max = 24, plot = FALSE)$acf[-1]
sampleacf10 = acf(arma10, lag.max = 24, plot = FALSE)$acf[-1]
sampleacf01 = acf(arma01, lag.max = 24, plot = FALSE)$acf[-1]
samplepacf11 = pacf(arma11, lag.max = 24, plot = FALSE)$acf
samplepacf10 = pacf(arma10, lag.max = 24, plot = FALSE)$acf
samplepacf01 = pacf(arma01, lag.max = 24, plot = FALSE)$acf
par(mfrow=c(3,2))
tsplot(sampleacf11, type="h", xlab = "lag", ylim=c(-0.8,1), main = "sample acf arma(1,1)")
abline(h=0)
tsplot(samplepacf11, type="h", xlab = "lag", ylim=c(-0.8,1), main = "sample pacf arma(1,1)")
abline(h=0)
tsplot(sampleacf10, type="h", xlab = "lag", ylim=c(-0.8,1), main = "sample acf arma(1,0)")
abline(h=0)
tsplot(samplepacf10, type="h", xlab = "lag", ylim=c(-0.8,1), main = "sample pacf arma(1,0)")
abline(h=0)
tsplot(sampleacf01, type="h", xlab = "lag", ylim=c(-0.8,1), main = "sample acf arma(0,1)")
abline(h=0)
tsplot(samplepacf01, type="h", xlab = "lag", ylim=c(-0.8,1), main = "sample pacf arma(0,1)")
abline(h=0)

#4.5
#part a
par(mfrow=c(1,2))
x = diff(cmort)
plot(cmort, main="cmort", col=3)
plot(x, main=" diff(cmort)", col=2)

#partb
par(mfrow=c(1,2))
acf(x, lag.max = 24, main="acf for diff(cmort)")
pacf(x, lag.max = 24, main="pacf for diff(cmort)")

#partc
par(mfrow=c(1,1))
sarima(x, p=1, d=0, q=0, no.constant = TRUE)

#parte
sarima.for(x, n.ahead = 4, p=1, d=0, q=0, no.constant = TRUE)

#partf
x[length(x)]

#partg
cmort[length(cmort)]

#5.11
#plot series
par(mfrow=c(1,1))
tsplot(birth)

#plot acf and pacf
acf2(birth)

#after seasonal and 1st difference
birth_transformed = diff(diff(birth,12), 1)
tsplot(birth_transformed)
acf2(birth_transformed)

#sarima with SMA(1)
model1 = sarima(birth, p=0, d=1, q=0, P=0, D=1, Q=1, S=12)
acf2(residuals(model1$fit))

model2 = sarima(birth, p=1, d=1, q=1, P=0, D=1, Q=1, S=12)
acf2(residuals(model2$fit))

#forecast
sarima.for(birth, p=1, d=1, q=1, P=0, D=1, Q=1, S=12, n.ahead = 12)

#5.13
#parta
par(mfrow=c(1,1))
tsplot(sales)
acf2(sales)

par(mfrow=c(1,1))
tsplot(diff(sales))
acf2(diff(sales))

model3 = sarima(sales, p=1, d=1, q=1, P=0, D=0, Q=0)

#partb
ccf.data = ccf(diff(lead), diff(sales), main="diff(lead) v/s diff(sales)", plot = FALSE)
plot(ccf.data, main="diff(lead) v/s diff(sales)")
lag2.plot(diff(lead), diff(sales), max.lag = 8)

#partc
reg_sales = lm(diff(sales) ~ lag(diff(lead),3))
acf2(residuals(reg_sales))

reg_sales_sarima = sarima(diff(sales), p=1, d=0, q=1, xreg = diff(lead))
acf2(residuals(reg_sales_sarima$fit))

#problem2
#parta
set.seed(666)
n=100
xt1 = ts(arima.sim(list(order=c(1,0,0), ar=0.5), n=n))
xt2 = ts(arima.sim(list(order=c(12,0,0), ar=c(rep(0,11),0.9)), n=n))

wt = rnorm(n)
yt1 = xt1 + cumsum(wt)
yt2 = xt2 + 5 * cumsum(wt)
zt = 5*yt1 - yt2

par(mfrow=c(3,1))
plot(yt1, col=2, main="yt1")
plot(yt2, col=3, main="yt2")
plot(zt, col=4, main="zt")
