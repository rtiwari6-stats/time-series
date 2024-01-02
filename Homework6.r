#homework6
library(astsa)
#5.2
tsplot(gdp, col=4)
tsplot(diff(log(gdp)), col=4, ylab = "GDP growth rate")
abline(h=mean(diff(log(gdp))), col=6)
acf2(diff(log(gdp)), main = "diff(log(gdp))")

sarima(diff(log(gdp)), p=1, d=0, q=0)
sarima(diff(log(gdp)), p=1, d=0, q=2)


#5.3
tsplot(oil, col=4)
tsplot(diff(log(oil)), col=4)

acf2(diff(log(oil)), main = "diff(log(oil))")
sarima(diff(log(oil)),p=1, d=0, q=1)
sarima(diff(log(oil)),p=1, d=0, q=3)
sarima(diff(log(oil)),p=3, d=0, q=3)


#5.6
tsplot(so2, col=4)
tsplot(diff(so2), col=4)
acf2(diff((so2)), main="diff(so2)")
sarima_111 = sarima(so2, p=1, d=1, q=1)
acf2(residuals(sarima_111$fit))
sarima(so2, p=3, d=1, q=1)
forecast = sarima.for(so2, p=3, d=1, q=1, n.ahead = 4)
as.numeric(forecast$pred) - 1.96 * as.numeric(forecast$se)
as.numeric(forecast$pred) + 1.96 * as.numeric(forecast$se)

#5.7
tsplot(AirPassengers, col=4)
tsplot(diff(diff(AirPassengers, 12)), col=4)
acf2(diff(diff(AirPassengers, 12)), main = "diff(diff(AirPassengers, 12))")
sarima(AirPassengers, p=1, d=1, q=0, P=0, D=1, Q=0, S=12) #can be made better!
#Sarima(0,1,1)(0,1,1)[12] -- auto.sarima
sarima(log(AirPassengers), p=0, d=1, q=1, P=0, D=1, Q=1, S=12) #seems similar to what we have

#5.15
trend = time(cmort)
temp = tempr - mean(tempr)
temp2 = temp^2
originalfit =  lm(cmort ~ trend + temp + temp2 + part)
partlag4 = as.numeric(c(part[-(1:4)], rep(NA,4)))
fit = lm(cmort ~ trend + temp + temp2 + part + partlag4)
summary(fit)
plot(fit$residuals, type = "l")
acf2(fit$residuals) #AR(2)
cdata = na.omit(data.frame(trend, temp, temp2, part, partlag4))
sarimafit1 = sarima(cmort[-(1:4)], p=2, d=0, q=0, xreg=cdata)
acf2(residuals(sarimafit1$fit), max.lag = 60)
sarimafit2 = sarima(cmort[-(1:4)], p=2, d=0, q=0, P=2, D=0, Q=0, S=15, xreg=cdata[-(1:4),])

num=length(cmort)
AIC(originalfit)/num - log(2*pi)
AIC(fit)/num - log(2*pi)
AIC(sarimafit2$fit)/num - log(2*pi)

BIC(originalfit)/num - log(2*pi)
BIC(fit)/num - log(2*pi)
BIC(sarimafit2$fit)/num - log(2*pi)

#8.3
par(mfrow=c(1,1))
resid = residuals(sarima(diff(log(oil)),p=3, d=0, q=3)$fit)
acf2(resid^2, main = "Squared Residuals of sarima(diff(log(oil)),p=3, d=0, q=3)")
library(fGarch)
fit1 = garchFit(~ arma(3,3) + garch(1,1), data=diff(log(oil)))
summary(fit1)
plot(fit1, which=3)

fit2 = garchFit(~ arma(3,3) + garch(3,3), data=diff(log(oil)))
summary(fit2)
plot(fit2, which=3)

fit3 = garchFit(~ arma(3,3) + garch(2,2), data=diff(log(oil)))
summary(fit3)
plot(fit3, which=3)

fit4 = garchFit(~ arma(3,3) + garch(3,2), data=diff(log(oil)))
summary(fit4)
plot(fit4, which=3)

#problem2
#partc
library(prophet)
library(lubridate)
textdates = my(paste0(1:12, "-", substr(time(birth), 1, 4)))
#Dataframe containing the history. Must have columns ds (date type) and y, the time series. 
#If growth is logistic, then df must also have a column cap that specifies the capacity at each ds. 
#If not provided, then the model object will be instantiated but not fit; use fit.prophet(m, df) to fit the model.
birth_df = data.frame(ds=textdates, y=birth)
fit3 = prophet(birth_df)
summary(fit3)
future = make_future_dataframe(fit3, periods = 12, freq = "month")
forecast = predict(fit3, future)
tail(forecast[c('ds', 'yhat', 
                'yhat_lower', 'yhat_upper')],13)
plot(fit3, forecast)
